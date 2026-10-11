;;; $DOOMDIR/ai.el -*- lexical-binding: t; -*-
;; gptel, copilot, aidermacs, claude-code, agent-shell, magit-gptcommit

(use-package! gptel
  :config
  (setq gptel-model 'gemini:gemini-flash-latest
        gptel-backend (gptel-make-gemini "Gemini"
                        :key (getenv "GEMINI_API_KEY")
                        :stream t)
        gptel-default-mode 'org-mode
        gptel-org-branching-context t)

  (gptel-make-anthropic "claude"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY"))

  (gptel-make-ollama "ollama-local"
    :host "localhost:11434"
    :stream t
    :models '(phi4:latest qwen2.5-coder:32b deepseek-r1:32b))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n"))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  ;; :bind (:map copilot-completion-map
  ;;             ("<TAB>" . 'copilot-accept-completion)
  ;;             ("TAB" . 'copilot-accept-completion)
  ;;             ("C-TAB" . 'copilot-accept-completion-by-word)
  ;;             ("C-<TAB>" . 'copilot-accept-completion-by-word))
  )

(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  (defun my/copilot-word-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion-by-word)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  ;; Use map! to bind keys in prog-mode and text-mode
  (map! :map (prog-mode-map text-mode-map)
        ;; :i "C-;" #'my/copilot-tab-or-default
        :i "<backtab>" #'my/copilot-tab-or-default
        ;; :i "C-S-l" #'my/copilot-word-or-default))
        :i "C-;" #'my/copilot-word-or-default))

;; `aidermacs' provides an Emacs interface for the Aider AI pair programming tool.
;; It allows for seamless interaction with Aider within Emacs buffers,
;; supporting various LLM backends and providing a transient menu for common tasks.
(use-package! aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq
   aidermacs-default-model "gemini/gemini-3-flash-preview"
   aidermacs-editor-model "gemini/gemini-3-flash-preview"
   aidermacs-architect-model "gemini/gemini-3-flash-preview"
   aidermacs-weak-model "gemini/gemini-3-flash-preview"
   aidermacs-watch-files t
   aidermacs-backend 'vterm
   aidermacs-auto-commits t
   ))

(use-package! claude-code
  :bind-keymap
  ("C-c C" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  (setopt claude-code-terminal-backend 'vterm)
  (claude-code-mode))

(after! agent-shell
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :api-key (getenv "GEMINI_API_KEY")))
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))

  ;; Replay the whole prior conversation into the buffer on resume, instead
  ;; of the default title-only restore.
  (setq agent-shell-session-restore-verbosity 'full)

  ;; The graphical header is an SVG sized from the default font (icon is
  ;; three char-heights tall), so laptop mode's size-40 font blows it up to
  ;; ~200px and it swallows the side window. Cap the sizes it renders at.
  (defvar my/agent-shell-header-font-cap '((:font-height . 26) (:font-size . 20))
    "Upper bounds for the font metrics `agent-shell' renders its header at.")

  (defun my/agent-shell-cap-header-size (model)
    "Clamp MODEL's font metrics to `my/agent-shell-header-font-cap'."
    (pcase-dolist (`(,key . ,cap) my/agent-shell-header-font-cap)
      (when-let* ((value (map-elt model key)))
        (map-put! model key (min value cap))))
    model)

  (advice-add 'agent-shell--make-header-model :filter-return
              #'my/agent-shell-cap-header-size)

  ;; Show the agent in a right-hand side window instead of taking over the
  ;; current one. `agent-shell-toggle' (SPC a a) shows/hides it.
  (setq agent-shell-display-action
        '(display-buffer-in-side-window
          (side . right)
          (window-width . 0.42)))

  ;; Files opened from @mentions / links open beside the conversation,
  ;; rather than replacing it.
  (setq agent-shell-file-display-action '(display-buffer-pop-up-window))

  ;; Upstream only offers slash-command completion when / is the very first
  ;; character of the input; the TUI offers it after any whitespace. Match
  ;; the TUI. (`agent-shell--completion-bounds' still requires / to follow
  ;; whitespace, so this doesn't fire inside paths or mid-word.)
  (advice-add 'agent-shell-completion--command-start-p :override #'always)

  ;; `global-corfu-mode' doesn't reach these buffers (shells restored from the
  ;; desktop session predate corfu's lazy load), so @/ completion fell back to
  ;; the static `*Completions*' window, which any edit -- a backspace over a
  ;; typo -- ends outright. Corfu's popup narrows as you edit instead.
  (add-hook 'agent-shell-mode-hook #'corfu-mode)

  ;; Corfu only auto-reopens after commands matching `corfu-auto-commands'.
  ;; `self-insert-command' is there, but the queue/steer remap replaces it in
  ;; these buffers, and evil's backspace isn't a `delete-backward-char'.
  (dolist (cmd '("my/agent-shell-self-insert-or-steer\\'"
                 "\\`evil-delete-backward-char"))
    (add-to-list 'corfu-auto-commands cmd))

  ;; TUI-style queueing: while the agent is busy, just start typing in the
  ;; shell buffer and the keystroke opens the minibuffer (prefilled with that
  ;; char). RET there steers the prompt into the running turn; C-g abandons.
  ;; Agents without steering support fall back to queueing. Same upstream
  ;; idiom `agent-shell-send-region' uses when the shell is busy.
  (defun my/agent-shell-self-insert-or-steer (n)
    "Steer a prompt into the running turn, else `self-insert-command'."
    (interactive "p")
    (if (and (derived-mode-p 'agent-shell-mode) (shell-maker-busy))
        (let ((steer (agent-shell-steering-supported-p)))
          (funcall (if steer #'agent-shell-prompt-steer #'agent-shell-prompt-queue)
                   (agent-shell--prompt-queue-read
                    :initial (string last-command-event))))
      (self-insert-command n last-command-event)))
  (define-key agent-shell-mode-map [remap self-insert-command]
              #'my/agent-shell-self-insert-or-steer)

  ;; Long-horizon agent-shell: Emacs equivalent of ~/.local/bin/claude-lh.
  ;; claude-lh exports CLAUDE_LH=<its pid>; the Stop hook (context-checkpoint.sh)
  ;; checkpoints only when that is set, wrapup writes .claude/relaunch.$CLAUDE_LH,
  ;; the hook SIGTERMs claude at turn end, and claude-lh restarts it with a
  ;; "resume" prompt. Here:
  ;;  - CLAUDE_LH is a per-project `sleep infinity' sentinel pid. It must be a
  ;;    live pid (wrapup does `kill -0') with no claude children (the hook's
  ;;    `pgrep -P' kills the first match; with the emacs pid that could be another
  ;;    project's agent). Stable across restarts, so flags keyed to it survive.
  ;;  - after each turn (or error: the hook kills the SDK process at Stop, which
  ;;    surfaces as a failed prompt) check for the relaunch flag. Present: rm it,
  ;;    touch auto_resume, restart the shell, submit "resume" once ready.
  (defvar my/agent-shell-lh-sentinels nil
    "Alist of (project-dir . sentinel process) backing CLAUDE_LH.")

  (defun my/agent-shell-lh-pid (&optional create)
    "Sentinel pid for `default-directory', spawning one when CREATE."
    (let* ((dir (expand-file-name default-directory))
           (proc (alist-get dir my/agent-shell-lh-sentinels nil nil #'equal)))
      (when (and create (not (process-live-p proc)))
        (setq proc (make-process :name "claude-lh-sentinel" :command '("sleep" "infinity")
                                 :noquery t :connection-type 'pipe))
        (setf (alist-get dir my/agent-shell-lh-sentinels nil nil #'equal) proc))
      (and (process-live-p proc) (process-id proc))))

  (defun my/agent-shell-lh-env (orig &rest args)
    "Around `agent-shell-anthropic-make-claude-client': export CLAUDE_LH."
    (let ((agent-shell-anthropic-claude-environment
           (cons (format "CLAUDE_LH=%d" (with-current-buffer (plist-get args :buffer)
                                           (my/agent-shell-lh-pid t)))
                 agent-shell-anthropic-claude-environment)))
      (apply orig args)))
  (advice-add 'agent-shell-anthropic-make-claude-client :around #'my/agent-shell-lh-env)

  (defun my/agent-shell-lh-maybe-relaunch (shell-buffer)
    "Restart SHELL-BUFFER and resume from the handoff when wrapup armed the flag."
    (when (buffer-live-p shell-buffer)
      (with-current-buffer shell-buffer
        (when-let* ((pid (my/agent-shell-lh-pid))
                    (flag (expand-file-name (format ".claude/relaunch.%d" pid)))
                    ((file-exists-p flag)))
          (delete-file flag)
          (with-temp-file (expand-file-name (format ".claude/auto_resume.%d" pid)))
          (message "agent-shell: context checkpoint, restarting and resuming from handoff")
          ;; Out of the event callback: restart kills this buffer.
          (run-at-time 0 nil #'my/agent-shell-lh-relaunch shell-buffer)))))

  (defun my/agent-shell-lh-relaunch (shell-buffer)
    (let ((name (buffer-name shell-buffer)))
      (with-current-buffer shell-buffer
        ;; The hook already SIGTERMed the agent; skip "Agent is busy. Restart anyway?".
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (agent-shell-restart)))
      (when-let* ((new (get-buffer name)))
        (agent-shell-subscribe-to
         :shell-buffer new :event 'prompt-ready
         :on-event (lambda (_)
                     (agent-shell--insert-to-shell-buffer
                      :shell-buffer new :text "resume" :submit t :no-focus t))))))

  (defun my/agent-shell-lh-subscribe (shell-buffer)
    "Watch SHELL-BUFFER for the relaunch flag after every turn."
    (dolist (event '(turn-complete error))
      (agent-shell-subscribe-to
       :shell-buffer shell-buffer :event event
       :on-event (lambda (_) (my/agent-shell-lh-maybe-relaunch shell-buffer))))
    shell-buffer)
  (advice-add 'agent-shell--start :filter-return #'my/agent-shell-lh-subscribe)

  ;; Agent picker sorted most-recently-used first.  The picker declares
  ;; `display-sort-function' as `identity', so vertico shows the candidate
  ;; list verbatim and the order has to be built here.
  (defvar my/agent-shell-config-mru nil
    "Agent config identifiers, most recently selected first.")
  (add-to-list 'savehist-additional-variables 'my/agent-shell-config-mru)

  (defun my/agent-shell-sort-configs-by-mru (configs)
    "Order CONFIGS by `my/agent-shell-config-mru', unused ones last."
    (seq-sort-by (lambda (config)
                   (or (seq-position my/agent-shell-config-mru
                                     (map-elt config :identifier))
                       most-positive-fixnum))
                 #'< configs))
  (advice-add 'agent-shell--resolved-agent-configs
              :filter-return #'my/agent-shell-sort-configs-by-mru)

  (defun my/agent-shell-record-config-mru (config)
    "Move CONFIG's identifier to the front of `my/agent-shell-config-mru'."
    (when-let* ((id (map-elt config :identifier)))
      (setq my/agent-shell-config-mru
            (cons id (delq id my/agent-shell-config-mru))))
    config)
  (advice-add 'agent-shell-select-config
              :filter-return #'my/agent-shell-record-config-mru)

  ;; Clock time and turn duration after each reply.  Drawn as an overlay so it
  ;; never becomes transcript text, which the agent is replayed with on resume.
  (defvar-local my/agent-shell-turn-start nil
    "Time at which the current turn's prompt was submitted.")

  (defun my/agent-shell-format-duration (seconds)
    "Format SECONDS as a compact duration string."
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm %ds" (/ seconds 60) (% seconds 60)))
          (t (format "%dh %dm" (/ seconds 3600) (% (/ seconds 60) 60)))))

  (defun my/agent-shell-stamp-turn (shell-buffer)
    "Stamp each completed turn in SHELL-BUFFER with its time and duration."
    (agent-shell-subscribe-to
     :shell-buffer shell-buffer :event 'input-submitted
     :on-event (lambda (_)
                 (with-current-buffer shell-buffer
                   (setq my/agent-shell-turn-start (current-time)))))
    (agent-shell-subscribe-to
     :shell-buffer shell-buffer :event 'turn-complete
     :on-event
     (lambda (_)
       (with-current-buffer shell-buffer
         ;; The turn ends with the next prompt already drawn, so walk back from
         ;; it to the reply's last character.  Anchoring on the prompt position
         ;; itself loses to the display properties that render its "Me" block.
         (let* ((prompt-start (and comint-last-prompt
                                   (agent-shell--live-input-prompt-p
                                    comint-last-prompt)
                                   (marker-position (car comint-last-prompt))))
                (anchor (save-excursion
                          (goto-char (or prompt-start (point-max)))
                          (skip-chars-backward " \t\n")
                          (point))))
           (when (> anchor (point-min))
             (let ((elapsed (when my/agent-shell-turn-start
                              (round (float-time
                                      (time-since my/agent-shell-turn-start)))))
                   (overlay (make-overlay (1- anchor) anchor)))
               (setq my/agent-shell-turn-start nil)
               (overlay-put
                overlay 'after-string
                (propertize (format "\n%s%s"
                                    (format-time-string "%H:%M")
                                    (if elapsed
                                        (concat " · " (my/agent-shell-format-duration
                                                       elapsed))
                                      ""))
                            'face 'shadow))))))))
    shell-buffer)
  (advice-add 'agent-shell--start :filter-return #'my/agent-shell-stamp-turn)
)

(use-package! magit-gptcommit
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :init
  (require 'llm-gemini)
  :custom
  (magit-gptcommit-llm-provider (make-llm-gemini :key (getenv "GEMINI_API_KEY") :model "gemini-2.0-flash"))
  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(defun my/dired-add-marked-files-to-claude ()
  "Add paths of marked Dired files to a Claude Code session.
If multiple sessions exist for the current directory, prompt to select one.
Uses the same session-selection mechanism as claude-code.el."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (let* ((files (dired-get-marked-files))
         (cmd (mapconcat (lambda (f) (concat "@" f)) files " ")))
    (when (string-empty-p cmd)
      (user-error "No files marked"))
    (claude-code--do-send-command cmd)))
