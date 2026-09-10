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

  ;; Show the agent in a right-hand side window instead of taking over the
  ;; current one. `agent-shell-toggle' (SPC a a) shows/hides it.
  (setq agent-shell-display-action
        '(display-buffer-in-side-window
          (side . right)
          (window-width . 0.42)))

  ;; Files opened from @mentions / links open beside the conversation,
  ;; rather than replacing it.
  (setq agent-shell-file-display-action '(display-buffer-pop-up-window))

  ;; TUI-style queueing: while the agent is busy, just start typing in the
  ;; shell buffer and the keystroke opens the queue minibuffer (prefilled
  ;; with that char). RET there enqueues; C-g abandons. Same upstream idiom
  ;; `agent-shell-send-region' uses when the shell is busy.
  (defun my/agent-shell-self-insert-or-queue (n)
    "Queue a prompt when the shell is busy, else `self-insert-command'."
    (interactive "p")
    (if (and (derived-mode-p 'agent-shell-mode) (shell-maker-busy))
        (agent-shell-prompt-queue
         (agent-shell--prompt-queue-read :initial (string last-command-event)))
      (self-insert-command n last-command-event)))
  (define-key agent-shell-mode-map [remap self-insert-command]
              #'my/agent-shell-self-insert-or-queue)

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
