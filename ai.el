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

(defvar-local my/agent-shell-sidebar-timer nil)
(defvar-local my/agent-shell-sidebar-snapshot nil)

(defun my/agent-shell-sidebar-entries ()
  "Return picker-style rows sorted by working directory, then buffer name."
  ;; shortcut: picker formatting is private upstream; update this with picker changes.
  (let* ((buffers (agent-shell-buffers))
         (ids (mapcar (lambda (buffer)
                        (with-current-buffer buffer
                          (map-nested-elt agent-shell--state '(:agent-config :identifier))))
                      buffers))
         (homogeneous (seq-every-p (lambda (id) (eq id (car ids))) ids))
         (entries
          (mapcar
           (lambda (buffer)
             (with-current-buffer buffer
               (let* ((config (map-elt agent-shell--state :agent-config))
                      (prefix (agent-shell--buffer-name-prefix (map-elt config :buffer-name)))
                      (title (string-trim
                              (car (split-string
                                    (or (map-nested-elt agent-shell--state '(:session :title)) "")
                                    "\n")))))
                 (list buffer
                       (if (and homogeneous prefix)
                           (string-remove-prefix prefix (buffer-name))
                         (buffer-name))
                       (symbol-name (agent-shell-status))
                       (if (> (length title) 50) (concat (substring title 0 47) "...") title)
                       (when agent-shell-show-config-icons
                         (agent-shell--config-icon :config config))
                       ;; Shell creation initializes this from `agent-shell-cwd'.
                       (directory-file-name (expand-file-name default-directory))))))
           buffers))
         (name-width (apply #'max 0 (mapcar (lambda (entry) (length (nth 1 entry))) entries)))
         (status-width (apply #'max 0 (mapcar (lambda (entry) (length (nth 2 entry))) entries))))
    (setq entries (sort entries
                        (lambda (a b)
                          (if (equal (nth 5 a) (nth 5 b))
                              (string-lessp (buffer-name (car a)) (buffer-name (car b)))
                            (string-lessp (nth 5 a) (nth 5 b))))))
    (mapcar
     (lambda (entry)
       (pcase-let ((`(,buffer ,name ,status ,title ,icon ,directory) entry))
         (cons (propertize
                (concat (if icon (concat icon " ") "")
                        (string-pad (propertize name 'face 'agent-shell-buffer-name) (1+ name-width))
                        (string-pad (propertize status 'face
                                                (pcase status
                                                  ("busy" 'agent-shell-warning)
                                                  ("blocked" 'agent-shell-error)
                                                  (_ 'agent-shell-success)))
                                    (1+ status-width))
                        (propertize title 'face 'agent-shell-session-title))
                'my/agent-shell-sidebar-directory directory)
               buffer)))
     entries)))

(defun my/agent-shell-sidebar-refresh (&optional force)
  "Refresh session rows when changed, preserving selection and scroll."
  (interactive "P")
  (let ((entries (my/agent-shell-sidebar-entries)))
    (when (or force (not (equal-including-properties entries my/agent-shell-sidebar-snapshot)))
      (let ((selected (get-text-property (point) 'my/agent-shell-sidebar-buffer))
            (old-point (point))
            (windows (mapcar (lambda (window)
                               (list window (window-start window) (window-hscroll window)
                                     (get-text-property (window-point window) 'my/agent-shell-sidebar-buffer)))
                             (get-buffer-window-list (current-buffer) nil t)))
            (inhibit-read-only t))
        (erase-buffer)
        (if entries
            (let (directory)
              (dolist (entry entries)
                (let ((next-directory (get-text-property 0 'my/agent-shell-sidebar-directory (car entry))))
                  (unless (equal directory next-directory)
                    (setq directory next-directory)
                    (insert (propertize (concat directory "\n") 'face 'font-lock-keyword-face))))
                (insert (propertize (concat (car entry) "\n")
                                    'my/agent-shell-sidebar-buffer (cdr entry)
                                    'mouse-face 'highlight))))
          (insert "No agent-shell sessions\n"))
        (setq my/agent-shell-sidebar-snapshot entries)
        (goto-char (or (and selected (text-property-any (point-min) (point-max)
                                                       'my/agent-shell-sidebar-buffer selected))
                       (min old-point (point-max))))
        (dolist (state windows)
          (pcase-let ((`(,window ,start ,hscroll ,buffer) state))
            (set-window-start window (min start (point-max)) t)
            (set-window-hscroll window hscroll)
            (when-let* ((position (and buffer (text-property-any (point-min) (point-max)
                                                                 'my/agent-shell-sidebar-buffer buffer))))
              (set-window-point window position))))))))

(defun my/agent-shell-sidebar-stop-timer ()
  "Stop the sidebar's refresh timer."
  (when (timerp my/agent-shell-sidebar-timer)
    (cancel-timer my/agent-shell-sidebar-timer))
  (setq my/agent-shell-sidebar-timer nil))

(defun my/agent-shell-sidebar-tick (buffer)
  "Refresh BUFFER only while it is visible."
  (when (and (buffer-live-p buffer) (get-buffer-window buffer t))
    (with-current-buffer buffer
      (my/agent-shell-sidebar-refresh))))

(defun my/agent-shell-sidebar-close ()
  "Hide this frame's sidebar without closing any sessions."
  (interactive)
  (when-let* ((window (get-buffer-window (current-buffer))))
    (delete-window window)))

(defun my/agent-shell-sidebar-open ()
  "Switch the invoking window to the session at point."
  (interactive)
  (let ((shell (get-text-property (point) 'my/agent-shell-sidebar-buffer)))
    (unless shell
      (user-error "Choose a session row"))
    (unless (buffer-live-p shell)
      (my/agent-shell-sidebar-refresh t)
      (user-error "This session is no longer available"))
    (let* ((origin (window-parameter (selected-window) 'my/agent-shell-sidebar-origin-window))
           (window (if (and (window-live-p origin)
                            (eq (window-frame origin) (selected-frame))
                            (not (eq (window-buffer origin) (current-buffer))))
                       origin
                     (car (sort (seq-filter
                                 (lambda (candidate)
                                   (and (not (window-dedicated-p candidate))
                                        (not (window-parameter candidate 'window-side))
                                        (not (eq (window-buffer candidate) (current-buffer)))))
                                 (window-list))
                                (lambda (a b) (> (window-use-time a) (window-use-time b)))))))
           (target (or (when agent-shell-prefer-viewport-interaction
                         (agent-shell-viewport--buffer :shell-buffer shell :existing-only t))
                       shell)))
      (unless window
        (user-error "No window available for this session"))
      (select-window window)
      (let ((dedicated (window-dedicated-p window)))
        (unwind-protect
            (progn
              (set-window-dedicated-p window nil)
              (switch-to-buffer target))
          (set-window-dedicated-p window dedicated))))))

(defun my/agent-shell-sidebar-fit (window)
  "Fit WINDOW to the session list on focus; collapse it on leaving."
  (when (window-live-p window)
    (if (eq window (selected-window))
        (fit-window-to-buffer window)
      (fit-window-to-buffer window 4 4))))

(defun my/agent-shell-sidebar-click (event)
  "Show the session clicked by EVENT."
  (interactive "e")
  (mouse-set-point event)
  (my/agent-shell-sidebar-open))

(defvar my/agent-shell-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'my/agent-shell-sidebar-open)
    (define-key map [mouse-1] #'my/agent-shell-sidebar-click)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "g") #'my/agent-shell-sidebar-refresh)
    (define-key map (kbd "q") #'my/agent-shell-sidebar-close)
    (define-key map (kbd "C-l") #'windmove-up)
    (define-key map (kbd "C-k") #'windmove-up)
    map))

(define-derived-mode my/agent-shell-sidebar-mode special-mode "Agent sessions"
  "Browse live Agent Shell sessions in a bottom bar."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (add-hook 'kill-buffer-hook #'my/agent-shell-sidebar-stop-timer nil t)
  (add-hook 'window-selection-change-functions #'my/agent-shell-sidebar-fit nil t))

(after! evil
  (evil-set-initial-state 'my/agent-shell-sidebar-mode 'motion)
  (evil-define-key 'motion my/agent-shell-sidebar-mode-map
    (kbd "RET") #'my/agent-shell-sidebar-open
    (kbd "g") #'my/agent-shell-sidebar-refresh
    (kbd "q") #'my/agent-shell-sidebar-close
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-up))

(defun my/agent-shell-sidebar-toggle ()
  "Toggle a bottom bar listing the same sessions as the buffer picker."
  (interactive)
  (require 'agent-shell)
  (let ((buffer (get-buffer-create "*Agent sessions*"))
        (origin (selected-window)))
    (with-current-buffer buffer
      (if (get-buffer-window buffer)
          (my/agent-shell-sidebar-close)
        (unless (derived-mode-p 'my/agent-shell-sidebar-mode)
          (my/agent-shell-sidebar-mode))
        (my/agent-shell-sidebar-refresh t)
        (let* ((window-sides-vertical nil)
               (window (display-buffer-in-side-window
                        buffer '((side . bottom) (slot . -2) (window-height . 8)
                                 (window-parameters . ((no-delete-other-windows . t)))))))
          (set-window-parameter window 'my/agent-shell-sidebar-origin-window origin)
          (select-window window))
        (unless (timerp my/agent-shell-sidebar-timer)
          (setq my/agent-shell-sidebar-timer
                (run-at-time 1 1 #'my/agent-shell-sidebar-tick buffer)))))))

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

  ;; Compact buffer names: "Claude@project" instead of "Claude Agent @ project".
  (setq agent-shell-buffer-name-format
        (lambda (agent-name project-name)
          (format "%s@%s" agent-name project-name)))

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

  ;; TUI-style queueing: while the agent is busy, just start typing (or paste)
  ;; in the shell buffer and the keystroke opens the minibuffer, prefilled
  ;; with that char or the clipboard. RET there steers the prompt into the
  ;; running turn; C-g abandons. Agents without steering support fall back to
  ;; queueing. Same upstream idiom `agent-shell-send-region' uses when busy.
  (defun my/agent-shell-steer-prompt (initial)
    "Read a prompt prefilled with INITIAL, then steer it or queue it."
    (funcall (if (agent-shell-steering-supported-p)
                 #'agent-shell-prompt-steer
               #'agent-shell-prompt-queue)
             (agent-shell--prompt-queue-read :initial initial)))

  (defun my/agent-shell-busy-p ()
    "Non-nil in a shell buffer whose agent is mid-turn."
    (and (derived-mode-p 'agent-shell-mode) (shell-maker-busy)))

  (defun my/agent-shell-self-insert-or-steer (n)
    "Steer a prompt into the running turn, else `self-insert-command'."
    (interactive "p")
    (if (my/agent-shell-busy-p)
        (my/agent-shell-steer-prompt (string last-command-event))
      (self-insert-command n last-command-event)))

  (defun my/agent-shell-paste-or-steer ()
    "Steer the clipboard into the running turn, else paste as usual.
Pasting mid-turn otherwise lands in the read-only transcript above the
prompt, since only `self-insert-command' is remapped to steer."
    (interactive)
    (if (my/agent-shell-busy-p)
        ;; `current-kill' pulls in the system clipboard and signals when the
        ;; kill ring is empty; an empty prefill just means type it yourself.
        (my/agent-shell-steer-prompt (ignore-errors (current-kill 0)))
      (call-interactively
       ;; Pre-remap command, so `p' still pastes the evil way and C-y the
       ;; Emacs way.  Guard the M-x case, where it is this command itself.
       (if (memq this-original-command '(nil my/agent-shell-paste-or-steer))
           #'yank
         this-original-command))))

  (define-key agent-shell-mode-map [remap self-insert-command]
              #'my/agent-shell-self-insert-or-steer)
  (define-key agent-shell-mode-map [remap evil-paste-after]
              #'my/agent-shell-paste-or-steer)
  (define-key agent-shell-mode-map [remap evil-paste-before]
              #'my/agent-shell-paste-or-steer)
  (define-key agent-shell-mode-map [remap yank]
              #'my/agent-shell-paste-or-steer)

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
