;;; $DOOMDIR/ui.el -*- lexical-binding: t; -*-
;; tabspaces, popper, treemacs, spacious-padding, highlight-thing, dwim-shell, dirvish, vterm, display-buffer rules, buffer faces, laptop-mode

(use-package! tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)

  :config

  (defun my/name-tab-by-project-or-default ()
    "return project name if in a project, or default tab-bar name if not.
the default tab-bar name uses the buffer name."
    (let ((project-name (projectile-project-name)))
      (if (string= "-" project-name)
          (tab-bar-tab-name-current)
        (projectile-project-name))))

  (map! :map tabspaces-command-map
        "C" #'tabspaces-clear-buffers
        "b" #'tabspaces-switch-to-buffer
        "d" #'tabspaces-close-workspace
        "k" #'tabspaces-kill-buffers-close-workspace
        "o" #'tabspaces-open-or-create-project-and-workspace
        "r" #'tabspaces-remove-current-buffer
        "R" #'tabspaces-remove-selected-buffer
        "s" #'tabspaces-switch-or-create-workspace
        "t" #'tabspaces-switch-buffer-and-tab)

  ;; filter buffers for consult-buffer

  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult-source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "workspace buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))

  ;; set dired buffer list for consult-buffer
  (defvar consult--source-dired
    (list :name     "Dired"
          :narrow   ?d
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items    (lambda () (consult--buffer-query
                                :mode 'dired-mode
                                :sort 'visibility
                                :as #'buffer-name)))
    "Set dired buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-dired)

  )

(use-package! popper
  :bind (
         ("C-`"   . #'popper-kill-latest-popup)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*messages\\*"
          "output\\*$"
          "\\*async shell command\\*"
          "\\*python\\*"
          "^*jupyter"
          "\\*matlab\\*"
          "\\*ibuffer\\*"
          "\\*denote-backlinks"
          "\\*chatgpt\\* "
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  ;; (map! :map prog-mode-map :nv "`" #'popper-toggle-latest)
  )                ; for echo area hints

(use-package! captain
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local captain-predicate (lambda () (nth 8 (syntax-ppss (point)))))))


  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local captain-predicate (lambda () t))))


  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local captain-predicate
                 (lambda () (not (org-in-src-block-p))))))
  (global-captain-mode)
  )

(use-package! tmr
  :init
  (setq tmr-sound-file "~/documents/sounds/tibetian-bowl-1.wav")
  )

(after! (:and treemacs ace-window)
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))
  (setq aw-scope 'global)
  )

(use-package! highlight-thing
  :hook (doom-first-buffer . global-highlight-thing-mode)
  :config
  (setopt
   highlight-thing-delay-seconds 0.5
   highlight-thing-case-sensitive-p t
   highlight-thing-exclude-thing-under-point t
   highlight-thing-prefer-active-region t
   highlight-thing-ignore-list '("false" "true")
   highlight-thing-all-visible-buffers-p t
   highlight-thing-limit-to-defun t
   )

  (setq
   highlight-thing-limit-to-region-in-large-buffers-p nil
   highlight-thing-narrow-region-lines 15
   highlight-thing-large-buffer-limit 5000)
  )

(after! treemacs
  (setopt treemacs-sorting 'mod-time-desc)
  (define-key treemacs-mode-map (kbd "C-l") #'windmove-right)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "y n") #'my/treemacs-copy-name-at-point)
  ;; Hide heavy data dirs on SSHFS mounts to avoid stat'ing ~100k files.
  (add-to-list 'treemacs-ignored-file-predicates
               (lambda (filename absolute-path)
                 (and (string-match-p "chronos-mount" absolute-path)
                      (string= filename "data"))))
  ;; Disable git for SSHFS mounts only — git status traverses the full
  ;; worktree (including data/) over the network. Local projects keep git.
  (defun my/treemacs--skip-sshfs-git (orig-fn path &rest args)
    "Skip git status for paths under SSHFS mounts."
    (unless (string-match-p "chronos-mount" path)
      (apply orig-fn path args)))
  (advice-add 'treemacs--git-status-process-function :around #'my/treemacs--skip-sshfs-git))

(use-package! spacious-padding
  :hook (doom-first-buffer . spacious-padding-mode))

(use-package! drag-stuff
  :defer t
  :init
  (map!
   "<M-up>"    #'drag-stuff-up
   "<M-down>"  #'drag-stuff-down))

(after! vterm
  (defvar-local my/vterm--copy-mode-prev-sticky-scroll nil
    "Saved value of `my/vterm-sticky-scroll' before entering `vterm-copy-mode'.")

  (defun my/vterm-copy-mode-toggle ()
    "Toggle `vterm-copy-mode'.

When exiting copy-mode, restore the previous follow vs sticky-scroll state."
    (interactive)
    (unless (derived-mode-p 'vterm-mode)
      (user-error "Not in a vterm buffer"))
    (if (bound-and-true-p vterm-copy-mode)
        (progn
          (vterm-copy-mode -1)
          (if my/vterm--copy-mode-prev-sticky-scroll
              (setq my/vterm-sticky-scroll t)
            (my/vterm-resume-follow)))
      (setq my/vterm--copy-mode-prev-sticky-scroll my/vterm-sticky-scroll)
      (vterm-copy-mode 1)))

  (add-hook 'vterm-mode-hook #'my/vterm-enable-sticky-scroll)
  (unless (advice-member-p #'my/vterm--skip-reset-when-sticky 'vterm-reset-cursor-point)
    (advice-add 'vterm-reset-cursor-point :around #'my/vterm--skip-reset-when-sticky))
  (map! :map vterm-mode-map
        :i "C-j" (kbd "<down>")
        :i "C-k" (kbd "<up>")
        :i "C-z" #'evil-emacs-state
        :nvi "C-u" (cmd! (vterm-copy-mode 1) (evil-previous-visual-line))))

(after! dirvish
  (setq dirvish-reuse-session 'resume)
  (add-hook! 'dirvish-directory-view-mode-hook (writeroom-mode +1)))

(defun my/dwim-shell-command-convert-image-to-jpg ()
    "convert all marked images to jpg(s)."
    (interactive)
    (require 'dwim-shell-command)
    (dwim-shell-command-on-marked-files
     "convert to jpg"
     "convert -verbose '<<f>>' '<<fne>>.jpg'"
     :utils "convert"))

(defun my/dwim-shell-command-convert-audio-to-mp3 ()
    "convert all marked audio to mp3(s)."
    (interactive)
    (require 'dwim-shell-command)
    (dwim-shell-command-on-marked-files
     "convert to mp3"
     "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
     :utils "ffmpeg"))

(defun my/dwim-shell-commands-files-combined-size ()
    "get files combined file size."
    (interactive)
    (require 'dwim-shell-command)
    (dwim-shell-command-on-marked-files
     "get files combined file size"
     "du -csh '<<*>>'"
     :utils "du"
     :on-completion (lambda (buffer _process)
                      (with-current-buffer buffer
                        (message "total size: %s"
                                 (progn
                                   (re-search-backward "\\(^[ 0-9.,]+[a-za-z]+\\).*total$")
                                   (match-string 1))))
                      (kill-buffer buffer))))

;; Utility function for Treemacs to quickly copy the name of the file or 
;; directory at the current cursor position to the clipboard.
(defun my/treemacs-copy-name-at-point ()
  "copy the filename or directory name at point in treemacs."
  (interactive)
  (let ((name (treemacs-node-at-point)))
    (if name
        (progn
          (kill-new (treemacs--get-label-of name))
          (message "copied: %s" (treemacs--get-label-of name)))
      (message "no file or directory at point"))))

;; vterm helpers
;; =============
(defvar-local my/vterm-sticky-scroll nil
  "When non-nil, keep vterm from auto-following new output in the current buffer.")

(defun my/vterm--at-bottom-p (&optional window)
  "Return non-nil when WINDOW (or the selected window) shows the buffer end.
Treat partially visible end-of-buffer as NOT being at the bottom (so that
scrolling up even slightly disables auto-follow during streaming output)."
  (let ((win (or window (selected-window))))
    (and (window-live-p win)
         (eq (window-buffer win) (current-buffer))
         ;; `pos-visible-in-window-p' returns nil for partially visible chars
         ;; unless PARTIALLY is non-nil. We want strict visibility here so that
         ;; scrollback mode engages immediately.
         (pos-visible-in-window-p (point-max) win))))

(defun my/vterm--recompute-sticky (&optional window)
  "Update `my/vterm-sticky-scroll' based on WINDOW visibility state."
  (when (derived-mode-p 'vterm-mode)
    (if (my/vterm--at-bottom-p window)
        (when my/vterm-sticky-scroll
          (setq my/vterm-sticky-scroll nil)
          (when (fboundp 'vterm-reset-cursor-point)
            (vterm-reset-cursor-point)))
      (setq my/vterm-sticky-scroll t))))

(defun my/vterm--handle-window-scroll (window _start)
  "Track manual WINDOW scrolling to toggle sticky scroll."
  (with-current-buffer (window-buffer window)
    (my/vterm--recompute-sticky window)))

(defun my/vterm--post-command (&rest _)
  "Update sticky scroll state after each command."
  (my/vterm--recompute-sticky (selected-window)))

(defun my/vterm-enable-sticky-scroll ()
  "Enable sticky-scroll tracking for the current vterm buffer."
  (setq my/vterm-sticky-scroll nil)
  (add-hook 'window-scroll-functions #'my/vterm--handle-window-scroll nil t)
  (add-hook 'post-command-hook #'my/vterm--post-command nil t))

(defun my/vterm-resume-follow ()
  "Jump to the prompt and resume auto-following output in vterm."
  (interactive)
  (when (derived-mode-p 'vterm-mode)
    (when (bound-and-true-p vterm-copy-mode)
      (vterm-copy-mode -1))
    (setq my/vterm-sticky-scroll nil)
    (goto-char (point-max))
    (when (fboundp 'vterm-reset-cursor-point)
      (vterm-reset-cursor-point))
    (recenter -1)))

(defun my/vterm--skip-reset-when-sticky (orig &rest args)
  "Skip `vterm-reset-cursor-point' via ORIG when sticky scrolling is active."
  (if my/vterm-sticky-scroll
      (point)
    (apply orig args)))

(defun my-buffer-face-mode-programming ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq writeroom-width 120)
  ;; (setq buffer-face-mode-face '(:extend t :family "Fira Code Retina"))
  (setq buffer-face-mode-face '(:extend t :family "Iosevka Comfy Duo"))
  (buffer-face-mode))

(defun my-buffer-face-mode-text ()
  (interactive)
  (setq writeroom-width 120)
  (setq buffer-face-mode-face '(:extend t :family "Iosevka Comfy Duo"))
  (buffer-face-mode)
  (set-face-attribute 'fixed-pitch nil :height 1.0)
  (set-face-attribute 'variable-pitch nil :height 1.0)
  )

;; disable company mode everywhere
(setq company-global-modes nil)

;; activate which-function-mode in all programming modes
(add-hook 'prog-mode-hook 'which-function-mode)

;; ;; Shouldn't be here but didn't work otherwise
(defun my/setup-display-rules ()
  "Configure display rules for Jupyter buffers."
  (interactive)
  (add-to-list 'display-buffer-alist
               (cons "\\`\\*jupyter-.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . t)
                             (inhibit-switch-frame . nil)))))

  (add-to-list 'display-buffer-alist
               (cons "\\`\\*copilot-.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . t)
                             (inhibit-switch-frame . nil)))))

  (add-to-list 'display-buffer-alist
               (cons "\\`\\*aidermacs.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . t)
                             (inhibit-switch-frame . nil)))))

  (add-to-list 'display-buffer-alist
               (cons "\\`\\*claude.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . t)
                             (inhibit-switch-frame . nil)))))

  (add-to-list 'display-buffer-alist
               (cons "\\`\\*codex.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . t)
                             (inhibit-switch-frame . nil))))))

(my/setup-display-rules)

(defvar my/laptop-mode nil
  "Non-nil when `my/laptop-mode' is active.")

(defun my/laptop-mode ()
  "Toggle laptop settings: large font (and fullscreen when enabling)."
  (interactive)
  (setq my/laptop-mode (not my/laptop-mode))
  (if my/laptop-mode
      (progn
        (setq doom-font (font-spec :family "Iosevka Comfy" :weight 'regular :size 40))
        (doom/reload-font)
        (unless (eq (frame-parameter nil 'fullscreen) 'fullboth)
          (toggle-frame-fullscreen))
        (message "Laptop mode enabled"))
    (setq doom-font (font-spec :family "Iosevka Comfy" :weight 'regular :size 20))
    (doom/reload-font)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (text-scale-set 0)))
    (message "Laptop mode disabled")))
