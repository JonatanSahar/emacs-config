;;; functions.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yonatan
;;
;; Author: Yonatan <yonatan@fedora>
;; Maintainer: Yonatan <yonatan@fedora>
;; Created: April 04, 2025
;; Modified: April 04, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/yonatan/functions
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun extract-code-fences (text)
  "extract text between code fences denoted by triple single quotes ''' in text."
  (let ((start nil) (results '()) (in-fence nil))
    ;; loop through each line of text
    (dolist (line (split-string text "\n"))
      (cond
       ;; check for the start or end of a code fence
       ((string-match-p "^'''$\\|^'''.*" line)
        (setq in-fence (not in-fence))
        ;; if exiting a code fence, save collected lines and reset start
        (when (and in-fence (not start))
          (setq start (point)))
        (unless in-fence
          (push (reverse results) start)
          (setq results '())))
       ;; collect lines if within a code fence
       (in-fence
        (push line results))))
    ;; return the collected content between fences as a single string
    (mapconcat (lambda (lines)
                 (string-join lines "\n"))
               (nreverse start) "\n")))

(defun my/search-replace ()
  (interactive)
  (if (use-region-p) (my/search-replace-in-region) (evil-ex "%s/"))
  )

;; My functions
(defun my/search-replace-in-region ()
  (interactive)
  ;; (if (eq last-command 'evil-yank)
  (let ((evil-ex-initial-input "s/"))
    (call-interactively 'evil-ex)))

(defun my/make-cursor-here ()
  (interactive)
  (+multiple-cursors/evil-mc-toggle-cursor-here)
  (evil-mc-pause-cursors))

(defun my/toggle-org-timer ()
  "Toggle org timer on or off. If a timer is running, stop it. Otherwise, start a new timer."
  (interactive)
  (if (and (boundp 'org-timer-start-time )
           org-timer-start-time)
      (progn
        (org-timer-stop)
        (message "Org timer stopped."))
    (progn
      (org-timer-start)
      (message "Org timer started."))))

(defun evil-kill-to-prev-word-end ()
  "Kill from point to the end of the prev word."
  (interactive)
  (evil-delete (point) (progn (evil-backward-word-end) (point))))

(defun evil-kill-to-next-word-start ()
  "Kill from point to the start of the next word."
  (interactive)
  (evil-delete (point) (progn (evil-forward-word-begin) (point))))

(defun conditional-evil-kill-to-prev-word-end ()
  "Kill to the end of the previous word only if the previous character is whitespace or at the beginning of a line."
  (interactive)
  (if (or (bolp) ; At beginning of line
          (save-excursion (backward-char) (looking-at-p "\\s-"))) ; Previous char is whitespace
      (evil-delete (point) (save-excursion
                             (evil-backward-word-end)
                             (forward-char) ; Move forward to avoid deleting the last char
                             (point)))
    (call-interactively 'backward-kill-word)))

(defun conditional-evil-kill-to-next-word-start ()
  "Kill to the start of the next word only if the next character is whitespace or at the end of a line."
  (interactive)
  (if (or (looking-at-p "\\s-") (eolp)) ; Check for whitespace or end of line
      (evil-kill-to-next-word-start)
    (call-interactively 'kill-word)))

(map! :ni "C-<backspace>" #'conditional-evil-kill-to-prev-word-end
      :ni "C-<delete>" #'conditional-evil-kill-to-next-word-start)

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

(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(after! org)

(defun my/search-replace-in-region ()
  (interactive)
  ;; (if (eq last-command 'evil-yank)
  (let ((evil-ex-initial-input "s/"))
    (call-interactively 'evil-ex)))

(load! "keybindings.el")

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
                             (inhibit-switch-frame . nil))))))

;; Jupyter cell execution status (per buffer)
(defface my/jupyter-cell-status-running
  '((t :inherit warning))
  "Face for running Jupyter cell status.")

(defface my/jupyter-cell-status-done
  '((t :inherit success))
  "Face for completed Jupyter cell status.")

(defface my/jupyter-cell-status-idle
  '((t :inherit shadow))
  "Face for idle Jupyter cell status.")

(defvar-local my/jupyter-cell-status--overlay nil)
(defvar-local my/jupyter-cell-status--requests nil)
(defvar-local my/jupyter-cell-status--pending 0)
(defvar-local my/jupyter-cell-status--last-start nil)
(defvar-local my/jupyter-cell-status--last-finish nil)
(defvar-local my/jupyter-cell-status--timer nil)
(defvar-local my/jupyter-cell-status--cleanup-hook-added nil)

(defun my/jupyter-cell-status--ensure-overlay ()
  (unless (overlayp my/jupyter-cell-status--overlay)
    (setq my/jupyter-cell-status--overlay (make-overlay (point-min) (point-min) nil t t))
    (overlay-put my/jupyter-cell-status--overlay 'priority 9999)
    (overlay-put my/jupyter-cell-status--overlay 'evaporate t))
  (move-overlay my/jupyter-cell-status--overlay (point-min) (point-min))
  (unless my/jupyter-cell-status--cleanup-hook-added
    (add-hook 'kill-buffer-hook #'my/jupyter-cell-status--cleanup nil t)
    (setq my/jupyter-cell-status--cleanup-hook-added t)))

(defun my/jupyter-cell-status--format-elapsed (start-time)
  (when start-time
    (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
           (total (max 0 (floor elapsed)))
           (minutes (/ total 60))
           (seconds (% total 60))
           (hours (/ minutes 60))
           (minutes (% minutes 60)))
      (if (> hours 0)
          (format "%dh%02dm%02ds" hours minutes seconds)
        (format "%02dm%02ds" minutes seconds)))))

(defun my/jupyter-cell-status--format-line ()
  (cond
   ((> my/jupyter-cell-status--pending 0)
    (let* ((count (if (> my/jupyter-cell-status--pending 1)
                      (format " (%d)" my/jupyter-cell-status--pending)
                    ""))
           (elapsed (my/jupyter-cell-status--format-elapsed
                     my/jupyter-cell-status--last-start))
           (elapsed-suffix (if elapsed (format " %s" elapsed) "")))
      (format "Jupyter: running%s%s\n" count elapsed-suffix)))
   (my/jupyter-cell-status--last-finish
    (format "Jupyter: done at %s\n"
            (format-time-string "%H:%M:%S" my/jupyter-cell-status--last-finish)))
   (t
    "Jupyter: idle\n")))

(defun my/jupyter-cell-status--update ()
  (my/jupyter-cell-status--ensure-overlay)
  (let* ((text (my/jupyter-cell-status--format-line))
         (face (cond
                ((> my/jupyter-cell-status--pending 0) 'my/jupyter-cell-status-running)
                (my/jupyter-cell-status--last-finish 'my/jupyter-cell-status-done)
                (t 'my/jupyter-cell-status-idle))))
    (overlay-put my/jupyter-cell-status--overlay
                 'before-string
                 (propertize text 'face face))))

(defun my/jupyter-cell-status--ensure-timer ()
  (unless (timerp my/jupyter-cell-status--timer)
    (setq my/jupyter-cell-status--timer
          (run-with-timer 1 1 #'my/jupyter-cell-status--tick (current-buffer)))))

(defun my/jupyter-cell-status--stop-timer ()
  (when (timerp my/jupyter-cell-status--timer)
    (cancel-timer my/jupyter-cell-status--timer)
    (setq my/jupyter-cell-status--timer nil)))

(defun my/jupyter-cell-status--prune-requests ()
  (let (pending)
    (dolist (req my/jupyter-cell-status--requests)
      (when (and req (not (jupyter-request-idle-p req)))
        (push req pending)))
    (setq my/jupyter-cell-status--requests (nreverse pending))
    (setq my/jupyter-cell-status--pending (length my/jupyter-cell-status--requests))))

(defun my/jupyter-cell-status--tick (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((was-running (> my/jupyter-cell-status--pending 0)))
        (when my/jupyter-cell-status--requests
          (my/jupyter-cell-status--prune-requests))
        (when (and was-running (zerop my/jupyter-cell-status--pending))
          (setq my/jupyter-cell-status--last-finish (current-time))
          (my/jupyter-cell-status--stop-timer))
        (my/jupyter-cell-status--update)))))

(defun my/jupyter-cell-status--start (req buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/jupyter-cell-status--requests
            (cons req my/jupyter-cell-status--requests))
      (setq my/jupyter-cell-status--pending (length my/jupyter-cell-status--requests))
      (setq my/jupyter-cell-status--last-start (current-time))
      (setq my/jupyter-cell-status--last-finish nil)
      (my/jupyter-cell-status--ensure-timer)
      (my/jupyter-cell-status--update))))

(defun my/jupyter-cell-status--cleanup ()
  (my/jupyter-cell-status--stop-timer)
  (setq my/jupyter-cell-status--requests nil)
  (setq my/jupyter-cell-status--pending 0)
  (setq my/jupyter-cell-status--last-start nil)
  (setq my/jupyter-cell-status--last-finish nil)
  (when (overlayp my/jupyter-cell-status--overlay)
    (delete-overlay my/jupyter-cell-status--overlay)
    (setq my/jupyter-cell-status--overlay nil)))

(defun my/jupyter-cell-status--around-eval (orig-fn &rest args)
  (let ((buffer (current-buffer))
        (req (apply orig-fn args)))
    (when (and (buffer-live-p buffer) (jupyter-request-p req))
      (my/jupyter-cell-status--start req buffer))
    req))

(after! jupyter
  (advice-add 'jupyter-eval-string :around #'my/jupyter-cell-status--around-eval))

;; Define the 'laptop-mode' minor mode
(define-minor-mode laptop-mode
  "A mode for adjusting settings when working on a laptop."
  :init-value nil
  :lighter " Laptop"
  :keymap nil
  (if laptop-mode
      ;; DONE also toggle fullscreen mode when enabling laptop-mode, don't disable it when disabling laptop-mode
      (progn
        (setq doom-font (font-spec :family "Iosevka Comfy" :weight 'regular :size 40))
        (doom/reload-font)
        (unless (eq (frame-parameter nil 'fullscreen) 'fullboth)
          (toggle-frame-fullscreen))
        (add-hook 'find-file-hook 'enable-laptop-mode-on-file-open))
    (setq doom-font (font-spec :family "Iosevka Comfy" :weight 'regular :size 20))
    (doom/reload-font)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (text-scale-set 0)))
    (remove-hook 'find-file-hook 'enable-laptop-mode-on-file-open)))

;; Function to apply laptop-mode settings to a new buffer
(defun enable-laptop-mode-on-file-open ()
  "Apply laptop mode font to a newly opened buffer."
  nil)

;; Bind laptop-mode to SPC t L in Doom Emacs
(map! :leader
      :desc "Toggle Laptop Mode"
      "t L" #'laptop-mode)


;; Custom Functions & Macros
;; =========================
(defun my-evil-end-of-visual-line ()
  "Wrapper for evil-end-of-visual-line that preserves visual selection."
  (interactive)
  (evil-end-of-visual-line))

(defun my-evil-end-of-visual-line-visual-mode ()
  "Wrapper for evil-end-of-visual-line that preserves visual selection."
  (interactive)
  (evil-end-of-visual-line)
  (evil-forward-char))

(defun my/save-and-change-to-normal ()
  (interactive)
  (evil-normal-state)
  (save-buffer))

(defun my/python-eval-line-or-defun ()
  (interactive)
  (if (and (or (eq major-mode 'python-mode)
               (eq major-mode 'python-ts-mode))
           (or (eq evil-state 'normal)
               (eq evil-state 'insert))
           (save-excursion
             (beginning-of-line)
             (looking-at "^[ \t]*def ")))
      (jupyter-eval-defun)
    (jupyter-eval-line-or-region)))

(defun my-shell nil (interactive) (shell) (popper-toggle-type) (evil-normal-state))

(defun make-bold()
  (interactive)
  (if (use-region-p) (evil-surround-region (region-beginning) (region-end) t *)))

(defun my/force-delete-frame ()
  "Force close a frame without prompting."
  (interactive)
  (delete-frame nil t)) ;; The second argument (force) makes it close without confirmation.

(defun my/jupytext-file (prefix)
  "Run jupytext to set formats to py:percent,ipynb for the current file.
For .py files, only run if a corresponding .ipynb file exists, unless a prefix argument (C-u) is given, which forces creation."
  (interactive "P")
  (when buffer-file-name
    (let* ((force-create (consp prefix)) ; C-u makes prefix a list like '(4)
           (file-path buffer-file-name)
           (extension (file-name-extension file-path))
           (base-name (file-name-sans-extension file-path))
           (ipynb-path (concat base-name ".ipynb")))
      (cond
       ;; If it's an ipynb file, always run jupytext
       ((string= extension "ipynb")
        (message "Running jupytext on %s" file-path)
        (start-process "jupytext-process" "*jupytext-output*"
                       "jupytext" "--set-formats" "py:percent,ipynb" file-path)
        (message "Jupytext conversion initiated for %s" file-path))
       ;; If it's a py file, run if the corresponding ipynb exists OR if forced by prefix arg
       ((and (string= extension "py")
             (or (file-exists-p ipynb-path) force-create))
        (if force-create
            (message "Forcing jupytext on %s to create/pair with %s" file-path ipynb-path)
          (message "Running jupytext on %s (paired with %s)" file-path ipynb-path))
        (start-process "jupytext-process" "*jupytext-output*"
                       "jupytext" "--set-formats" "py:percent,ipynb" file-path)
        (message "Jupytext conversion initiated for %s" file-path))))))

(defun my/mount_chronos ()
  (interactive)
  (start-process "mount-chronos-process" "*mount-chronos-output*" "bash" "-ic" "mount_chronos"))

(defun my/org-move-line (direction)
  "Move line up or down with DIRECTION."
  (interactive)
  (if (org-at-heading-or-item-p)
      (if (eq direction 'forward)
          (call-interactively #'org-metadown)
        (call-interactively #'org-metaup))
    (if (eq direction 'forward)
        (call-interactively #'org-drag-line-forward)
      (call-interactively #'org-drag-line-backward))))

(defun my/org-meta-down ()
  "Move line down, but only if not in a heading or table."
  (interactive)
  (my/org-move-line 'forward))

(defun my/org-meta-up ()
  "Move line up, but only if not in a heading or table."
  (interactive)
  (my/org-move-line 'backward))

(fset 'copy-with-square-brackets
      (kmacro [?y ?a ?\]] 0 "%d"))

;; from https://github.com/renzmann/.emacs.d
(defun pyrightconfig-write (virtualenv)
  "Write a `pyrightconfig.json' file at the Git root of a project
with `venvPath' and `venv' set to the absolute path of
`virtualenv'.  When run interactively, prompts for a directory to
select."
  (interactive "DEnv: ")
  ;; Naming convention for venvPath matches the field for pyrightconfig.json
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))
    (message (concat "Configured `" out-file "` to use environment `" venv-dir))))

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

(defun my/org-copy-image-at-point-to-clipboard ()
  "Copy image link at point to the system clipboard on GNU/Linux.

Point must be on an Org file link whose target is an image file.
The image data is piped to an external clipboard tool (`wl-copy',
`xclip' or `xsel')."
  (interactive)
  (unless (eq system-type 'gnu/linux)
    (user-error "This command is only implemented for GNU/Linux"))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (require 'org)
  (let* ((context (org-element-context)))
    (unless (eq (org-element-type context) 'link)
      (user-error "Point is not on an Org link"))
    (let* ((path (org-element-property :path context))
           (type (org-element-property :type context)))
      (unless (and (stringp type) (string= type "file"))
        (user-error "Link at point is not a file link"))
      (unless (and path (> (length path) 0))
        (user-error "File link has no path"))
      (let* ((file (expand-file-name
                    path
                    (or (and buffer-file-name
                             (file-name-directory buffer-file-name))
                        default-directory))))
        (unless (file-exists-p file)
          (user-error "File does not exist: %s" file))
        (let* ((img-type (image-type-from-file-name file)))
          (unless img-type
            (user-error "Not a recognized image file: %s" file))
          (let* ((mime-type (pcase img-type
                              ('png "image/png")
                              ((or 'jpeg 'image-jpeg) "image/jpeg")
                              ('gif "image/gif")
                              ('svg "image/svg+xml")
                              ('tiff "image/tiff")
                              (_ "image/png")))
                 (program (cond
                           ((executable-find "wl-copy") "wl-copy")
                           ((executable-find "xclip") "xclip")
                           ((executable-find "xsel") "xsel")))
                 (args (cond
                        ((string= program "wl-copy")
                         (list "--type" mime-type))
                        ((string= program "xclip")
                         (list "-selection" "clipboard" "-t" mime-type "-i"))
                        ((string= program "xsel")
                         (list "--clipboard" "--input")))))
            (unless program
              (user-error "No clipboard tool found (need wl-copy, xclip, or xsel)"))
            (let ((exit-code (apply #'call-process program file nil nil args)))
              (if (zerop exit-code)
                  (message "Copied image to clipboard: %s" file)
                (user-error "Failed to copy image (exit %d)" exit-code))))))))
  )

(defun my/jupyter-show-buffer-repl-map ()
  "Display a buffer showing the mapping between Python files and their Jupyter REPLs."
  (interactive)
  (let ((entries '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and jupyter-current-client
                   buffer-file-name
                   (not (derived-mode-p 'jupyter-repl-mode)))
          (let* ((client jupyter-current-client)
                 (repl-buffer (condition-case nil
                                  (oref client buffer)
                                (error nil)))
                 (repl-name (if (buffer-live-p repl-buffer)
                                (buffer-name repl-buffer)
                              "<dead REPL>")))
            (push (cons (buffer-name buf) repl-name) entries)))))
    (let ((map-buf (get-buffer-create "*jupyter-buffer-repl-map*")))
      (with-current-buffer map-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Python Buffer → Jupyter REPL Mapping\n")
          (insert (make-string 50 ?=) "\n\n")
          (if entries
              (dolist (entry (nreverse entries))
                (insert (format "%-40s → %s\n" (car entry) (cdr entry))))
            (insert "(no Python buffers associated with a Jupyter REPL)\n"))
          (special-mode)))
      (pop-to-buffer map-buf))))

;;DONE don't convet in place, create a new file with the same base name
(defun my/markdown-buffer-to-org ()
  "Convert the current buffer from Markdown to Org using pandoc."
  (interactive)
  (unless (executable-find "pandoc")
    (user-error "pandoc not found in PATH"))
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((input (buffer-substring-no-properties (point-min) (point-max)))
         (output-file (concat (file-name-sans-extension buffer-file-name) ".org")))
    (when (and (file-exists-p output-file)
               (not (y-or-n-p (format "Overwrite existing file %s? " output-file))))
      (user-error "Aborted"))
    (with-temp-buffer
      (insert input)
      (let ((exit-code (call-process-region (point-min) (point-max)
                                            "pandoc" t t nil
                                            "-f" "markdown" "-t" "org")))
        (unless (zerop exit-code)
          (user-error "pandoc failed with exit code %d" exit-code))
        (let ((output (buffer-string)))
          (with-current-buffer (find-file-noselect output-file)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert output)
              (org-mode)
              (save-buffer)))
          (message "Wrote Org file via pandoc: %s" output-file))))))

(defun my/yank-buffer-path-relative-to-project (&optional other-window)
  "Copy the current buffer's path relative to its project root.
With prefix arg OTHER-WINDOW, copy the path of the buffer in the other window instead."
  (interactive "P")
  (let* ((buf (if other-window
                  (window-buffer (next-window (selected-window) 'no-minibuf))
                (current-buffer)))
         (filename (or (buffer-file-name (or (buffer-base-buffer buf) buf))
                       (buffer-local-value 'list-buffers-directory buf)
                       (buffer-local-value 'default-directory buf))))
    (let* ((project-root (with-current-buffer buf (doom-project-root)))
           (path (abbreviate-file-name
                  (file-relative-name filename (or project-root (file-name-directory
                                                                 (directory-file-name filename)))))))
      (kill-new path)
      (message "Copied path (%s): %s" (buffer-name buf) path))))
