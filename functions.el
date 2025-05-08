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
                           '((reusable-frames . visible)
                             (inhibit-switch-frame . nil)))))

  (add-to-list 'display-buffer-alist
               (cons "\\`\\*copilot-.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . visible)
                             (inhibit-switch-frame . nil)))))

  (add-to-list 'display-buffer-alist
               (cons "\\`\\*aidermacs.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . visible)
                             (inhibit-switch-frame . nil))))))

;; Define the 'laptop-mode' minor mode
(define-minor-mode laptop-mode
  "A mode for adjusting settings when working on a laptop."
  :init-value nil
  :lighter " Laptop"
  :keymap nil
  (if laptop-mode
      (progn
        ;; Enable settings for all existing buffers
        (setq doom-font (font-spec :family "Iosevka Comfy" :weight 'regular :size 22))
        (doom/reload-font)
        (dolist (buf (buffer-list)) ;; Apply text scale to all open buffers
          (with-current-buffer buf
            (text-scale-increase 1)))
        ;; Apply settings for all new files
        (add-hook 'find-file-hook 'enable-laptop-mode-on-file-open))
    ;; Disable settings for all existing buffers
    (setq doom-font (font-spec :family "Iosevka Comfy" :weight 'regular :size 15))
    (doom/reload-font)
    (dolist (buf (buffer-list)) ;; Reset text scale for all open buffers
      (with-current-buffer buf
        (text-scale-set 0)))
    ;; Remove the hook for new files
    (remove-hook 'find-file-hook 'enable-laptop-mode-on-file-open)))

;; Function to apply laptop-mode settings to a new buffer
(defun enable-laptop-mode-on-file-open ()
  "Apply laptop mode font and text scaling to a newly opened buffer."
  (text-scale-increase 1))

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

(defun my/jupytext-file ()
  "Run jupytext to set formats to py:percent,ipynb for the current file.
For .py files, only run if a corresponding .ipynb file exists."
  (interactive)
  (when buffer-file-name
    (let* ((file-path buffer-file-name)
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
       ;; If it's a py file, only run if the corresponding ipynb exists
       ((and (string= extension "py")
             (file-exists-p ipynb-path))
        (message "Running jupytext on %s (paired with %s)" file-path ipynb-path)
        (start-process "jupytext-process" "*jupytext-output*"
                       "jupytext" "--set-formats" "py:percent,ipynb" file-path)
        (message "Jupytext conversion initiated for %s" file-path))))))

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
