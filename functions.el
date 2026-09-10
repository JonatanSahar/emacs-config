;;; functions.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

;; Flyspell: jump to next/previous error and correct it
(defun my/flyspell-next-and-correct ()
  "Go to the next flyspell error and correct it."
  (interactive)
  (call-interactively #'evil-next-flyspell-error)
  (call-interactively #'flyspell-correct-at-point))

(defun my/flyspell-prev-and-correct ()
  "Go to the previous flyspell error and correct it."
  (interactive)
  (call-interactively #'evil-prev-flyspell-error)
  (call-interactively #'flyspell-correct-at-point))

;; Custom Functions & Macros
;; =========================
(defun my/save-and-change-to-normal ()
  (interactive)
  (evil-normal-state)
  (save-buffer))

(defun my-shell nil (interactive) (shell) (popper-toggle-type) (evil-normal-state))

(fset 'copy-with-square-brackets
      (kmacro [?y ?a ?\]] 0 "%d"))

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
