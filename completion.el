;;; $DOOMDIR/completion.el -*- lexical-binding: t; -*-
;; consult / consult-dir / embark / xref / flycheck completion glue

(after! consult-dir
  (setopt consult-dir-sources '(consult-dir--source-default consult-dir--source-bookmark consult-dir--source-project consult-dir--source-recentf consult-dir--source-tramp-local consult-dir--source-tramp-ssh)))

(after! consult
  (consult-customize
   consult-buffer consult-buffer-other-window consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref consult-theme
   ;; consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key "C-.")

  (defun jnf/consult-find-using-fd (&optional dir initial)
    "Find project files. A replacement for `projectile-find-file'."
    (interactive "P")
    (let ((consult-find-command "fd --color=never --hidden --exclude .git/ --full-path ARG OPTS"))
      (consult-find dir initial)))

  (defun jnf/consult-line (consult-line-function &rest rest)
    "Advising function around `CONSULT-LINE-FUNCTION'.
        When there's an active region, use that as the first parameter
        for `CONSULT-LINE-FUNCTION'.  Otherwise, use the current word as
        the first parameter.  This function handles the `REST' of the
        parameters."
    (interactive)
    (apply consult-line-function
           (if (use-region-p) (buffer-substring (region-beginning) (region-end)))
           rest))

  (defun jnf/consult-ripgrep (consult-ripgrep-function &optional dir &rest rest)
    "Use region or thing at point to populate initial parameter for `CONSULT-RIPGREP-FUNCTION'.

When there's an active region, use that as the initial parameter
for the `CONSULT-RIPGREP-FUNCTION'.  Otherwise, use the thing at
point.

`DIR' use the universal argument (e.g. C-u prefix) to first set
the directory.  `REST' is passed to the `CONSULT-RIPGREP-FUNCTION'."
    (interactive "P")
    (apply consult-ripgrep-function
           dir
           (if (use-region-p) (buffer-substring (region-beginning) (region-end)))
           rest))

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'consult-line :around #'jnf/consult-line '((name . "wrapper")))
  (advice-add #'consult-ripgrep :around #'jnf/consult-ripgrep '((name . "wrapper")))

  (defun my/xref--current-group-header-pos ()
    "Return buffer position of current xref group header, or nil."
    (save-excursion
      (beginning-of-line)
      (while (and (not (bobp))
                  (not (get-text-property (point) 'xref-group)))
        (forward-line -1)
        (beginning-of-line))
      (when (get-text-property (point) 'xref-group)
        (point))))

  (defun my/xref-toggle-current-group ()
    "Toggle fold for the xref group (file subtree) at point."
    (interactive)
    (unless (derived-mode-p 'xref--xref-buffer-mode)
      (user-error "Not in an xref results buffer"))
    (let ((group-pos (my/xref--current-group-header-pos)))
      (unless group-pos
        (user-error "No xref group at point"))
      (save-excursion
        (goto-char group-pos)
        (let ((subtree-start (save-excursion (forward-line 1) (point))))
          (if (and (< subtree-start (point-max))
                   (outline-invisible-p subtree-start))
              (outline-show-subtree)
            (outline-hide-subtree))))))

  (with-eval-after-load 'xref
    (keymap-set xref--xref-buffer-mode-map "<backtab>" #'my/xref-toggle-current-group))

  ;; Updating the default to include "--ignore-case"
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --ignore-case"))

  (after! flycheck
    (define-key flycheck-command-map "!" #'consult-flycheck)))

(after! embark
  (defun copy-grep-results-as-kill (strings)
    (embark-copy-as-kill
     (mapcar (lambda (string)
               (substring string
                          (1+ (next-single-property-change
                               (1+ (next-single-property-change 0 'face string))
                               'face string))))
             strings)))

  (add-to-list 'embark-multitarget-actions 'copy-grep-results-as-kill)

  (defvar embark-consult-grep-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "w") #'copy-grep-results-as-kill)
      map)
    "Keymap for actions for consult-grep results."
    )

  (setf (alist-get 'consult-grep embark-keymap-alist) 'embark-consult-grep-map)

  (defun my/consult-notes--denote-path (cand)
    "Return Denote path text-property from CAND, or nil."
    (let ((path (get-text-property 0 'denote-path cand)))
      (and (stringp path) path)))

  (defun my/consult-notes--cand->xref (cand)
    "Convert consult-notes CAND to an xref item when possible."
    (when-let* ((path (my/consult-notes--denote-path cand)))
      (xref-make (abbreviate-file-name path)
                 (xref-make-file-location path 1 0))))

  (defun my/consult-notes-export-denote-xref (items)
    "Export consult-notes ITEMS to a navigable xref buffer."
    (require 'xref)
    (let ((xrefs (delq nil (mapcar #'my/consult-notes--cand->xref items))))
      (unless xrefs
        (user-error "No Denote-backed consult-notes candidates to export"))
      ;; Bypass `xref-show-xrefs-function' entirely (which may be `consult-xref')
      ;; and open a real persistent *xref* buffer.
      (set-buffer
       (xref--show-xref-buffer
        (lambda () xrefs)
        `((fetched-xrefs . ,xrefs)
          (window . ,(selected-window))
          (auto-jump . nil)
          (display-action))))))

  (with-eval-after-load 'consult-notes
    (setf (alist-get consult-notes-category embark-exporters-alist nil nil #'equal)
          #'my/consult-notes-export-denote-xref)
    (setf (alist-get `(file . ,consult-notes-category) embark-exporters-alist nil nil #'equal)
          #'my/consult-notes-export-denote-xref)))
