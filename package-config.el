;;; package-config.el --- Description -*- lexical-binding: t; -*-
;;; Code:


(use-package! denote
  :config
  ;; Remember to check the doc strings of those variables.
  (setq! denote-directory (expand-file-name "~/Documents/notes")
         denote-excluded-directories-regexp "export.*"
         denote-known-keywords '("emacs" "thesis")
         denote-infer-keywords t
         denote-sort-keywords t
         denote-prompts '(title keywords)
         denote-excluded-directories-regexp nil
         denote-excluded-keywords-regexp nil)

  (denote-rename-buffer-mode 1)
  (setq! denote-sort-dired-default-sort-component 'last-modified
         denote-sort-dired-default-reverse-sort nil
         denote-sort-dired-extra-prompts nil)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-allow-multi-word-keywords nil)
  (setq denote-date-format nil)
  (setq denote-backlinks-show-context t)

  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  (defun my/denote--pretty-format-filename (file)
    (let* (
           (title (denote-retrieve-filename-title file))
           (keywords (denote-extract-keywords-from-path file))
           (keywords-as-string (mapconcat 'identity keywords ", "))
           )
      (concat title "      " "(" keywords-as-string ")" )
      )
    )

  (defun my/denote--find-file-with-pretty-format (&optional initial-text)
    (interactive)
    (let* (
           (paths (mapcar #'(lambda (file)
                              (cons (my/denote--pretty-format-filename file) file))
                          (denote-directory-files)))
           (filename (cdr (assoc (completing-read "Select a file: " paths  nil t) paths)))
           )
      filename
      )
    )


  (defun my/denote-link()
    (interactive)
    (let ((denote-file-prompt 'my/denote--find-file-with-pretty-format))
      (advice-add 'denote-file-prompt :around denote-file-prompt)
      )
    (call-interactively 'denote-link)
    (advice-remove 'denote-file-prompt 'my/denote--find-file-with-pretty-format)
    )

  (defun my/denote-link-or-create()
    (interactive)
    (let ((denote-file-prompt 'my/denote--find-file-with-pretty-format))
      (advice-add 'denote-file-prompt :around denote-file-prompt)
      )
    (call-interactively 'denote-link-or-create)
    (advice-remove 'denote-file-prompt 'my/denote--find-file-with-pretty-format)))

(use-package! consult-notes
  :after denote
  :init
  (consult-notes-denote-mode)
  :commands (consult-notes)
  :config
  (setq consult-notes-denote-display-id nil)
  ;; Override default Denote source formatting to show keywords before titles.
  (setq consult-notes-denote--source
        (list :name (propertize "Denote notes" 'face 'consult-notes-sep)
              :narrow ?d
              :category consult-notes-category
              :annotate consult-notes-denote-annotate-function
              :items
              (lambda ()
                (let* ((window-width (window-width (minibuffer-window)))
                       (kw-width consult-notes-denote-display-keywords-width)
                       (title-column (+ kw-width 2))
                       (dir-min-width 20)
                       (entries
                        (mapcar
                         (lambda (f)
                           (let* ((id (denote-retrieve-filename-identifier f))
                                  (title-1
                                   (or (denote-retrieve-title-value
                                        f (denote-filetype-heuristics f))
                                       (denote-retrieve-filename-title f)))
                                  (title
                                   (if consult-notes-denote-display-id
                                       (concat id " " title-1)
                                     title-1))
                                  (keywords (denote-extract-keywords-from-path f)))
                             (list :path f :title title :keywords keywords)))
                         (funcall consult-notes-denote-files-function)))
                       (max-title-width
                        (apply #'max 0 (mapcar (lambda (e) (string-width (plist-get e :title))) entries)))
                       (title-width
                        (if consult-notes-denote-dir
                            ;; Keep a visible, aligned dir column within minibuffer width.
                            (max 12 (min max-title-width
                                         (- window-width title-column dir-min-width 2)))
                          (max 12 (min max-title-width
                                       (- window-width title-column 2)))))
                       (dir-column (+ title-column title-width 2)))
                  (mapcar
                   (lambda (entry)
                     (let* ((path (plist-get entry :path))
                            (keywords (plist-get entry :keywords))
                            (kw-str
                             (string-trim
                              (funcall consult-notes-denote-display-keywords-function
                                       keywords)))
                            (kw-cell-base
                             (truncate-string-to-width kw-str kw-width nil nil ""))
                            (kw-pad (max 0 (- kw-width (string-width kw-cell-base))))
                            (kw-cell (concat kw-cell-base (make-string kw-pad ? )))
                            (title-cell
                             (truncate-string-to-width
                              (plist-get entry :title) title-width nil nil ""))
                            (dirs
                             (directory-file-name
                              (file-relative-name
                               (file-name-directory path) denote-directory)))
                            (candidate
                             (concat
                              (propertize kw-cell 'face 'consult-notes-name)
                              "  "
                              title-cell
                              (when consult-notes-denote-dir
                                (concat
                                 (propertize
                                  " "
                                  'display
                                  `(space :align-to (+ left ,dir-column)))
                                 (propertize
                                  (funcall consult-notes-denote-display-dir-function dirs)
                                  'face 'consult-notes-name))))))
                       (propertize candidate
                                   'denote-path path
                                   'denote-keywords keywords)))
                   entries)))
              :state #'consult-notes-denote--state
              :action (lambda (cand)
                        (if-let ((path (get-text-property 0 'denote-path cand)))
                            (find-file path)
                          (user-error "No Denote path found for candidate")))
              :new #'consult-notes-denote--new-note))
  )


(use-package! jupyter
  :demand t
  :after (:all org python)

  :config
  (setq jupyter-eval-short-result-max-lines 5)
  (map! :map jupyter-repl-mode-map
        :i "C-k" #'jupyter-repl-history-previous
        :i "C-j" #'jupyter-repl-history-next
        :nvi "C-e" #'evil-end-of-line-or-visual-line
        :i "<up>" #'jupyter-repl-history-previous
        :n  "gj" #'evil-avy-goto-char-timer
        :i "<down>" #'jupyter-repl-history-next
        ;; Kernel disconnection commands
        :nvi "C-c C-d" #'jupyter-repl-disconnect-kernel
        :nvi "C-c C-S-d" #'jupyter-repl-force-disconnect-kernel
        :nvi "C-c C-s" #'jupyter-repl-connection-status)

  (map! :map (python-mode-map python-ts-mode-map ess-mode-map)
        ;; Jupyter Integration
        :nv "C-<return>" #'jupyter-eval-line-or-region
        :nv "S-<return>" #'jupyter-eval-line-or-region ; Alternative
        ;; :v "C-c <return>" #'python-shell-send-region ; Send region to shell (standard python.el)
        :localleader
        :n :desc "eval buffer" "eb" #'jupyter-eval-buffer
        :n :desc "eval function" "ed" #'jupyter-eval-defun
        :nv :desc "eval region" "er" #'jupyter-eval-region)


  (add-hook! 'jupyter-repl-mode-hook #'electric-pair-mode))

;; code cells
(use-package! code-cells
  :load-path "~/.config/doom/external-lisp/code-cells.el/"
  :config

  (defun my/insert-code-cell()
    (interactive)
    (evil-open-above 1)
    (insert "#%%")
    )

  (defun my/insert-markdown-cell()
    (interactive)
    (evil-open-above 1)
    (insert "#%% [markdown]")
    )

  (defun my/delete-code-cell()
    (interactive)
    (code-cells-mark-cell)
    (let ((beg (region-beginning))
          (end (region-end)))
      (evil-delete beg end)))

  (defun my/code-cell-to-md()
    (interactive)
    (beginning-of-line)
    (when (not (looking-at "^#%%.*"))
      (code-cells-backward-cell))
    (evil-append-line 1)
    (insert " [markdown]")
    (evil-force-normal-state)
    )

  (defun my/md-cell-to-code()
    (interactive)
    (beginning-of-line)
    (when (not (looking-at "^#%%.*"))
      (code-cells-backward-cell))
    (evil-end-of-visual-line)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "\\[markdown\\]" (line-end-position) t)
        (replace-match ""))))

  (defun my/eval-code-cell-and-next()
    (interactive)
    (call-interactively #'code-cells-eval)
    (call-interactively #'code-cells-forward-cell)
    (evil-force-normal-state)
    )

  (defun my/code-cells-eval-text-object()
    (interactive)
    (when (evil-insert-state-p)
      (evil-normal-state))
    (unless (evil-visual-state-p)
      (evil-visual-char))
    (call-interactively #'evil-inner-symbol)
    (evil-visual-expand-region)
    (code-cells-eval (region-beginning) (region-end))
    )

  (defun my/code-cells-eval-line ()
    (interactive)
    ;; Get the beginning and end positions of the current line
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      ;; Call the eval function with the positions
      (code-cells-eval beg end)))

  (defun my/tag-cell ()
    (interactive)
    (beginning-of-line)
    (when (not (looking-at "^#%%.*"))
      (code-cells-backward-cell))
    (let* ((tag-regex "# %%.*tags=\\[\\(.*?\\)\\]")
           (all-tags (save-excursion
                       (goto-char (point-min))
                       (let (tags)
                         (while (re-search-forward tag-regex nil t)
                           (let ((tag-str (match-string 1)))
                             (setq tags (append tags (split-string tag-str ", " t "\"")))))
                         tags)))
           (tag (completing-read "Enter tag: " (delete-dups all-tags))))
      (save-excursion
        (end-of-line)
        (if (looking-back "# %% tags=\\[\\(.*?\\)\\]" (line-beginning-position))
            (progn
              (backward-char 1)
              (unless (looking-back "\\[" (1- (point)))
                (insert ", "))
              (insert (format "\"%s\"" tag)))
          (insert (format " tags=[\"%s\"]" tag))))))

  (defalias 'my-code-cells-eval-line-normal
    (kmacro "C-g V C-c C-c C-g"))
  (defalias 'my-code-cells-eval-line-insert
    (kmacro "C-g V C-c C-c C-g i"))


  (map! :map code-cells-mode-map
        :nvi "C-c C-k" #'code-cells-backward-cell
        :nvi "C-c C-j" #'code-cells-forward-cell
        :nvi "C-c C-<up>" #'code-cells-move-cell-up
        :nvi "C-c C-<down>" #'code-cells-move-cell-down
        :nvi "C-c E" #'code-cells-eval-above
        :nvi "C-c C-c" #'code-cells-eval
        :nvi "C-c C-\." #'code-cells-eval
        :v "C-<return>" #'code-cells-eval
        :ni "C-<return>" #'my/code-cells-eval-line
        ;; :n "C-<return>" #'my-code-cells-eval-line-normal
        ;; :i "C-<return>" #'my-code-cells-eval-line-insert
        :nvi "S-<return>" #'my/eval-code-cell-and-next
        :nvi "C-S-<return>" #'my/code-cells-eval-text-object
        :nvi "C-c C-o" #'my/code-cells-eval-text-object
        :nvi "C-c i" #'my/insert-code-cell
        :i "C-c i" #'my/insert-code-cell

        :nvi "C-c I" #'my/insert-markdown-cell
        :nvi "C-c k" #'jupyter-repl-pop-to-buffer
        :nvi "C-c m" #'my/code-cell-to-md
        :nvi "C-c M" #'my/md-cell-to-code
        :nvi "C-c d" #'my/delete-code-cell
        :nvi "C-c t" #'my/tag-cell
        :nvi "C-c C-v" (lambda () (interactive) (code-cells-mark-cell) (exchange-point-and-mark)))

  (map! :map python-mode-map
        :nvi "C-c C-o" #'jupyter-eval-line-or-region
        :nvi "C-c k" #'jupyter-repl-pop-to-buffer
        )
  )

(use-package! consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (setq! consult-dir-sources '(consult-dir--source-default consult-dir--source-bookmark consult-dir--source-project consult-dir--source-recentf consult-dir--source-tramp-local consult-dir--source-tramp-ssh))
  )

(after! consult
  (consult-customize
   consult-buffer consult-buffer-other-window consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref consult-theme
   ;; consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key "C-.")

  ;; The :init configuration is always executed (Not lazy)
  :init

  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)


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

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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
  (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --ignore-case --no-heading --line-number . -e ARG OPTS")

  (use-package! consult-flycheck
    :bind (:map flycheck-command-map
                ("!" . consult-flycheck)))


  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  )

(use-package! org
  :init
  (setq my-org-refile-maxlevel 1)
  :config
  (setq
   bidi-paragraph-direction nil
   org-id-link-to-org-use-id 'create-if-interactive
   org-id-method 'ts
   org-outline-path-complete-in-steps nil
   org-goto-interface 'outline-path-completion
   org-cycle-separator-lines 2
   ;; org-image-actual-width nil
   org-export-with-toc 'nil
   ;; org-image-actual-width (list 550)
   org-image-actual-width (list 150)
   org-ellipsis "…"
   ;; ➡, ⚡, ▼, ↴, ∞, ⬎, ⤷, ⤵, …
   org-deadline-warning-days 7
   org-agenda-breadcrumbs-separator " ❱ "
   org-odd-levels-only  nil
   org-startup-with-inline-images t
   org-hide-block-startup t
   org-startup-folded "fold"
   org-hide-emphasis-markers t
   org-list-indent-offset 2
   org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
   org-list-demote-modify-bullet
   '(("+" . "*") ("-" . "+") ("*" . "-"))
   org-capture-papers-file "~/notes/20230402T133604--interesting-papers__thesis.org"
   org-capture-microdosing-journal-file "~/notes/20230523T162209--microdosing-journal__journal.org"

   org-refile-targets '(
                        ("~/Documents/notes/20240219T111038--analysis-log-vaccine-response__work.org" :level . 1)
                        ("~/Documents/notes/20240417T172124--analysis-log-spatial-pipeline__work.org" :level . 1)
                        (nil . (:maxlevel . 9)) ;; current buffer)

                        org-todo-keywords '(
                                            (sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "SKIM(s)" "READ(r)" "NOTE(N)" "RESOURCE(R)" "|" "DONE(d)")
                                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))

                        org-agenda-block-separator " "

                        org-agenda-custom-commands
                        '(
                          ("o" "my agenda"
                           (
                            (todo "NEXT" (
                                          (org-agenda-overriding-header "\n⚡ Next up:\n")
                                          (org-agenda-remove-tags t)
                                          (org-agenda-prefix-format (concat "  %-2i  %t%s" ))
                                          (org-agenda-todo-keyword-format "")))

                            (todo "HOLD" (
                                          (org-agenda-overriding-header "\n⚡ Stuck tasks (on HOLD):\n")
                                          (org-agenda-remove-tags t)
                                          (org-agenda-prefix-format (concat "  %-2i  %t%s" ))
                                          (org-agenda-todo-keyword-format "")))

                            (todo "TODO|HOLD" (
                                               (org-agenda-overriding-header "\n⚡ All queued tasks:\n")
                                               (org-agenda-remove-tags t)
                                               (org-agenda-prefix-format (concat "  %-2i  %t%s" ))
                                               ;; (org-agenda-prefix-format (concat "  %-2i %-13b" ))
                                               (org-agenda-todo-keyword-format "")))

                            (todo "READ" (
                                          (org-agenda-overriding-header "\n⚡ Reading list:\n")
                                          (org-agenda-remove-tags t)
                                          (org-agenda-prefix-format (concat "  %-2i  %t%s" ))
                                          (org-agenda-todo-keyword-format "")))

                            (todo "RESOURCE" (
                                              (org-agenda-overriding-header "\n⚡ Resource list:\n")
                                              (org-agenda-remove-tags t)
                                              (org-agenda-prefix-format (concat "  %-2i  %t%s" ))
                                              (org-agenda-todo-keyword-format "")))
                            (agenda "" (
                                        (org-agenda-overriding-header "⚡ Schedule:\n")
                                        (org-agenda-start-day "+0d")
                                        (org-agenda-span 5)
                                        (org-agenda-remove-tags t)
                                        (org-agenda-prefix-format   (concat "  %-3i  %t%s"))
                                        (org-agenda-current-time-string "⟸ now")
                                        (org-agenda-scheduled-leaders '("" ""))
                                        (org-agenda-time-grid (quote ((daily today remove-match)
                                                                      (0900 1200 1800 2100)
                                                                      "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈")))))
                            )
                           )

                          ("l" "literature"
                           (
                            (todo "SKIM" (
                                          (org-agenda-overriding-header "\n⚡ Next to skim:\n")
                                          (org-agenda-remove-tags t)
                                          (org-agenda-prefix-format (concat " %b  %-2i  %t%s" ))
                                          ;; (org-agenda-prefix-format "  %?-12t% s")
                                          (org-agenda-todo-keyword-format "")))
                            (todo "READ" (
                                          (org-agenda-overriding-header "\n⚡ Next to read:\n")
                                          (org-agenda-remove-tags t)
                                          ;; (org-agenda-prefix-format "  %?-12t% s")
                                          (org-agenda-prefix-format (concat " %b  %-2i  %t%s" ))
                                          (org-agenda-todo-keyword-format "")))
                            )
                           )


                          )

                        org-capture-templates '(
                                                ("t" "Todo"
                                                 entry
                                                 (file org-capture-inbox-file )
                                                 "* TODO %? %i")

                                                ("T" "Todo with link"
                                                 entry
                                                 (file+headline org-capture-inbox-file "Tasks")
                                                 "* TODO %? %i\n** source: %l")

                                                ("m" "Microdosing journal entry"
                                                 entry
                                                 (file org-capture-microdosing-journal-file)
                                                 ;; (concat "* " (format-time-string "%Y%m%d")))
                                                 "* %^{תאריך}, %^{יום בשבוע}, %^{שעה}")

                                                ("p" "Paper ref to read "
                                                 entry
                                                 (file org-capture-papers-file)
                                                 "* SKIM %^{title?|%i}
- link/cite: %^{link/DOI?}
- type of paper: %^{type?|study|review|theoretical|theory & study}
- why read it?
  %^{why read it?}
- figures:

%^{a short summary?}"

                                                 :empty-lines-after 1)

                                                ;;                          ("n" "Note"
                                                ;;                           entry
                                                ;;                           (file+headline org-capture-writing-inbox-file "Notes")
                                                ;;                           "* NOTE %? \n")

                                                ;;                          ("j" "Journal entry" entry (function org-journal-find-location)
                                                ;;                           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
                                                ;;
                                                ("i" "Interesting things"
                                                 entry
                                                 (file+headline org-capture-someday-file "To read/watch")
                                                 "** %? :bucket_list:\n")
                                                )
                        ))

  (setq org-format-latex-options
        (quote
         (:foreground default :background default :scale 2.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
          ("begin" "$1" "$" "$$" "\\(" "\\["))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) ;; Other languages
     (shell . t)
     ;; Python & Jupyter
     (jupyter . t)
     (python . t)
     (ipython . t)
     ))

  (map! :map org-mode-map
        :nvi "C-c C-k" #'org-previous-visible-heading
        :nvi "C-c C-j" #'org-next-visible-heading
        :ni "C-c C-c" #'org-babel-execute-maybe
        :n "<return>" #'org-open-at-point)
  )

(after! citar
  (defun my/get-bib-file-list ()
    "Get the list of all the bib files containing my bib database."
    (mapcan (lambda (dir) (directory-files dir t "\\.bib\\'"))
            '("~/Documents/bibliography")))
  (setq!
   citar-bibliography (my/get-bib-file-list)
   org-cite-global-bibliography (my/get-bib-file-list)
   citar-at-point-function 'embark-act
   citar-file-note-org-include '(org-id)
   citar-notes-paths (list denote-directory)
   citar-citeproc-csl-styles-dir "~/notes/export-csl-style"
   citar-citeproc-csl-style "apa.csl"
   citar-library-paths (list "~/Documents/bibliography")
   ;; citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
   ;;                   (suffix . "         ${tags keywords keywords:*}   ${=key= id:15}    ${=type=:12}")
   ;;                   (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
   ;;                   (note . "")))
   )
  ;; open PDFs with system viewer instead of pdf-tools
  (add-to-list 'citar-file-open-functions (cons "pdf" #'citar-file-open-external))

  (setq citar-symbols
        `((file . (,(propertize " " 'face 'error) .
                   ,(propertize " " 'face 'citar-icon-dim)))
          (note . (,(propertize " " 'face 'success) .
                   ,(propertize " " 'face 'citar-icon-dim)))
          (link . (,(propertize " " 'face 'link) .
                   ,(propertize " " 'face 'citar-icon-dim)))))

  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces))

(setq denote-templates
      '((biblio . "%^{title}\n\n* Abstract\n\n* Review\n")
        (plain . nil))
      citar-denote-template 'biblio)

(use-package! citar-denote
  :custom
  (citar-open-always-create-notes nil)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))

(after! lsp-pyright
  ;; Configuration specific to lsp-pyright can go here if needed in the future
  ;; e.g., (setq lsp-pyright-some-option t)
  (setq lsp-pyright-use-library-code-for-types t)
  )

(add-hook! python-mode #'eglot-ensure #'code-cells-mode)

(setq ispell-personal-dictionary-en   "~/Documents/dictionaries/personal.en")
(setq ispell-personal-dictionary-heb  "~/Documents/dictionaries/personal.heb")
;; NOTE: The dictionary paths below are hardcoded for Windows.
;; Adjust these paths if using Linux or macOS.
(setq ispell-local-dictionary-alist '(("en_us"
                                       "[[:alpha:]]"
                                       "[^[:alpha:]]"
                                       "[']"
                                       t
                                       ("-d" "en_us" "-p"   "c:\\users\\jonathan\\programs\\hunspell\\share\\hunspell\\personal.en")
                                       nil
                                       iso-8859-1)

                                      ("hebrew"
                                       "[[:alpha:]]"
                                       "[^[:alpha:]]"
                                       "[']"
                                       t
                                       ("-d" "hebrew" "-p"   "c:\\users\\jonathan\\programs\\hunspell\\share\\hunspell\\personal.heb")
                                       nil
                                       iso-8859-1)))

(setq ispell-dictionary "en_us") ; default dictionary to use
;; (add-to-list 'exec-path "c:\\users\\jonathan\\programs\\hunspell\\bin")

(setq ispell-program-name (locate-file "hunspell"
                                       exec-path exec-suffixes 'file-executable-p))

(unless (file-exists-p ispell-personal-dictionary-en)
  (write-region "" nil ispell-personal-dictionary-en nil 0))
(unless (file-exists-p ispell-personal-dictionary-heb)
  (write-region "" nil ispell-personal-dictionary-heb nil 0))


(defun init-spellchecker()
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (setq ispell-dictionary "en_US") ;; ,hebrew") ; default dictionary to use
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US")) ;; ,hebrew"))

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

  (defvar tabspaces-command-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C") 'tabspaces-clear-buffers)
      (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
      (define-key map (kbd "d") 'tabspaces-close-workspac)
      (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
      (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
      (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
      (define-key map (kbd "r") 'tabspaces-remove-selected-buffer)
      (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
      (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
      map)
    "keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")

  ;; filter buffers for consult-buffer

  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
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

(after! conda
  (setq! conda-anaconda-home (expand-file-name "~/miniforge3/bin/conda"))
  (setq-default mode-line-format (cons  '(:exec conda-env-current-name) mode-line-format))
  )

(use-package! captain
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq captain-predicate (lambda () (nth 8 (syntax-ppss (point)))))))


  (add-hook 'text-mode-hook
            (lambda ()
              (setq captain-predicate (lambda () t))))


  (add-hook
   'org-mode-hook
   (lambda ()
     (setq captain-predicate
           (lambda () (not (org-in-src-block-p))))))
  (global-captain-mode)
  )

(use-package! tmr
  :init
  (setq tmr-sound-file "~/documents/sounds/tibetian-bowl-1.wav")
  )

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

(use-package! gptel
  :init
  :config
  (setq
   gptel-model 'gemini:gemini-flash-latest
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (getenv "GEMINI_API_KEY")
                   :stream t))

  )
(setq gptel-api-key (getenv "OPENAI_API_KEY")
      gptel-use-curl 'nil
      gptel-stream nil
      gptel-default-mode 'org-mode)

(gptel-make-anthropic "claude"          ;any name you want
  :stream t                             ;streaming responses
  :key(getenv "ANTHROPIC_API_KEY"))

(gptel-make-ollama "ollama-local"
  :host "localhost:11434"
  :stream t
  ;; :endpoint "/api/generate"
  :models '(phi4:latest qwen2.5-coder:32b deepseek-r1:32b))

(setq gptel-org-branching-context t)

(setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
(setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")


(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "images"
        org-download-heading-lvl 0
        org-download-timestamp "%y%m%d-%h%m%s_"
        org-image-actual-width nil
        org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'"
        org-download-image-org-width 650)
  :bind
  ("C-M-p" . org-download-screenshot))

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "C-l") #'windmove-right))

(after! (:and treemacs ace-window)
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))
  (setq aw-scope 'global)
  )

(use-package! highlight-thing
  :config
  (global-highlight-thing-mode)

  (setq!
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


(use-package! dwim-shell-command
  :config
  (defun my/dwim-shell-command-convert-image-to-jpg ()
    "convert all marked images to jpg(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "convert to jpg"
     "convert -verbose '<<f>>' '<<fne>>.jpg'"
     :utils "convert"))

  (defun my/dwim-shell-command-convert-audio-to-mp3 ()
    "convert all marked audio to mp3(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "convert to mp3"
     "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
     :utils "ffmpeg"))

  (defun my/dwim-shell-commands-files-combined-size ()
    "get files combined file size."
    (interactive)
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
  )


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

(after! treemacs
  (setq! treemacs-sorting 'mod-time-desc)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "y n") #'my/treemacs-copy-name-at-point)
  )

(use-package! spacious-padding
  :config
  (spacious-padding-mode 1))


(use-package! smerge-mode)

(defun my/search-replace ()
  (interactive)
  (if (use-region-p) (my/search-replace-in-region) (evil-ex "%s/"))
  )

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

(use-package! elysium
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package! copilot-chat
  :bind (:map global-map
              ("C-c C-p" . copilot-chat-yank)
              )
  )


(after! python
  (require 'eglot)
  (setq python-check-command "ruff check"))
;; Run jupytext on save for Python files
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook #'my/run-jupytext-on-save nil t))))

(after! format
  (setq +format-on-save-enabled-modes '(prog-mode))
  (set-formatter! 'ruff-format
    '("ruff" "format" "--stdin-filename" "%file" "-")
    :modes '(python-mode python-ts-mode)))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package! drag-stuff
  :defer t
  :init
  (map!
   "<M-up>"    #'drag-stuff-up
   "<M-down>"  #'drag-stuff-down
   "<M-k>"    #'drag-stuff-up
   "<M-j>"  #'drag-stuff-down))

(with-eval-after-load 'eglot
  (setq eglot-workspace-configuration
        '((:pylsp .
           (:plugins
            (:pycodestyle (:enabled :json-false)
             :flake8      (:enabled :json-false)
             :pylint      (:enabled :json-false)
             :pyflakes    (:enabled :json-false)
             :jedi_completion (:enabled t))))

          (:pyright .
                    (:typeCheckingMode "off"
                     :disableLanguageServices :json-true
                     :disableOrganizeImports :json-false)))))

(use-package! vterm
  :config
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
        :nvi "C-u" #'vterm-copy-mode
        )
  )

;; (use-package! corfu-candidate-overlay
;;   :after corfu
;;   :config
;;   ;; enable corfu-candidate-overlay mode globally
;;   ;; this relies on having corfu-auto set to nil
;;   (corfu-candidate-overlay-mode +1)
;;   ;; bind Ctrl + TAB to trigger the completion popup of corfu
;;   (global-set-key (kbd "S-<tab>") 'completion-at-point)
;;   ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;;   ;; (keybing <iso-lefttab> may not work for your keyboard model)
;;   (global-set-key (kbd "<tab>") 'corfu-candidate-overlay-complete-at-point))


(use-package! magit-gptcommit
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider (make-llm-gemini :key (getenv "GEMINI_API_KEY") :model "gemini-2.0-flash"))

  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

(use-package! embark
  :config
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

;; (use-package! magit-todos
;;   :after magit
;;   :config (magit-todos-mode 1))

;; `ai-code' provides a unified interface for various AI coding assistants.
;; It allows switching between backends like Claude Code and provides
;; integration with Magit for AI-generated commit messages and code analysis.
(use-package! ai-code
  :ensure nil
  :config
  (ai-code-set-backend  'claude-code) ;; use claude-code-ide as backend
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  (setq claude-code-terminal-backend 'vterm)
  ;; (setq claude-code-terminal-backend 'eat)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

;; install claude-code.el, using :depth 1 to reduce download size:
(use-package! inheritenv)
(use-package! claude-code
  :bind-keymap
  ("C-c C" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
:config
(add-hook 'claude-code-process-environment-functions
          (lambda (claude-buffer-name directory)
            '("ANTHROPIC_BASE_URL=http://0.0.0.0:4000"
              "ANTHROPIC_API_KEY=sk-ant-dummy")))
(claude-code-mode)

;; for slash commands popup
(use-package! popup :ensure t)
(use-package! gemini-cli
  :bind-keymap
  ("C-c c" . gemini-cli-command-map)
  :config
  (gemini-cli-mode))

(use-package! eat)
(require 'shell-maker)

(require 'acp)
(require 'agent-shell)

(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))
;; With string
(setq agent-shell-google-authentication
      (agent-shell-google-make-authentication :api-key (getenv "GEMINI_API_KEY")))
(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))

(use-package! logview)
(use-package! aider
  :config
  (setq aider-args '("--model" "gemini/gemini-3-flash-preview"))
  (require 'aider-doom))

(after! jupyter
  (load! "jupyter-timer-fix"))
