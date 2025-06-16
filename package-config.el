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
  )


(use-package! jupyter
  :demand t
  :after (:all org python)

  :config
  (map! :map jupyter-repl-mode-map
        :i "C-k" #'jupyter-repl-history-previous
        :i "C-j" #'jupyter-repl-history-next
        :nvi "C-e" #'evil-end-of-line-or-visual-line
        :i "<up>" #'jupyter-repl-history-previous
        :i "<down>" #'jupyter-repl-history-next)

  (map! :map (python-mode-map python-ts-mode-map)
        ;; Jupyter Integration
        :nv "C-<return>" #'jupyter-eval-line-or-region
        :nv "S-<return>" #'jupyter-eval-line-or-region ; Alternative
        :v "C-c <return>" #'python-shell-send-region ; Send region to shell (standard python.el)
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
    (evil-normal-state)
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
    (call-interactively #'windmove-up)
    (call-interactively #'code-cells-forward-cell)
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
        :nvi "C-c C-o" #'jupyter-eval-line-or-region
        :nvi "C-c i" #'my/insert-code-cell
        :i "C-c i" #'my/insert-code-cell

        :nvi "C-c I" #'my/insert-markdown-cell
        :nvi "C-c k" #'jupyter-repl-pop-to-buffer
        :nvi "C-c m" #'my/code-cell-to-md
        :nvi "C-c M" #'my/md-cell-to-code
        :nvi "C-c d" #'my/delete-code-cell
        :nvi "C-c t" #'my/tag-cell
        :nvi "C-c C-v" #'code-cells-mark-cell)

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
   citar-file-note-org-include '(org-id org-roam-ref)
   citar-notes-paths (list denote-directory)
   citar-citeproc-csl-styles-dir "~/notes/export-csl-style"
   citar-citeproc-csl-style "apa.csl"
   citar-library-paths (list "~/Documents/bibliography")
   ;; (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))
   citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                     (suffix . "         ${tags keywords keywords:*}   ${=key= id:15}    ${=type=:12}")
                     (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                     (note . "")))

  ;; open PDFs with system viewer instead of pdf-tools
  (add-to-list 'citar-file-open-functions (cons "pdf" #'citar-file-open-external))

  (setq citar-symbols
        `((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                   ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
          (note . (,(all-the-icons-icon-for-file "foo.txt") .
                   ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
          (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                 ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))
  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface citar-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces))

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
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)

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
  (setq gptel-api-key (getenv "OPENAI_API_KEY")
        gptel-use-curl 'nil
        gptel-stream nil
        gptel-default-mode 'org-mode)

  (setq! gptel-directives
         '(
           (default   . "you are a large language model and a helpful assistant. answer the user’s questions accurately and concisely.")
           (programming . "you are a language model with expert programming knowledge. provide clean, correct code solutions with minimal commentary")
           (writing   . "you are a skilled writing assistant. help improve text for clarity, style, and correctness. respond succinctly and to the point.")
           (chat      . "you are a friendly conversational partner and knowledgeable assistant. engage naturally and helpfully in conversation, keeping your responses concise.")

           (refactor  . "your primary task is to produce code for in-place refactoring with modifications based on the included request. you are to do this with precise line number ranges. follow these instructions meticulously:\n1. carefully analyze the original code, paying close attention to its structure and line numbers. line numbers start from 1 and include all lines, even empty ones.\n2. when suggesting modifications include all of the code supplied, with the relevant changes applied.\nb. provide the exact code snippet to be replaced without enclosing it in a code block\n3. crucial guidelines for suggested code snippets:\n- only apply the change(s) suggested by the most recent assistant message (before your generation).\n- do not make any unrelated changes to the code.\n- produce a valid full rewrite of the entire original code without skipping any lines. do not be lazy!\n- do not arbitrarily delete pre-existing comments/empty lines.\n- do not omit large parts of the original file for no reason.\n- do not omit any needed changes from the requisite messages/code blocks.\n- if there is a clicked code block, bias towards just applying that (and applying other changes implied).\n- please keep your suggested code changes minimal, and do not include irrelevant lines in the code snippet.\n- maintain the same indentation in the returned code as in the source code\n4. final check:\n- review all suggestions, ensuring each line number is correct, especially the start_line and end_line.\n- confirm that no unrelated code is accidentally modified or deleted.\n- verify that the start_line and end_line correctly include all intended lines for replacement.\n- perform a final alignment check to ensure your line numbers haven't shifted, especially the start_line.\n- double-check that your line numbers align perfectly with the original code structure.\n- do not show the full content after these modifications.\nremember: accurate line numbers are critical. the range start_line to end_line must include all lines to be replaced, from the very first to the very last. double-check every range before finalizing your response, paying special attention to the start_line to ensure it hasn't shifted down. ensure that your line numbers perfectly match the original code structure without any overall shift.\nfinally, make sure to output only code, as text, without code block fences or anything, perfectly prepared for insertion into the original code.")

           (rewrite   . "you are a language model with expert programming knowledge. provide clean, correct code solutions with minimal commentary; output code and only code, do not add code fences e.g. python ''' ''' around the code.")
           ))
  (setq
   gptel-model 'ChatGPT:gpt-o4-mini
   )

(gptel-make-gemini "gemini" :key (getenv "GEMINI_API_KEY"):stream t)

(gptel-make-anthropic "claude"          ;any name you want
  :stream t                             ;streaming responses
  :key(getenv "ANTHROPIC_API_KEY"))

;; github models offers an openai compatible api
(gptel-make-openai "github models" ;any name you want
  :host "models.inference.ai.azure.com"
  :endpoint "/chat/completions?api-version=2024-05-01-preview"
  :stream t
  :key(getenv "GITHUB_API_KEY")
  :models '(deepseek-v3 codestral-2501 cohere-command-r-08-2024))

  (gptel-make-ollama "ollama"
    :host "localhost:11434"
    :stream t
    ;; :endpoint "/api/generate"
    :models '(phi4:latest qwen2.5-coder:32b deepseek-r1:32b))

  (gptel-make-ollama "ollama-local"
    :host "localhost:11434"
    :stream t
    ;; :endpoint "/api/generate"
    :models '(phi4:latest qwen2.5-coder:32b deepseek-r1:32b)))


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
        :i "C-;" #'my/copilot-tab-or-default
        :i "C-S-l" #'my/copilot-word-or-default))

(use-package! aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
   (setq
   ;; aidermacs-editor-model "anthropic/claude-3-5-sonnet-20241022"
   ;; aidermacs-architect-model "anthropic/claude-3-7-sonnet-20250219"
   aidermacs-editor-model "openai/o3-mini"
   aidermacs-architect-model "openai/o3"
   aidermacs-weak-model "openai/o4-mini"
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

(use-package! emigo
  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  ;; Encourage using OpenRouter with Deepseek
  (emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
  (emigo-base-url "https://openrouter.ai/api/v1")
  (emigo-api-key (getenv "OPENROUTER_API_KEY")))

(after! python
  :config
  (require 'eglot)
  (setq python-check-command "ruff check"))
  ;; Run jupytext on save for Python files
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook #'my/run-jupytext-on-save nil t))))

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

(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package! vterm
  :config
  (map! :map vterm-mode-map
        :i "C-j" (kbd "<down>")
        :i "C-k" (kbd "<up>")))

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

(defun copy-grep-results-as-kill (strings)
  (embark-copy-as-kill
   (mapcar (lambda (string)
             (substring string
                        (1+ (next-single-property-change
                             (1+ (next-single-property-change 0 'face string))
                             'face string))))
           strings)))

(add-to-list 'embark-multitarget-actions 'copy-grep-results-as-kill)

;;  ai! This doesn't work, as embark-define-keymap does not exist
(embark-define-keymap embark-consult-grep-map
  "Keymap for actions for consult-grep results."
  ("w" copy-grep-results-as-kill))

(setf (alist-get 'consult-grep embark-keymap-alist) 'embark-consult-grep-map)
