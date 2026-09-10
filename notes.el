;;; $DOOMDIR/notes.el -*- lexical-binding: t; -*-
;; denote, consult-notes, citar, citar-denote, denote-markdown, denote-silo

(use-package! denote
  :config
  ;; Remember to check the doc strings of those variables.
  (setopt denote-directory (expand-file-name "~/Documents/notes")
          denote-known-keywords '("emacs" "thesis")
          denote-infer-keywords t
          denote-sort-keywords t
          denote-prompts '(title keywords)
          denote-excluded-directories-regexp nil
          denote-excluded-keywords-regexp nil)

  (denote-rename-buffer-mode 1)
  (setopt denote-sort-dired-default-sort-component 'last-modified
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
                        (if-let* ((path (get-text-property 0 'denote-path cand)))
                            (find-file path)
                          (user-error "No Denote path found for candidate")))
              :new #'consult-notes-denote--new-note))
  )

(after! (:and citar denote)
  (defun my/get-bib-file-list ()
    "Get the list of all the bib files containing my bib database."
    (mapcan (lambda (dir) (directory-files dir t "\\.bib\\'"))
            '("~/Documents/bibliography")))
  (setopt
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
  (add-to-list 'citar-file-open-functions (cons "pdf" #'citar-file-open-external)))

(setq denote-templates
      '((biblio . "%^{title}\n\n* Abstract\n\n* Review\n")
        (plain . nil))
      citar-denote-template 'biblio)

(use-package! citar-denote
  :custom
  (citar-open-always-create-notes nil)
  :config
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

(after! ispell
  (setq ispell-dictionary "en_US"))

(use-package! denote-silo
  ;; Bind these commands to key bindings of your choice.
  :commands ( denote-silo-create-note
              denote-silo-open-or-create
              denote-silo-select-silo-then-command
              denote-silo-dired
              denote-silo-cd )
  :config
  ;; Add your silos to this list.  By default, it only includes the
  ;; value of the variable `denote-directory'.
  (setq denote-silo-directories
        (list denote-directory
              "~/Documents/silverbullet-notes/")))

(defun my/denote-markdown-convert-links-in-md-dir (dir &optional absolute)
    "Convert Denote links in all Markdown files under DIR.

For each `*.md' file, iterate all Denote link positions and call
`denote-markdown-convert-links-to-file-paths' at each position.
With optional ABSOLUTE, convert to absolute paths."
    (interactive "DMarkdown directory: \nP")
    (require 'denote-markdown)
    (let* ((root (expand-file-name dir))
           (files (directory-files-recursively root "\\.md\\'"))
           (changed-files 0))
      (dolist (file files)
        (let* ((existing (get-file-buffer file))
               (buf (or existing (find-file-noselect file)))
               (modified nil))
          (with-current-buffer buf
            (unless (derived-mode-p 'markdown-mode)
              (set-auto-mode t))
            (when (derived-mode-p 'markdown-mode)
              (let (positions)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward (denote-markdown--get-regexp 'denote) nil t)
                    (push (match-beginning 0) positions)))
                (dolist (pos (nreverse positions))
                  (save-excursion
                    (goto-char pos)
                    (denote-markdown-convert-links-to-file-paths absolute)))
                (setq modified (buffer-modified-p))
                (when modified
                  (save-buffer)))))
          (unless existing
            (kill-buffer buf))
          (when modified
            (setq changed-files (1+ changed-files)))))
      (message "Processed %d markdown files; changed %d"
               (length files) changed-files)))
