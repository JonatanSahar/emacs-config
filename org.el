;;; $DOOMDIR/org.el -*- lexical-binding: t; -*-
;; org, org-download, capture/agenda/refile, org helpers (image clipboard, move-line, odt)

(defvar org-capture-inbox-file (expand-file-name "~/Documents/notes/inbox.org")
  "Org file receiving captured TODO items.")
(defvar org-capture-someday-file (expand-file-name "~/Documents/notes/someday.org")
  "Org file receiving captured someday/maybe items.")
(defvar org-capture-papers-file
  (expand-file-name "~/Documents/notes/20230402T133604--interesting-papers__thesis.org"))
(defvar org-capture-microdosing-journal-file
  (expand-file-name "~/Documents/notes/20230523T162209--microdosing-journal__journal.org"))
(setq my-org-refile-maxlevel 1)

(after! org
        (setq
         org-id-link-to-org-use-id 'create-if-interactive
         org-id-method 'ts
         org-outline-path-complete-in-steps nil
         org-goto-interface 'outline-path-completion
         org-cycle-separator-lines 2
         org-export-with-toc 'nil
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
         org-refile-targets '(
                              ("~/Documents/notes/20240219T111038--analysis-log-vaccine-response__work.org" :level . 1)
                              ("~/Documents/notes/20240417T172124--analysis-log-spatial-pipeline__work.org" :level . 1)
                              (nil . (:maxlevel . 9))) ;; current buffer

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
                                  "** %? :bucket_list:\n")))

        (setq org-format-latex-options
              (quote
               (:foreground default :background default :scale 2.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                            ("begin" "$1" "$" "$$" "\\(" "\\["))))

        (map! :map org-mode-map
              :nvi "C-c C-k" #'org-previous-visible-heading
              :nvi "C-c C-j" #'org-next-visible-heading
              :ni "C-c C-c" #'org-babel-execute-maybe
              :n "<return>" #'org-open-at-point)
        )

(use-package! org-download
  :after org
  :config
  (setq org-download-method 'directory
        org-download-image-dir "images"
        org-download-heading-lvl 0
        org-download-timestamp "%Y%m%d-%H%M%S_"
        org-image-actual-width nil
        org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'"
        org-download-image-org-width 650)
  :bind
  ("C-M-p" . org-download-screenshot))

(setq org-odt-preferred-output-format "docx")

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
