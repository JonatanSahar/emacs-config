;;; ~/.doom.d/my-functions.el -*- lexical-binding: t; -*-
(defun evil--mc-make-cursor-at-col (_startcol endcol orig-line)
  (move-to-column endcol)
  (unless (= (line-number-at-pos) orig-line)
    (evil-mc-make-cursor-here))
  )
    ;;; During visual selection point has +1 value
(defun my/evil-mc-make-vertical-cursors (beg end)
  (interctive (list (region-beginning) (- (region-end) 1)))
  (evil-exit-visual-state)
  (evil-mc-pause-cursors)
      ;;; Because `evil-mc-resume-cursors` produces a cursor,
      ;;; we have to skip a current line here to avoid having +1 cursor
  (apply-on-rectangle #'evil--mc-make-cursor-at-col
                      beg end (line-number-at-pos))
  (evil-mc-resume-cursors)
      ;;; Because `evil-mc-resume-cursors` produces a cursor, we need to place it on on the
      ;;; same column as the others
  (move-to-column (evil-mc-column-number end))
  ;; (evil-digit-argument-or-evil-beginning-of-line)
  ;; (evil-beginning-of-visual-line)
  )

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  ;; (goto-char (point-min)))
  )

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (push 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun my/org-search ()
  (interactive)
  (org-refile '(4)))

(defun my/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'agenda))

(defun my/paragraph-LTR ()
  (interactive)
  (setq bidi-paragraph-direction 'left-to-right))


(defun my/paragraph-RTL ()
  (interactive)
  (setq bidi-paragraph-direction 'right-to-left))


(defun my_refile(file headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos)))
  ;; (switch-to-buffer (current-buffer))
  )

(defun my_get_anki_cloze()
  (interactive)
  (when (looking-at ".*[.*].*$")
    (save-excursion
      (beginning-of-line)
      (setq question
            (buffer-substring-no-properties (point) (line-end-position)))
      (with-temp-buffer
        (insert "
** Question %^G
:PROPERTIES:
:ANKI_NOTE_TYPE: Cloze
:ANKI_DECK: 
:END:
*** Text
\t"
                question
                "
*** Extra
\t"
                )

        (search-backward "Question")
        (my_refile org-my-anki-file "Waiting for export")))))

(defun my_get_anki_question()
  (interactive)
  (when (looking-at ".*Q\..*$")
    (beginning-of-line)
    (search-forward "Q.")
    (setq question (string-trim-left (concat (org-get-heading) "\n" (org-get-entry)) "\t*\**Q\."))
    (search-forward "A.")
    (setq answer (string-trim-left (concat (org-get-heading) "\n" (org-get-entry)) "\t*\**A\."))
    (save-excursion
      (with-temp-buffer
        (insert "
** Question %^G
:PROPERTIES:
:ANKI_NOTE_TYPE: Basic
:ANKI_DECK: TheDeck
:END:
*** Front
\t"
                question
                "
*** Back
\t"
                answer)

        (search-backward "drill")
        (my_refile org-my-anki-file "Waiting for export")))))


(defun my/org-open-other-window ()
  "Jump to bookmark in another frame. See `bookmark-jump' for more."
  (interactive)
  (let ((org-link-frame-setup (acons 'file 'find-file-other-window org-link-frame-setup)))
    (org-open-at-point)))

(map! :prefix "g" :nv "F" nil)
(map! :prefix "g" :nv "F" 'my/org-open-other-window)

(defun ar/toggle-quote-wrap-all-in-region (beg end)
  "Toggle wrapping all items in region with double quotes."
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "no region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"" item)))
                              (split-string (buffer-substring beg end)))
                      " ")))
    (delete-region beg end)
    (insert replacement)))

(defun my/search-replace ()
  (interactive)
  (if (use-region-p) (my/search-replace-in-region) (evil-ex "%s/"))
  )

(defun my/search-replace-in-region ()
  (interactive)
  ;; (if (eq last-command 'evil-yank)

  (let ((evil-ex-initial-input "s/"))
    (call-interactively 'evil-ex))
  )

(evil-define-command my/evil-insert-char (count char)
  (interactive "<c><C>")
  (setq count (or count 1))
  (insert (make-string count char)))

(evil-define-command my/evil-append-char (count char)
  (interactive "<c><C>")
  (setq count (or count 1))
  (when (not (eolp))
    (forward-char))
  (insert (make-string count char)))

(defun my/visual-inside-org-header()
  (interactive)
  (evil-middle-of-visual-line)
  (evil-org-beginning-of-line)
  (evil-visual-char)
  (end-of-line))

(defun my/fix-windows-bib-file()
  (interactive)
  (let ((case-fold-search t)) ; or nil
    (goto-char (point-min))
    (while (search-forward "C\\\:" nil t)
      (replace-match "c:\/"))

    (goto-char (point-min))
    (while (search-forward "\\\\" nil t)
      (replace-match "\/"))

    (goto-char (point-min))
    (while (search-forward "G\\\:" nil t)
      (replace-match "g:\/")

      ))

  (save-buffer)
  )

(defun my/convert-windows-to-linux-paths()
  (interactive)
  (let ((case-fold-search t)) ; or nil
    (goto-char (point-min))
    (while (search-forward "C\\:" nil t)
      (replace-match "\/mnt\/c"))

    (goto-char (point-min))
    (while (search-forward "\\\\" nil t)
      (replace-match "\/"))
    (save-buffer) ;; repeat for other string pairs

    (goto-char (point-min))
    (while (search-forward "G\\:" nil t)
      (replace-match "\/mnt\/g")

      (save-buffer)
      )))

(defun my/get-buffer-name()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun my/org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(defun my/export-org-to-docx ()
  "Export the current org file as a docx via markdown."
  (interactive)
  (let* ((bibfile (expand-file-name (car (org-ref-find-bibliography))))
         ;; this is probably a full path
         (current-file (buffer-file-name))
         (basename (file-name-sans-extension current-file))
         (docx-file (concat basename ".docx")))
    (save-buffer)
    (when (file-exists-p docx-file) (delete-file docx-file))
    (shell-command (format
                    ;; "pandoc -s -S %s -o %s"
                    ;; (shell-quote-argument current-file) (shell-quote-argument docx-file)))
                    "pandoc -s -S --bibliography=%s %s -o %s"
                    (shell-quote-argument bibfile) (shell-quote-argument current-file) (shell-quote-argument docx-file)))
    ))

(defun helm-bibtex-format-pandoc-citation (keys)
  (concat "[" (mapconcat (lambda (key) (concat "@" key)) keys "; ") "]"))

;; inform helm-bibtex how to format the citation in org-mode
;; (setf (cdr (assoc 'org-mode bibtex-completion-format-citation-functions))
;;   'helm-bibtex-format-pandoc-citation)

(setq bibtex-completion-format-citation-functions
      '((org-mode      . helm-bibtex-format-pandoc-citation)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))
(setq bibtex-completion-additional-search-fields '(keywords))
(defun my/yank-org-headline ()
  (interactive)
  (let
      ((text (nth 4 (org-heading-components))))
    (kill-new text)))

;; (make-temp-name
;;  (concat (s-replace  "/mnt/c/Users/Jonathan/Google Drive/" "/home/jonathan/google_drive/" (buffer-file-name))
;;          "_"
;;          (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
(defun my/org-paste-image ()
  "Paste an image into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (let* ((target-file
          (concat
           (make-temp-name
            (concat "/home/jonathan/google_drive/.notes/associated_images/" (buffer-name)
                    "_"
                    (format-time-string "%Y%m%d_%H%M%S_"))) ".jpeg"))
         (wsl-path
          (concat (as-windows-path(file-name-directory target-file))
                  "\\"
                  (file-name-nondirectory target-file)))
         (ps-script
          (concat "(Get-Clipboard -Format image).Save('" wsl-path "')")))

    (powershell ps-script)

    (if (file-exists-p target-file)
        (progn (insert (concat "[[" target-file "]]"))
               (org-display-inline-images))
      (user-error wsl-path)
      (user-error
       "Error pasting the image, make sure you have an image in the clipboard!"))
    ))

(defun my/capture-screen ()
  (interactive)
  (let* ((ps-script "[system.windows.forms.sendkeys]::sendwait('{PRTSC}')"))
    (powershell ps-script)
    )
  )

(defun as-windows-path (unix-path)
  ;; "Takes a unix path and returns a matching WSL path
  ;; (e.g. \\wsl$\Ubuntu-20.04\tmp)"
  ;; substring removes the trailing \n
  (substring
   (shell-command-to-string
    (concat "wslpath -w " unix-path)) 0 -1))

(defun powershell (script)
  "executes the given script within a powershell and returns its return value"
  (call-process "powershell.exe" nil nil nil
                "-Command" (concat "& {" script "}")))

(defun my/fix-hebrew-hyphen()
  "Hide Org property drawer using text properties.
Based on the code shared at
https://org-roam.discourse.group/t/org-roam-major-redesign/1198/34."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "־" nil t)
      (replace-match "-"))
    ))

(defun my/org-hide-properties-display ()
  "Hide Org property drawer using text properties.
Based on the code shared at
https://org-roam.discourse.group/t/org-roam-major-redesign/1198/34."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:PROPERTIES:\n\\( *:.+?:.*\n\\)+ *:END:\n" nil t)
      (add-text-properties (match-beginning 0) (match-end 0)
                           (list 'display ""
                                 'line-prefix "⋮ ")))))

(defun my/org-show-properties-display ()
  "Show Org property drawer using text properties.
Based on the code shared at
https://org-roam.discourse.group/t/org-roam-major-redesign/1198/34."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:PROPERTIES:\n\\( *:.+?:.*\n\\)+ *:END:\n" nil t)
      (remove-list-of-text-properties (match-beginning 0) (match-end 0)
                                      (list 'display
                                            'line-prefix)))))

(defun my/helm-or-evil-escape ()
  "Escape from anything."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ;; ((cl-find-if #'funcall doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))


                                        ; alternate whitespace-mode with whitespace.el defaults, doom defaults and off:

(defun my/set-whitespace-defaults()
                                        ; only save the values the first time we get here
  (unless (boundp 'my/default-whitespace-style)
    (setq bh:default-whitespace-style                (default-value 'whitespace-style)
          bh:default-whitespace-display-mappings     (default-value 'whitespace-display-mappings)
          bh:doom-whitespace-style                   whitespace-style
          bh:doom-whitespace-display-mappings        whitespace-display-mappings
          bh:whitespace-mode                         "doom")))

                                        ; whitespace-style etc are set up with default-values in whitespace.el and then
                                        ; modified in doom-highlight-non-default-indentation-h (in core/core-ui.el).
                                        ; This is added to after-change-major-mode-hook in doom-init-ui-h (in
                                        ; core/core-ui.el) and called a LOT: so I need to capture doom's modified
                                        ; settings after that. The trouble is, this file (config.el) is called _before_
                                        ; doom-init-ui-h which is called in window-setup-hook as the last gasp of
                                        ; doom-initialize! find-file-hook appears to work.

(add-hook 'find-file-hook #'my/set-whitespace-defaults 'append)

                                        ; doom=>default=>off=>doom=>...
(defun my/toggle-whitespace () (interactive)
       (cond ((equal my/whitespace-mode "doom")
              (setq whitespace-style my/default-whitespace-style
                    whitespace-display-mappings my/default-whitespace-display-mappings
                    my/whitespace-mode "default")
              (prin1 (concat "whitespace-mode is whitespace default"))
              (whitespace-mode))
             ((equal my/whitespace-mode "default")
              (setq my/whitespace-mode "off")
              (prin1 (concat "whitespace-mode is off"))
              (whitespace-mode -1))
             (t ; (equal my/whitespace-mode "off")
              (setq whitespace-style my/doom-whitespace-style
                    whitespace-display-mappings my/doom-whitespace-display-mappings
                    my/whitespace-mode "doom")
              (prin1 (concat "whitespace-mode is doom default"))
              (whitespace-mode))))

(global-set-key (kbd "C-<f4>")          'my/toggle-whitespace)

(defun my/org-toggle-item
    (interactive)
  (org-toggle-item)
  (org-end-of-line)
  )


(defun open-file-in-browser (file) (browse-url-of-file file))
;; (defun export-to-html-and-open (file)
;;   (let* (html-filename (org-html-export-to-html file))
;;        (open-file-in-browser html-filename)))
(map! :map embark-file-map "O" #'open-file-in-browser)
(map! :map embark-file-map "F" #'citar-file-open-external)
(map! :map embark-file-map "C-o" #'find-file-other-frame)
(map! :map embark-general-map "W" #'widen)
(map! :map citar-citation-map "f" #'citar-file-open-external)

;; (defun my/set-writeroom-width ()
;;   (interactive)
;;   (setq writeroom-width (if writeroom-width )))

(defun my/orgnv-update-db ()
  (interactive)
  (setq orgnv--database (orgnv-build-database))
  )

(defmacro wsl--open-with (id &optional app dir)
  `(defun ,(intern (format "wsl/%s" id)) ()
     (interactive)
     (wsl-open-with ,app ,dir)))

(defun wsl-open-with (&optional app-name path)
  "Send PATH to APP-NAME on WSL."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "%s `wslpath -w %s`" (shell-quote-argument app-name) path)))
    (shell-command-to-string command)))

(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)
;; M-x wsl/open-in-default-program
;; M-x wsl/reveal-in-explorer
(defun my/open-current-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (read-shell-command "Open current file with: ")
                    " "
                    (shell-quote-argument buffer-file-name)))))
(global-set-key (kbd "C-c O") #'my/open-current-with)

(defun my/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
   same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat
           (buffer-file-name)
           "_"
           (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (setq filename (concat (file-name-directory filename) "images/" (file-name-nondirectory filename)))
  (powershell (concat "Add-Type -AssemblyName System.Windows.Forms; $clipImg = [System.Windows.Forms.Clipboard]::GetImage(); $clipImg.Save('" filename "')"))
  (insert (concat "[[file:" filename "]]"))
  (org-display-inline-images))

(defun as-windows-path (unix-path)
  ;; "Takes a unix path and returns a matching WSL path
  ;; (e.g. \\wsl$\Ubuntu-20.04\tmp)"
  ;; substring removes the trailing \n
  (substring
   (shell-command-to-string
    (concat "wslpath -w " unix-path)) 0 -1))

(defun powershell (script)
  "executes the given script within a powershell and returns its return value"
  (call-process "powershell.exe" nil nil nil
                "-Command" (concat "& {" script "}")))

(defun format-image-inline (source attributes info)
  (let* ((ext (file-name-extension source))
         (prefix (if (string= "svg" ext) "data:image/svg+xml;base64," "data:;base64,"))
         (data (with-temp-buffer (url-insert-file-contents source) (buffer-string)))
         (data-url (concat prefix (base64-encode-string data)))
         (attributes (org-combine-plists `(:src ,data-url) attributes)))
    (org-html-close-tag "img" (org-html--make-attribute-string attributes) info)))

(defun matlab-shell-help-at-point ()
  (interactive)
  (let ((fcn (matlab-read-word-at-point)))
    (if (and fcn (not (equal fcn "")))
        (matlab-shell-describe-command fcn))))

(defhydra my/mc-hydra (:color pink
                       :hint nil
                       :pre (evil-mc-pause-cursors))
  "
^Match^            ^Line-wise^           ^Manual^
^^^^^^----------------------------------------------------
_Z_: match all     _j_: make & go down   _z_: toggle here
_n_: make & next   _k_: make & go up     _r_: remove last
_p_: make & prev   ^ ^                   _R_: remove all
_N_: skip & next   ^ ^                   _t_: pause/resume
_P_: skip & prev

Current pattern: %`evil-mc-pattern

"
  ;; (setq evil-mc-pattern (buffer-substring region-beginning region-end))

  ("Z" #'evil-mc-make-all-cursors)
  ("n" #'evil-mc-make-and-goto-next-match)
  ("p" #'evil-mc-make-and-goto-prev-match)
  ("N" #'evil-mc-skip-and-goto-next-match)
  ("P" #'evil-mc-skip-and-goto-prev-match)
  ("k" #'evil-mc-make-cursor-move-next-line)
  ("j" #'evil-mc-make-cursor-move-prev-line)
  ("z" #'my/make-cursor-here)
  ("r" #'+multiple-cursors/evil-mc-undo-cursor)
  ("R" #'evil-mc-undo-all-cursors)
  ("t" #'+multiple-cursors/evil-mc-toggle-cursors)
  ("q" #'evil-mc-resume-cursors "quit" :color blue)
  ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))

(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(shell-command-to-string "wslpath -w '/mnt/g/My Drive/notes/gtd/inbox.org_20220410_170636_v4R344.png'")

(defun my/make-cursor-here ()
  (interactive)
  (+multiple-cursors/evil-mc-toggle-cursor-here)
  (evil-mc-pause-cursors))

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
  ;; (setq buffer-face-mode-face '(:extend t :family "iA Writer Quattro V Regular"))
  (setq buffer-face-mode-face '(:extend t :family "Iosevka Comfy Duo"))
  (buffer-face-mode)
  (set-face-attribute 'fixed-pitch nil :height 1.0)
  (set-face-attribute 'variable-pitch nil :height 1.0)
  )

(defun my/bash-shell ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\System32\\bash.exe" ))
    (shell "*bash*"))
  )

(defun my/indent-buffer ()
  (interactive)
  (save-excursion
    (evil-indent (point-min) (point-max))
    ))

(defun my/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (projectile-project-name))))

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

    ;; (advice-add #'(lambda () (add-hook! minibuffer-setup #'+zen/toggle)) :before consult-notes)
    ;; (advice-add #'(lambda () (remove-hook! minibuffer-setup #'+zen/toggle)) :after consult-notes)

(defun my/denote-link-or-create()
  (interactive)
  (let ((denote-file-prompt 'my/denote--find-file-with-pretty-format))
    (advice-add 'denote-file-prompt :around denote-file-prompt)
    )
  (call-interactively 'denote-link-or-create)
  (advice-remove 'denote-file-prompt 'my/denote--find-file-with-pretty-format)
  )

(defun +org/my-refile-to-other-window (arg)
  "Refile current heading to an org buffer visible in another window.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (win (delq (selected-window) (window-list)))
      (with-selected-window win
        (let ((file (buffer-file-name (buffer-base-buffer))))
          (and (eq major-mode 'org-mode)
               file
               (cl-pushnew (cons file (cons :maxlevel my-org-refile-maxlevel))
                           org-refile-targets)))))
    (call-interactively #'org-refile)))
(map! :map evil-org-mode-map :localleader :nv "ro" #'+org/my-refile-to-other-window)


(defun other-window-consult-ripgrep ()
  (interactive)
  (let ((dir default-directory))
    (with-selected-window (next-window)
      (let ((default-directory dir))
        (consult-ripgrep)))))

(defvar universal-coding-system-env-list '("PYTHONIOENCODING")
  "List of environment variables \\[universal-coding-system-argument] should set")

(defadvice universal-coding-system-argument (around provide-env-handler activate)
  "Augments \\[universal-coding-system-argument] so it also sets environment variables

Naively sets all environment variables specified in
`universal-coding-system-env-list' to the literal string
representation of the argument `coding-system'.

No guarantees are made that the environment variables set by this advice support
the same coding systems as Emacs."
  (let ((process-environment (copy-alist process-environment)))
    (dolist (extra-env universal-coding-system-env-list)
      (setenv extra-env (symbol-name (ad-get-arg 0))))
    ad-do-it))

(defun my/consult-notes (&optional sources)
  "Find a file in a notes directory with consult-multi, or from SOURCES."
  (interactive)
  (setq consult-notes-denote-files-function (denote-directory-text-only-files))
  (consult-notes--make-file-dir-sources)
  (consult--multi (or sources consult-notes-all-sources)
                  :require-match
                  (confirm-nonexistent-file-or-buffer)
                  :prompt "Notes: "
                  :history 'consult-notes-history))

(defun my/prompt-on-dired-buffer-kill ()
  (or (not (derived-mode-p 'dired-mode))
      (y-or-n-p (format "Kill Dired buffer `%s'" (current-buffer)))))
