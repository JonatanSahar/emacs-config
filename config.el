;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jonathan Sahar"
      user-mail-address "jonathan.sahar@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-font
;;
(setq
 ;; Roboto Mono font
 ;; doom-font  (font-spec :family "Roboto Mono" :weight 'regular :size 20)
 ;; doom-big-font  (font-spec :family "Roboto Mono" :weight 'regular :size 20)
 ;; doom-variable-pitch-font (font-spec :family "Noto Sans" :size 20)

 ;; Iosevka Comfy font
 doom-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 15)
 doom-big-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 15)
 doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :weight 'regular :size 15)
 )

(load! "themes-and-fonts.el")

;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default display-line-numbers-type 'relative)
(custom-set-variables '(linum-format 'dynamic))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; various settings

(setq package-gnupghome-dir ".local/elpa/gnupg")
(setq-default line-spacing 0.1)

(setq
 text-scale-mode-step 1.05
 evil-respect-visual-line-mode 't
 scroll-bar-mode 1
 delete-by-moving-to-trash nil                      ; Delete files to trash
 uniquify-buffer-name-style nil              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other        windows (not just current)
 x-stretch-cursor t                              ; Stretch cursor to the glyph width
 undo-limit 80000000                         ; Raise undo-limit to 80Mb
 evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
 garbage-collection-messages nil
 auto-save-default t                         ; Nobody likes to lose work, I certainly don't
 inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
 backup-directory-alist `(("." . ,(concat user-emacs-directory "autosaved_files")))
 truncate-string-ellipsis "…"
 visual-fill-column-width 300
 split-window-preferred-function 'visual-fill-column-split-window-sensibly

 dired-dwim-target t

 evil-vsplit-window-right t
 evil-split-window-below t
 evil-escape-delay 0.4
 mouse-wheel-scroll-amount '(2 (hscroll))
 )

(evil-snipe-override-mode 1)

(add-hook! 'text-mode-hook 'my-buffer-face-mode-text)
(add-hook! 'text-mode-hook
  (remove-hook! 'company-mode (company-box-mode))
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (abbrev-mode 1)
  (font-lock-mode 1)
  (buffer-face-mode 1)
  (captain-mode 1)
  (hl-todo-mode 1)
  (+zen/toggle)
  (setq org-modern-star nil)
  (setq org-hide-leading-stars t)
  (+word-wrap-mode 1)
  (flyspell-lazy-mode -1)

  (setq bidi-paragraph-direction nil)
  (setq bidi-paragraph-start-re  "^")
  (setq bidi-paragraph-separate-re  "^")
  (setq helm-ff-fuzzy-matching t)
  (setq company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))
  (setq-local company-frontends '(company-preview-frontend))
  (setq line-spacing 0.3)
  (setq buffer-file-coding-system 'utf-8)
  (setq save-buffer-coding-system 'utf-8)
  (set-face-attribute 'fixed-pitch nil :height 1.0)
  (set-face-attribute 'variable-pitch nil :height 1.0)
  )

(defun company-prog-mode-hook ()
  (when (and (boundp 'company-mode) company-mode)
    (company-box-mode)
    ))
(add-hook! 'prog-mode-hook 'my-buffer-face-mode-programming)
(add-hook! 'prog-mode-hook
  (company-prog-mode-hook)
  (setq company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))
  (setq-local company-frontends '(company-box-frontend))
  (setq line-spacing 0.3)
  (delete-selection-mode 1)
  (hl-todo-mode 1)
  (flyspell-lazy-mode -1)
  (+word-wrap-mode 1)
  (+zen/toggle)
  )

(after! tramp
  (add-to-list 'tramp-remote-path "/home/yonatan/.local/bin")
  )

(setq which-key-idle-delay 0.2
      which-key-idle-secondary-delay 0.1
      which-key-allow-multiple-replacements t)

(defadvice! prompt-for-buffer (&rest _)
  :after 'evil-window-vsplit (switch-to-buffer))

(set-input-method 'hebrew-full)
(setq +bidi-hebrew-font (font-spec :family "Heebo"))
(set-language-environment "UTF-8")
(set-input-method 'hebrew-full)
(setq file-coding-system-alist '(("\\.elc\\'" . utf-8-emacs) ("\\.el\\'" . prefer-utf-8) ("\\.utf\\(-8\\)?\\'" . utf-8) ("\\.xml\\'" . xml-find-file-coding-system) ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix) ("\\.tar\\'" no-conversion . no-conversion) ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system) ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system) ("\\.org\\'" . utf-8)  ("" undecided)))


(remove-hook 'after-save-hook #'ws-butler-after-save)

;; move to package-config.el
(setq org-odt-preferred-output-format "docx")

;; width x hight
(defun my/make-small-frame () (interactive) (set-frame-size (selected-frame) 50 42))
(defun my/make-medium-frame () (interactive) (set-frame-size (selected-frame) 110 40))
(defun my/make-large-frame () (interactive) (set-frame-size (selected-frame) 140 47))

(setq default-frame-alist '(
                            (height . 47)
                            (width . 140)
                            (vertical-scroll-bars)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (left-fringe . 8)
                            (right-fringe . 8)))


(setq initial-frame-alist '(
                            (top . 00)
                            (left . 2000)
                            (height . 50)
                            (width . 140)
                            (vertical-scroll-bars)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)
                            (left-fringe . 8)
                            (right-fringe . 8)))


;; Ibuffer and extras (dired-like buffer list manager)
(setq ibuffer-expert t)
(setq ibuffer-display-summary nil)
(setq ibuffer-use-other-window nil)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-movement-cycle nil)
(setq ibuffer-default-sorting-mode 'filename/process)
(setq ibuffer-use-header-line t)
(setq ibuffer-default-shrink-to-minimum-size nil)
(setq ibuffer-formats
      '((mark modified read-only locked " "
         (name 40 40 :left :elide)
         " "
         (size 9 -1 :right)
         " "
         (mode 16 16 :left :elide)
         " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))
(setq ibuffer-saved-filter-groups nil)
(setq ibuffer-old-time 48)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)

(setq citar--multiple-setup (cons "<tab>"  "RET"))
(setq writeroom-mode-line 't)
(evil-define-text-object evil-select-inner-line-no-whitespace (count &optional beg end type)
  "Select all text on the current line, excluding leading and trailing whitespace."
  (let* ((begin (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward " \t")
                  (point)))
         (end (save-excursion
                (end-of-line)
                (skip-chars-backward " \t")
                (point))))
    (evil-range begin end 'exclusive)))

;; Bind the text object to "il" for inner line
(define-key evil-inner-text-objects-map "l" 'evil-select-inner-line-no-whitespace)

(yas-global-mode nil)

;; tabs
(defun my/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (projectile-project-name))))

(setq! tab-bar-show nil)
;; (setq tab-bar-show nil)
(setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-mode t)
(setq tab-bar-new-tab-choice "*doom*")
(setq tab-bar-tab-name-function #'my/name-tab-by-project-or-default)
(put 'tab-bar-tab-inactive 'face-alias 'tab-bar)

(setq company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))
(setq writeroom-width 100)

;; Dired
(dired-async-mode 1)
(setq dired-compress-file-alist '(("\\.gz\\'" . "gzip -9f %i") ("\\.bz2\\'" . "bzip2 -9f %i") ("\\.xz\\'" . "xz -9f %i") ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i") ("\\.zip\\'" . "zip %o -r --filesync %i")))

(setq dired-guess-shell-alist-user
      '(("\\.\\(png\\|jpe?g\\|gif\\|bmp\\|tiff?\\)$" "eog")))

(defun my/dired-async-no-popup (orig-fun &rest args)
  "Run `dired-do-async-shell-command' without popping up a buffer."
  (let ((display-buffer-alist '(("Async Shell Command" display-buffer-no-window))))
    (apply orig-fun args)))

(advice-add 'dired-do-async-shell-command :around #'my/dired-async-no-popup)

(set-face-attribute 'fixed-pitch nil :height 1.0)
(set-face-attribute 'variable-pitch nil :height 1.0)
;; (add-hook 'denote-backlinks-mode-hook #'+zen/toggle)

(setq doom-projectile-fd-binary "fdfind")
(global-hl-todo-mode 1)
;;
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

(pixel-scroll-precision-mode 1)

;; ;; Shouldn't be here but didn't work otherwise
;; (add-hook! 'jupyter-repl-mode-hook #'electric-pair-mode (writeroom-mode -1))
;; (map! :map jupyter-repl-mode-map
;;       :i "C-k" #'jupyter-repl-history-previous
;;       :nvi "C-e" #'evil-end-of-line-or-visual-line
;;       :i "C-j" #'jupyter-repl-history-next
;;       :i "<up>" #'jupyter-repl-history-previous
;;       :i "<down>" #'jupyter-repl-history-next)
(add-to-list 'display-buffer-alist
               (cons "\\`\\*jupyter-.*\\'"
                     (cons 'display-buffer-reuse-window
                           '((reusable-frames . visible)
                             (inhibit-switch-frame . nil)))))

(load! "keybindings.el")

(use-package! denote
  :config
  (setq org-agenda-files (list (concat (file-name-as-directory denote-directory) "20240219T111038--analysis-log-vaccine-response__work.org") (concat (file-name-as-directory denote-directory) "20240417T172124--analysis-log-spatial-pipeline__work.org")(concat (file-name-as-directory denote-directory) "20240219T105512--papers-to-read__work.org")))

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
  )

(use-package! consult-notes
  :after denote
  :init
  (consult-notes-denote-mode)
  :commands (consult-notes)
  :config
  (setq consult-notes-denote-display-id nil)
  )

(after! denote
  (map!
   (:map org-mode-map :leader
         (:prefix "n"
          :nv "o" #'denote-open-or-create
          ;; :nv "f" #'denote-open-or-create
          :nv "f" #'consult-notes
          :nv "n" #'denote
          :nv "r" #'denote-rename-file
          :nv "R" #'denote-rename-file-using-front-matter
          :nv "k" #'denote-keywords-add
          :nv "K" #'denote-keywords-remove
          :nv "D" #'denote-date
          :nv "z" #'denote-signature ; "zettelkasten" mnemonic
          :nv "s" #'denote-subdirectory
          :nv "t" #'denote-template
          :nv "i" #'denote-link-or-create ; denote-link ; "insert" mnemonic
          :nv "I" #'denote-link
          :nv "L" #'denote-link-after-creating
          :nv "a" #'denote-link-add-links
          :nv "b" #'denote-backlinks
          :nv "F" #'denote-link-find-file
          :nv "B" #'denote-link-find-backlink))

   (:map org-mode-map :nvi
         "C-c n j" #'my-denote-journal ; our custom command
         "C-c n o" #'denote-open-or-create
         "C-c n n" #'denote
         "C-c n N" #'denote-type
         "C-c n d" #'denote-date
         "C-c n z" #'denote-signature ; "zettelkasten" mnemonic
         "C-c n s" #'denote-subdirectory
         "C-c n t" #'denote-template
         ;; If you intend to use Denote with a variety of file types, it is
         ;; easier to bind the link-related commands to the `global-map', as
         ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
         ;; `markdown-mode-map', and/or `text-mode-map'.
         "C-c n I" #'denote-link; "insert" mnemonic
         "C-c n L" #'denote-link-after-creating
         "C-c n i" #'denote-link-or-create ; "insert" mnemonic
         "[[" #'denote-link-or-create
         "C-c n a" #'denote-link-add-links
         "C-c n b" #'denote-backlinks
         "C-c n f f" #'denote-link-find-file
         "C-c n f b" #'denote-link-find-backlink
         "C-c n k a" #'denote-keywords-add
         "C-c n k r" #'denote-keywords-remove
         ;; Note that `denote-rename-file' can work from any context, not just
         ;; Dired bufffers.  That is why we bind it here to the `global-map'.
         "C-c n r" #'denote-rename-file
         "C-c n R" #'denote-rename-file-using-front-matter)

   ;; Key bindings specifically for Dired.
   (:map dired-mode-map
         "C-c C-d C-i" #'denote-link-dired-marked-notes
         "C-c C-d C-r" #'denote-dired-rename-marked-files
         "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter)

   (:map evil-org-mode-map :prefix "C-n" :nvi
         "j" #'my-denote-journal ; our custom command

         "o" #'denote-open-or-create
         "n" #'denote
         "N" #'denote-type
         "d" #'denote-date
         "z" #'denote-signature ; "zettelkasten" mnemonic
         "s" #'denote-subdirectory
         "t" #'denote-template
         ;; If you intend to use Denote with a variety of file types, it is
         ;; easier to bind the link-related commands to the `global-map', as
         ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
         ;; `markdown-mode-map', and/or `text-mode-map'.
         "I" #'denote-link; "insert" mnemonic
         "i" #'denote-link-or-create ; "insert" mnemonic
         "[[" #'denote-link-or-create
         "]]" #'denote-link-or-create
         "a" #'denote-link-add-links
         "b" #'denote-backlinks
         "f f" #'denote-link-find-file
         "f b" #'denote-link-find-backlink
         "k a" #'denote-keywords-add
         "k r" #'denote-keywords-remove
         ;; Note that `denote-rename-file' can work from any context, not just
         ;; Dired bufffers.  That is why we bind it here to the `global-map'.
         "r" #'denote-rename-file
         "R" #'denote-rename-file-using-front-matter))
  )

(use-package jupyter
  :demand t
  :after (:all org python)

  :config
  (map! :map jupyter-repl-mode-map
        :i "C-k" #'jupyter-repl-history-previous
        :i "C-j" #'jupyter-repl-history-next
        :nvi "C-e" #'evil-end-of-line-or-visual-line
        :i "<up>" #'jupyter-repl-history-previous
        :i "<down>" #'jupyter-repl-history-next)

  (add-hook! 'jupyter-repl-mode-hook #'electric-pair-mode)
  (add-hook! 'jupyter-repl-mode-hook (writeroom-mode -1)))




;; code cells
(use-package code-cells
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

(use-package consult-dir
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

(use-package org
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
                        ;; ( org-capture-projects-file :maxlevel . 1)
                        ;; ( org-capture-someday-file :level . 1)
                        ;; ( org-capture-inbox-file :maxlevel . 2)
                        ;; ((concat denote-directory "20240417T172124--analysis-log-spatial-pipeline__work.org") :level . 1)
                        ;; ((concat denote-directory "20240219T111038--analysis-log-vaccine-response__work.org") :level . 1)
                        ("~/Documents/notes/20240219T111038--analysis-log-vaccine-response__work.org" :level . 1)
                        ("~/Documents/notes/20240417T172124--analysis-log-spatial-pipeline__work.org" :level . 1)
                        (nil . (:maxlevel . 9)) ;; current buffer
                        ;; ( org-capture-reminders-file :maxlevel . 1)
                        )

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
   )

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
    :group 'all-the-icons-faces)

  (defun citar-open-library-file (key-entry)
    "Open library file associated with the KEY-ENTRY.

With prefix, rebuild the cache before offering candidates."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg)))
    (let ((embark-default-action-overrides '((file . citar-file-open-external))))
      (message "embark-default-action-overrides %s" embark-default-action-overrides)
      (when (and citar-library-paths
                 (stringp citar-library-paths))
        (error "Make sure 'citar-library-paths' is a list of paths"))
      (citar--library-file-action key-entry 'open)))

  (defun citar--library-file-action (key-entry action)
    "Run ACTION on file associated with KEY-ENTRY."
    (let* ((fn (pcase action
                 ('open 'citar-file-open-external 'citar-file-open)
                 ('attach 'mml-attach-file)))
           (ke (citar--ensure-entries key-entry))
           (key (caar ke))
           (entry (cdar ke))
           (files
            (citar-file--files-for-entry
             key
             entry
             citar-library-paths
             citar-file-extensions))
           (file
            (pcase (length files)
              (1 (car files))
              ((guard (> 1))
               (citar-select-file files)))))
      (if file
          (funcall fn file)
        (message "No associated file"))))
  )

(after! lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (eglot))) ; or lsp-
  :config

  ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-use-library-code-for-types t))

(after! company
  :config
  ;; completion
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t)
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  (setq company-dabbrev-other-buffers 't)
  (setq company-dabbrev-code-other-buffers 't)
  (map! :map org-mode-map :i
        "C-;" #'+company/complete
        "M-;" #'+company/complete)
  )

(setq ispell-personal-dictionary-en   "~/Documents/dictionaries/personal.en")
(setq ispell-personal-dictionary-heb  "~/Documents/dictionaries/personal.heb")
(setq ispell-local-dictionary-alist '(("en_US"
                                       "[[:alpha:]]"
                                       "[^[:alpha:]]"
                                       "[']"
                                       t
                                       ("-d" "en_US" "-p"   "C:\\Users\\Jonathan\\programs\\hunspell\\share\\hunspell\\personal.en")
                                       nil
                                       iso-8859-1)

                                      ("hebrew"
                                       "[[:alpha:]]"
                                       "[^[:alpha:]]"
                                       "[']"
                                       t
                                       ("-d" "hebrew" "-p"   "C:\\Users\\Jonathan\\programs\\hunspell\\share\\hunspell\\personal.heb")
                                       nil
                                       iso-8859-1)))

(setq ispell-dictionary "en_US") ; Default dictionary to use
;; (add-to-list 'exec-path "C:\\Users\\Jonathan\\programs\\hunspell\\bin")

(setq ispell-program-name (locate-file "hunspell"
                                       exec-path exec-suffixes 'file-executable-p))

(unless (file-exists-p ispell-personal-dictionary-en)
  (write-region "" nil ispell-personal-dictionary-en nil 0))
(unless (file-exists-p ispell-personal-dictionary-heb)
  (write-region "" nil ispell-personal-dictionary-heb nil 0))


(defun init-spellchecker()
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (setq ispell-dictionary "en_US,hebrew") ; Default dictionary to use
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,hebrew"))

(use-package! company-box
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  )


(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)

  :config

  (defun my/name-tab-by-project-or-default ()
    "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
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
      (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
      (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
      (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
      map)
    "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")

  ;; Filter Buffers for Consult-Buffer

  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  )

(use-package! popper
  :bind (
         ("C-`"   . #'popper-kill-latest-popup)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Python\\*"
          "^*jupyter"
          "\\*MATLAB\\*"
          "\\*Ibuffer\\*"
          "\\*denote-backlinks"
          "\\*ChatGPT\\* "
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  ;; (map! :map prog-mode-map :nv "`" #'popper-toggle-latest)
  )                ; For echo area hints

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
  (setq tmr-sound-file "~/Documents/sounds/tibetian-bowl-1.wav")
  )

(use-package! gptel
  :init
  :config
  (setq gptel-api-key (getenv "OPENAI_API_KEY")
        gptel-use-curl 'nil
        gptel-stream nil
        gptel-default-mode 'org-mode)

  (setq! gptel-directives
         '(
           (default   . "You are a large language model and a helpful assistant. Answer the user’s questions accurately and concisely.")
           (programming . "You are a language model with expert programming knowledge. Provide clean, correct code solutions with minimal commentary")
           (writing   . "You are a skilled writing assistant. Help improve text for clarity, style, and correctness. Respond succinctly and to the point.")
           (chat      . "You are a friendly conversational partner and knowledgeable assistant. Engage naturally and helpfully in conversation, keeping your responses concise.")
           (refactor  . "You are a language model with expert programming knowledge. Provide clean, correct code solutions with minimal commentary; output code and only code, DO NOT add code fences e.g. Python ''' ''' around the code.")
           (rewrite   . "You are a language model skilled in rephrasing. Rewrite the provided text to improve clarity and conciseness while preserving its original meaning.")
           ))
  (setq
   gptel-model 'phi4:latest
   gptel-backend (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    ;; :endpoint "/api/generate"
    :models '(phi4:latest qwen2.5-coder:32b deepseek-r1:32b)
))

(gptel-make-anthropic "Claude"          ;Any name you want
  :stream t                             ;Streaming responses
  :key(getenv "ANTHROPIC_API_KEY"))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    ;; :endpoint "/api/generate"
    :models '(phi4:latest qwen2.5-coder:32b deepseek-r1:32b)))


(use-package org-download
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
   highlight-thing-ignore-list '("False" "True")
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
    "Convert all marked images to jpg(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to jpg"
     "convert -verbose '<<f>>' '<<fne>>.jpg'"
     :utils "convert"))

  (defun my/dwim-shell-command-convert-audio-to-mp3 ()
    "Convert all marked audio to mp3(s)."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Convert to mp3"
     "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
     :utils "ffmpeg"))

  (defun my/dwim-shell-commands-files-combined-size ()
    "Get files combined file size."
    (interactive)
    (dwim-shell-command-on-marked-files
     "Get files combined file size"
     "du -csh '<<*>>'"
     :utils "du"
     :on-completion (lambda (buffer _process)
                      (with-current-buffer buffer
                        (message "Total size: %s"
                                 (progn
                                   (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
                                   (match-string 1))))
                      (kill-buffer buffer))))
  )


(defun my/treemacs-copy-name-at-point ()
  "Copy the filename or directory name at point in Treemacs."
  (interactive)
  (let ((name (treemacs-node-at-point)))
    (if name
        (progn
          (kill-new (treemacs--get-label-of name))
          (message "Copied: %s" (treemacs--get-label-of name)))
      (message "No file or directory at point"))))

(after! treemacs
  (setq! treemacs-sorting 'mod-time-desc)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "y n") #'my/treemacs-copy-name-at-point)
  )

(after! spacious-padding
  (spacious-padding-mode 1))

;; (use-package elysium
;;   :custom
;;   ;; Below are the default values
;;   (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
;;   (elysium-window-style 'vertical)) ; Can be customized to horizontal

(use-package smerge-mode
  :hook
  (prog-mode . smerge-mode))

(defun my/search-replace ()
  (interactive)
  (if (use-region-p) (my/search-replace-in-region) (evil-ex "%s/"))
  )

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

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
