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
        doom-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 20)
        doom-big-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 20)
        doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :weight 'regular :size 20)

        ;; doom-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 24)
        ;; doom-big-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 24)
        ;; doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :weight 'regular :size 24)

        ;; iA Writer Quattro font
        ;; doom-font  (font-spec :family "iA Writer Mono V Regular" :weight 'regular :size 18)
        ;; doom-big-font  (font-spec :family "iA Writer Quattro V Regular" :weight 'regular :size 18)
        ;; doom-variable-pitch-font (font-spec :family "iA Writer Quattro V Regular" :weight 'regular :size 18)

        ;; doom-font  (font-spec :family "iA Writer Quattro V Regular" :weight 'regular :size 24)
        ;; doom-big-font  (font-spec :family "iA Writer Quattro V Regular" :weight 'regular :size 24)
        ;; doom-variable-pitch-font (font-spec :family "iA Writer Quattro V Regular" :weight 'regular :size 24)
 )

(load! "themes-and-fonts.el")

;;
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq-default display-line-numbers-type 'relative)


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
(setq
 notes-dir "~/Documents/notes"
 gtd-dir (concat (file-name-as-directory notes-dir)  (file-name-as-directory "gtd"))
 slip-box-dir (concat (file-name-as-directory notes-dir)  (file-name-as-directory "slip-box"))
 literature-notes-dir (list (concat (file-name-as-directory slip-box-dir)  (file-name-as-directory "literature-notes")))
 emacs-directory doom-emacs-dir
 org-capture-writing-inbox-file (concat (file-name-as-directory notes-dir) "writing_inbox.org")
)

(setq package-gnupghome-dir ".local/elpa/gnupg")
(setq-default line-spacing 0.1)

(setq
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
 truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(add-load-path! "../doom-emacs/packages/")
(load! "package-config.el")
(load! "my-functions.el")
(load! "keybindings.el")


(setq which-key-idle-delay 0.2
      which-key-idle-secondary-delay 0.1
      which-key-allow-multiple-replacements t)

(setq-default
 evil-shift-width 4 ; globally
 tab-width 4) ; globally
(setq evil-shift-width 4
      tab-width 4)

(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words

(setq
 visual-fill-column-width 90
 split-window-preferred-function 'visual-fill-column-split-window-sensibly

 dired-dwim-target t

 evil-vsplit-window-right t
 evil-split-window-below t)

(remove-hook 'text-mode-hook #'auto-fill-mode)

(defun my/snipe_ivy ()
  (evilem-create (list 'evil-snipe-repeat
                       'evil-snipe-repeat-reverse)
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

(map! :map evil-snipe-parent-transient-map "C-;" #'my/snipe_ivy )

(custom-set-variables
 '(helm-ag-base-command "rg --no-heading")
 `(helm-ag-success-exit-status '(0 2)))



(setq-default evil-escape-delay 0.4)

(font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)))
(add-hook! org-mode
          (sp-pair "$" "$")
                      )
;; (add-hook! 'evil-normal-state-entry-hook  #'save-buffer)
;; (add-hook! 'evil-insert-state-entry-hook #'save-buffer)

;; (add-hook! 'text-mode-hook 'my-buffer-face-mode-text)
(add-hook! 'text-mode-hook
                (delete-selection-mode 1)
                (visual-fill-column-mode 1)
                (visual-line-mode 1)
                (abbrev-mode 1)
                (font-lock-mode 1)
                (buffer-face-mode 1)
                (captain-mode 1)
                (+zen/toggle)
                (setq org-modern-star nil)
                (setq org-hide-leading-stars t)
                (+word-wrap-mode 1)
                (flyspell-lazy-mode -1)
                (set-face-attribute 'fixed-pitch nil :height 1.0)
                (set-face-attribute 'variable-pitch nil :height 1.0)
                             )
(add-hook! 'text-mode-hook
                            (setq bidi-paragraph-direction nil)
                            (setq bidi-paragraph-start-re  "^")
                            (setq bidi-paragraph-separate-re  "^")
                            (setq helm-ff-fuzzy-matching t)
                            (setq company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))
                            (setq company-frontends '(company-preview-frontend))
                            (setq line-spacing 0.3)
                            (setq buffer-file-coding-system 'utf-8)
                            (setq save-buffer-coding-system 'utf-8)
                            (set-face-attribute 'fixed-pitch nil :height 1.0)
                            (set-face-attribute 'variable-pitch nil :height 1.0)
                            )


(add-hook! 'prog-mode-hook 'my-buffer-face-mode-programming)
(add-hook! 'prog-mode-hook
                            (setq company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))
                            (setq line-spacing 0.3)
                            (delete-selection-mode 1)
                            (flyspell-lazy-mode -1)
                            (+word-wrap-mode 1)
                            (+zen/toggle)
                            )


(evil-snipe-override-mode 1)

(add-hook 'after-init-hook 'company-statistics-mode)

(add-hook 'occur-mode-hook
          (defun occur-show-replace-context+ ()
            (add-hook 'replace-update-post-hook
                      'occur-mode-display-occurrence nil 'local)))

(define-advice occur-mode-display-occurrence
    (:around (fun &rest args) save-match-data)
  (save-match-data
    (apply fun args)))


 (setq-default prescient-history-length 1000)

(add-hook 'bibtex-mode-hook 'my/fix-windows-bib-file)

(defadvice! prompt-for-buffer (&rest _)
  :after 'evil-window-vsplit (switch-to-buffer))

(set-input-method 'hebrew-full)

(remove-hook 'after-save-hook #'ws-butler-after-save)

(defun my/dedicate-org-roam-buffer ()
  (interactive)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (dedicated . t)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t))))))

(setq org-odt-preferred-output-format "docx")
(defun my/make-small-frame () (interactive) (set-frame-size (selected-frame) 50 42))
(defun my/make-medium-frame () (interactive) (set-frame-size (selected-frame) 100 35))
(defun my/make-large-frame () (interactive) (set-frame-size (selected-frame) 100 45))
(add-to-list 'default-frame-alist '(height . 38))
(add-to-list 'default-frame-alist '(width . 50))

(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq python-shell-prompt-detect-failure-warning nil)
(setq lsp-pylsp-plugins-flake8-max-line-length 90)
(custom-set-variables '(linum-format 'dynamic))

;;; On Windows, commands run by flycheck may have CRs (\r\n line endings).
;;; Strip them out before parsing.
(defun flycheck-parse-output (output checker buffer)
  "Parse OUTPUT from CHECKER in BUFFER.

OUTPUT is a string with the output from the checker symbol
CHECKER.  BUFFER is the buffer which was checked.

Return the errors parsed with the error patterns of CHECKER."
  (let ((sanitized-output (replace-regexp-in-string "\r" "" output))
        )
    (funcall (flycheck-checker-get checker 'error-parser) sanitized-output checker buffer)))

(setq lsp-typescript-npm "c:/Program Files/nodejs/npm")

(setq lsp-python-ms-python-executable-cmd "c:/Users/Jonathan/miniconda3/python")
(setq lsp-pyright-python-executable-cmd "c:/Users/Jonathan/miniconda3/python")

(setq flycheck-python-pyright-executable  "c:/Users/Jonathan/programs/scripts/pyright.exe")
(setq flycheck-python-pycompile-executable "c:/Users/Jonathan/miniconda3/python")

(setq python-shell-interpreter "c:/Users/Jonathan/miniconda3/python")


(setq +zen-text-scale nil)

(setq +bidi-hebrew-font (font-spec :family "Heebo"))

(add-hook! (text-mode) :local (lambda ()
                            (add-hook! after-save-hook #'my/fix-hebrew-hyphen)

                            ))

;; (setq mouse-wheel-scroll-amount '(2 (1)))
(setq mouse-wheel-scroll-amount '(2 (hscroll)))


;;; Ibuffer and extras (dired-like buffer list manager)
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
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  ;; (let ((map ibuffer-mode-map))
  ;;   (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
  ;;   (define-key map (kbd "* g") #'ibuffer-mark-by-content-regexp) ; "g" is for "grep"
  ;;   (define-key map (kbd "* n") #'ibuffer-mark-by-name-regexp)
  ;;   (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)  ; "sort name" mnemonic
  ;;   (define-key map (kbd "/ g") #'ibuffer-filter-by-content))



;; define global minor modes
(add-hook! 'dired-mode-hook :append '(dired-filter-mode +zen/toggle))
(setq citar--multiple-setup (cons "<tab>"  "RET"))
(setq writeroom-mode-line 't)
(setq +zen-text-scale 0)

(define-globalized-minor-mode global-zen-mode writeroom-mode
  (lambda () (+zen/toggle 1)))
(define-globalized-minor-mode global-delete-selection-mode delete-selection-mode
  (lambda () (delete-selection-mode 1)))

;; (global-delete-selection-mode 1)
;; (global-zen-mode 1)

(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "l" "^" "\s*$") ; a line object without trailing whitespaces

(yas-global-mode 0)

(setq eshell-prompt-function
      (lambda()
        (concat (getenv "USER") "@" (getenv "HOST") ":"
                ((lambda (p-lst)
                   (if (> (length p-lst) 3)
                       (concat
                        (mapconcat (lambda (elm) (substring elm 0 1))
                                   (butlast p-lst (- (length p-lst) 3))
                                   "/")
                        "/"
                        (mapconcat (lambda (elm) elm)
                                   (last p-lst (- (length p-lst) 3))
                                   "/"))
                     (mapconcat (lambda (elm) elm)
                                p-lst
                                "/")))
                 (split-string (eshell/pwd) "/"))
                (if (= (user-uid) 0) " # " " $ "))))

(setq tramp-default-method "plink -share")

(put 'tab-bar-tab-inactive 'face-alias 'tab-bar)

(setq company-backends '((company-capf company-files company-dabbrev-code company-dabbrev)))
(setq writeroom-width 100)
;; (diredp-toggle-find-file-reuse-dir 1)
(setq dired-compress-file-alist '(("\\.gz\\'" . "gzip -9f %i") ("\\.bz2\\'" . "bzip2 -9f %i") ("\\.xz\\'" . "xz -9f %i") ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i") ("\\.zip\\'" . "zip %o -r --filesync %i")))
;; tabs
(defun my/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (projectile-project-name))))

(setq tab-bar-show 1)
;; (setq tab-bar-show nil)
(setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-mode t)
(setq tab-bar-new-tab-choice "*doom*")
(setq tab-bar-tab-name-function #'my/name-tab-by-project-or-default)

;; (add-hook! minibuffer-setup #'+zen/toggle)
(remove-hook! minibuffer-setup #'+zen/toggle)
(setq auto-revert-remote-files nil)
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)
(setq projectile-mode-line "Projectile")

;; example for kmacro binding
;; (fset 'insert-link-to-new-note-thesis
;;    (kmacro-lambda-form [?d ?  ?n ?i ?\C-y return return ?\C-y return ?t ?h ?e ?s return] 0 "%d"))
;; (map! :leader :map org-mode-map :v "nk"  #'insert-link-to-new-note-thesis)

(fset 'make-region-bold
   (kmacro-lambda-form [?S ?*] 0 "%d"))
(map! :map evil-org-mode-map :v "C-b"  #'make-region-bold)
;; (map! :map evil-org-mode-map :v "C-u"  #'make-region-underline)
;; (map! :map evil-org-mode-map :v "C-i"  #'make-region-italic)


;; *****************************************
;; never collect garbage. use with caution!~
  ;; (setq gc-cons-threshold 5000000000)
  ;; (defun garbage-collect (&rest args)
  ;;   (message "trying to garbage collect. probably you want to quit emacs."))
  (setq garbage-collection-messages nil)
;; *****************************************

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-input-method 'hebrew-full)
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-keyboard-coding-system 'utf-8-unix)
(setq file-coding-system-alist '(("\\.elc\\'" . utf-8-emacs) ("\\.el\\'" . prefer-utf-8) ("\\.utf\\(-8\\)?\\'" . utf-8) ("\\.xml\\'" . xml-find-file-coding-system) ("\\(\\`\\|/\\)loaddefs.el\\'" raw-text . raw-text-unix) ("\\.tar\\'" no-conversion . no-conversion) ("\\.po[tx]?\\'\\|\\.po\\." . po-find-file-coding-system) ("\\.\\(tex\\|ltx\\|dtx\\|drv\\)\\'" . latexenc-find-file-coding-system) ("\\.org\\'" . utf-8)  ("" undecided)))
;; do this especially on Windows, for python output problems
;; (set-terminal-coding-system 'utf-8-unix)
;;


(set-face-attribute 'fixed-pitch nil :height 1.0)
(set-face-attribute 'variable-pitch nil :height 1.0)
(add-hook! minibuffer-setup #'+zen/toggle)
;; (remove-hook! minibuffer-setup #'+zen/toggle)

(add-to-list '+lookup-provider-url-alist '("Google Scholar" . "https://scholar.google.com/scholar?q=%s"))
