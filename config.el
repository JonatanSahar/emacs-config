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
 doom-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 18)
 doom-big-font  (font-spec :family "Iosevka Comfy" :weight 'regular :size 18)
 doom-variable-pitch-font (font-spec :family "Iosevka Comfy Duo" :weight 'regular :size 18)
 )

(setq doom-theme 'modus-operandi-tinted)
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

(setq package-gnupghome-dir ".doom.d/elpa/gnupg")
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

(add-hook! 'prog-mode-hook 'my-buffer-face-mode-programming)
(add-hook! 'prog-mode-hook
  (setq line-spacing 0.3)
  (delete-selection-mode 1)
  (hl-todo-mode 1)
  (flyspell-lazy-mode -1)
  (+word-wrap-mode 1)
  (+zen/toggle)
  )

(add-hook! 'dirvish-directory-view-mode (+zen/toggle))

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
(setq! tab-bar-show nil)
;; (setq tab-bar-show nil)
(setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-mode t)
(setq tab-bar-new-tab-choice "*doom*")
(put 'tab-bar-tab-inactive 'face-alias 'tab-bar)

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
(defun my/dired-disable-vc-for-remote ()
  "Disable VC integration in remote Dired buffers for performance."
  (when (file-remote-p default-directory)
    (setq-local vc-handled-backends nil)))
(add-hook! 'dired-mode-hook
  (dired-hide-details-mode 1)
  (dired-omit-mode 1)
  (my/dired-disable-vc-for-remote)
  (+zen/toggle))

;; (add-hook 'denote-backlinks-mode-hook #'+zen/toggle)

(setq doom-projectile-fd-binary "fd")

(pixel-scroll-precision-mode 1)



(load! "package-config.el")
(load! "functions.el")
(load! "keybindings.el")

;; (connection-local-set-profiles '(:protocol "ssh") 'use-own-path)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(add-to-list 'tramp-remote-path "/home/yonatan/anaconda3/bin/conda")

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t."
  (ad-set-arg 1 t))

(setq gc-cons-threshold (* 32 1024 1024))

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 32 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(delete-selection-mode 1)

(setq dired-kill-when-opening-new-dired-buffer t)
