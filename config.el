;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "Jonathan Sahar"
      user-mail-address "jonathan.sahar@gmail.com")

;; Fonts
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

;; Line numbers
(setq-default display-line-numbers-type 'relative)

;; Various settings
(setq-default line-spacing 0.3)

(setq-default bidi-paragraph-direction nil
              bidi-paragraph-start-re "^"
              bidi-paragraph-separate-re "^")

(setq
 text-scale-mode-step 1.05
 evil-respect-visual-line-mode 't
 delete-by-moving-to-trash nil                      ; Delete files to trash
 uniquify-buffer-name-style nil              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other        windows (not just current)
 x-stretch-cursor t                              ; Stretch cursor to the glyph width
 evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
 garbage-collection-messages nil
 inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
 truncate-string-ellipsis "…"
 visual-fill-column-width 300
 split-window-preferred-function 'visual-fill-column-split-window-sensibly

 dired-dwim-target t

 evil-vsplit-window-right t
 evil-split-window-below t
 evil-escape-delay 0.4
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
 )

(after! org
  (setq org-hide-leading-stars t))
(after! org-modern
  (setq org-modern-star nil))

(+global-word-wrap-mode +1)

(add-hook! 'text-mode-hook 'my-buffer-face-mode-text)
(add-hook! 'text-mode-hook
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (abbrev-mode 1)
  (buffer-face-mode 1)
  (writeroom-mode +1)
  (flyspell-lazy-mode -1))

(add-hook! 'prog-mode-hook 'my-buffer-face-mode-programming)
(add-hook! 'prog-mode-hook
  (flyspell-lazy-mode -1)
  (writeroom-mode +1))

(after! tramp
  (add-to-list 'tramp-remote-path "/home/yonatan/.local/bin")
  (add-to-list 'tramp-remote-path "/home/yonatan/anaconda3/bin"))

(setq which-key-idle-delay 0.2
      which-key-idle-secondary-delay 0.1
      which-key-allow-multiple-replacements t
      ;; Imprecise fit sizes the popup in frame-char-height units, ignoring
      ;; `line-spacing' (0.3 here), so the last row gets clipped.
      which-key-allow-imprecise-window-fit nil)

(defadvice! my/prompt-for-buffer-after-vsplit-a (&rest _)
  :after 'evil-window-vsplit
  (call-interactively #'consult-buffer))

(setq default-input-method 'hebrew-full)
(setq +bidi-hebrew-font (font-spec :family "Heebo"))
(add-to-list 'file-coding-system-alist '("\\.org\\'" . utf-8))

;; width x hight
(defun my/make-small-frame () (interactive) (set-frame-size (selected-frame) 50 42))
(defun my/make-medium-frame () (interactive) (set-frame-size (selected-frame) 110 40))
(defun my/make-large-frame () (interactive) (set-frame-size (selected-frame) 140 47))

;; Frame geometry (add-to-list so Doom's early-init entries survive)
(dolist (p '((height . 47)
             (width . 140)
             (vertical-scroll-bars)
             (tool-bar-lines . 0)
             (menu-bar-lines . 0)
             (left-fringe . 8)
             (right-fringe . 8)))
  (add-to-list 'default-frame-alist p))

(dolist (p '((top . 00)
             (left . 2000) ;; ponytail: hardcoded second-monitor offset
             (height . 50)
             (width . 140)
             (vertical-scroll-bars)
             (tool-bar-lines . 0)
             (menu-bar-lines . 0)
             (left-fringe . 8)
             (right-fringe . 8)))
  (add-to-list 'initial-frame-alist p))

;; Ibuffer and extras (dired-like buffer list manager)
(after! ibuffer
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

  ;; Guard against an ibuffer internal range error seen on `x' (execute marks).
  (defun my/ibuffer-map-lines-safe (orig-fn function &optional nomodify group)
    "Run ORIG-FN and recover once from `args-out-of-range' in ibuffer buffers."
    (condition-case err
        (funcall orig-fn function nomodify group)
      (args-out-of-range
       (let ((inhibit-read-only t))
         (save-excursion
           (goto-char (point-max))
           (unless (or (bobp) (eq (char-before) ?\n))
             (insert "\n"))))
       (funcall orig-fn function nomodify group))))

  (advice-add 'ibuffer-map-lines :around #'my/ibuffer-map-lines-safe))

(after! citar
  (setq citar--multiple-setup (cons "<tab>"  "RET")))
(after! writeroom-mode
  (setq writeroom-mode-line 't))

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
(map! :textobj "l" #'evil-select-inner-line-no-whitespace nil)

;; tabs
(after! tab-bar
  (setopt tab-bar-show nil)
  (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
  (setq tab-bar-close-button-show nil)
  (tab-bar-mode +1)
  (setq tab-bar-new-tab-choice "*doom*")
  (put 'tab-bar-tab-inactive 'face-alias 'tab-bar))

;; Dired
(after! dired
  (setq dired-listing-switches "-laht --group-directories-first")
  (dired-async-mode 1)
  (setq dired-compress-file-alist '(("\\.gz\\'" . "gzip -9f %i") ("\\.bz2\\'" . "bzip2 -9f %i") ("\\.xz\\'" . "xz -9f %i") ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i") ("\\.zip\\'" . "zip %o -r --filesync %i")))

  (setq dired-guess-shell-alist-user
        '(("\\.\\(png\\|jpe?g\\|gif\\|bmp\\|tiff?\\)$" "eog")))

  (setq dired-kill-when-opening-new-dired-buffer t)

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
    (my/dired-disable-vc-for-remote)
    (writeroom-mode +1)))

;; (add-hook 'denote-backlinks-mode-hook #'+zen/toggle)

;; (connection-local-set-profiles '(:protocol "ssh") 'use-own-path)
;; Doom's :tools tree-sitter is elisp-tree-sitter; this alist only serves
;; manual `treesit-install-language-grammar'.
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

;; Always run `compile' in comint mode.
(defadvice! my/compile-comint-a (args)
  :filter-args #'compile
  (list (car args) t))

(setq gc-cons-threshold (* 32 1024 1024))

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 32 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(delete-selection-mode 1)

;; The :emacs undo module sets undo-limit/strong-limit inside undo-fu's
;; :config, which runs on `doom-first-buffer' -- i.e. AFTER any top-level
;; `setq undo-limit', silently clobbering it back to 256kb.
;; Re-assert after the package loads so the intent actually sticks.
;; (undo-outer-limit left at the module's 36mb; nothing here needs it raised.)
(after! undo-fu
  (setq undo-limit        80000000     ; 80mb  (module default: 256kb)
        undo-strong-limit 120000000))  ; 120mb (module default: 2mb)

;; Load the rest of the config last, so nothing runs after them.
(load! "completion")   ; consult / vertico / embark / consult-dir
(load! "ui")           ; tabspaces, popper, treemacs, spacious-padding, highlight-thing, dirvish, vterm, display-buffer rules, buffer-face fns, laptop-mode
(load! "org")          ; org, org-download, capture/agenda/refile, org helpers (image clipboard, move-line, odt)
(load! "notes")        ; denote*, consult-notes, citar, citar-denote, denote-markdown/silo
(load! "repl")         ; jupyter, code-cells, python, eglot, conda, jupytext, jupyter cell-status overlay, jupyter-timer-fix/bridge loads
(load! "ai")           ; gptel, copilot, aidermacs, claude-code, agent-shell, magit-gptcommit
(load! "functions")    ; pure editing helpers only
(load! "keybindings")
