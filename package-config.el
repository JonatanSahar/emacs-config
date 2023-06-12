;;; ~/.doom.d/package-config.el -*- lexical-binding: t; -*-


(use-package! general
  :config
  (general-auto-unbind-keys)
  )

(use-package! dired-filter
  :hook (dired-mode . dired-filter-mode)
  :config
  (evil-define-key* 'normal dired-mode-map "F" #'dired-filter-mode)
  (evil-define-key* 'normal dired-mode-map "f" dired-filter-map)
  (evil-define-key* 'normal dired-mode-map "M" dired-filter-mark-map)

  (setq dired-filter-group-saved-groups
        '("default"
          ("PDF"
           (extension . "pdf"))
          ("LaTeX"
           (extension "tex" "bib"))
          ("Org"
           (extension . "org"))
          ("Archives"
           (extension "zip" "rar" "gz" "bz2" "tar"))))

  )


(use-package! dired-ranger
  :config
  (map! :map dired-mode-map :n "c" nil)
  (evil-define-key* 'normal dired-mode-map "cz" #'dired-do-compress-to)
  (map! :map dired-mode-map :prefix "c" :n
        :desc "add file(s) to copy/move clipboard" "c" #'dired-ranger-copy
        :desc "paste file(s) from clipboard" "p" #'dired-ranger-paste
        :desc "move file(s) from clipboard" "r" #'dired-ranger-move)
  )

(use-package! org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("\u200b" "\u200b" "*" "\u200b" "●" "○" "✿"  "●" )
        inhibit-compacting-font-caches t)
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
   org-hide-emphasis-markers t
   org-list-indent-offset 2
   org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
   org-list-demote-modify-bullet
   '(("+" . "*") ("-" . "+") ("*" . "-"))
   org-capture-papers-file "~/notes/20230402T133604--interesting-papers__thesis.org"
   org-capture-microdosing-journal-file "~/notes/20230523T162209--microdosing-journal__journal.org"
   org-agenda-files '(
                      "~/notes/20230323T113003--knowledge-base__thesis.org"
                      "~/notes/20230402T133604--interesting-papers__thesis.org")

   org-refile-targets '(
                        ;; ( org-capture-projects-file :maxlevel . 1)
                        ;; ( org-capture-someday-file :level . 1)
                        ;; ( org-capture-inbox-file :maxlevel . 2)
                        ("~/notes/20230323T113003--knowledge-base__thesis.org" :level . 1)
                        (nil . (:maxlevel . 9)) ;; current buffer
                        ;; ( org-capture-reminders-file :maxlevel . 1)
                        )

   org-todo-keywords '(
                       (sequence "TODO(t)" "NEXT(n)" "SKIM(s)" "READ(r)" "NOTE(N)" "|" "DONE(d)")
                       (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))

   org-agenda-block-separator " "

   org-agenda-custom-commands
   '(
     ("o" "my agenda"
      (
       (todo "NEXT|HOLD" (
                          (org-agenda-overriding-header "\n⚡ Next up:\n")
                          (org-agenda-remove-tags t)
                          (org-agenda-prefix-format (concat "  %-2i  %t%s" ))
                          ;; (org-agenda-prefix-format (concat "  %-2i %-13b" ))
                          (org-agenda-todo-keyword-format "")))
       (agenda "" (
                   (org-agenda-overriding-header "⚡ Schedule:\n")
                   (org-agenda-start-day "+0d")
                   (org-agenda-span 5)
                   (org-agenda-remove-tags t)
                   (org-agenda-prefix-format   (concat "  %-3i  %t%s"))
                   ;; (org-agenda-prefix-format   (concat "  %-3i  %-15b %t%s"))
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
  )


(defun my/get-bib-file-list ()
  "Get the list of all the bib files containing my bib database."
  (mapcan (lambda (dir) (directory-files dir t "\\.bib\\'"))
          '("~/Documents/bibliography")))


(use-package hydra
  :defer 10
  :bind (
         ("C-c  C-u" . hydra-outline/body)
         ("C-x  C-'" . hydra-fold/body))
  :config

  (defhydra hydra-fold (:pre (hs-minor-mode 1))

    "fold"
    ("t" fold-dwim-toggle "toggle")
    ("h" fold-dwim-hide-all "hide-all")
    ("s" fold-dwim-show-all "show-all")
    ("q" nil "quit"))

  (defun my-funcs/resize-window-down ()
    "Resize a window downwards."
    (interactive)
    (if (window-in-direction 'below)
        (enlarge-window 10)
      (shrink-window 10)))
  (defun my-funcs/resize-window-up ()
    "resize a window upwards."
    (interactive)
    (if (window-in-direction 'above)
        (enlarge-window 10)
      (shrink-window 10)))
  (defun my-funcs/resize-window-left ()
    "resize a window leftwards."
    (interactive)
    (if (window-in-direction 'left)
        (enlarge-window-horizontally 10)
      (shrink-window-horizontally 10)))
  (defun my-funcs/resize-window-right ()
    "resize a window rightwards."
    (interactive)
    (if (window-in-direction 'right)
        (enlarge-window-horizontally 10)
      (shrink-window-horizontally 10)))
  (defhydra hydra-window-resize (global-map "C-c w")
    "Window resizing"
    ("j" my-funcs/resize-window-down "down")
    ("k" my-funcs/resize-window-up "up")
    ("l" my-funcs/resize-window-right "right")
    ("h" my-funcs/resize-window-left "left"))

  (defhydra hydra-outline (:color pink :hint nil)
    "
 ^Hide^             ^Show^           ^Move
 ^^^^^^------------------------------------------------------
 _q_: sublevels     _a_: all         _u_: up
 _t_: body          _e_: entry       _n_: next visible
 _o_: other         _i_: children    _p_: previous visible
 _c_: entry         _k_: branches    _f_: forward same level
 _l_: leaves        _s_: subtree     _b_: backward same level
 _d_: subtree   _<tab>_: cycle
 "
    ;; Hide
    ("q" hide-sublevels)  ; Hide everything but the top-level headings
    ("t" hide-body)    ; Hide everything but headings (all body lines)
    ("o" hide-other)   ; Hide other branches
    ("c" hide-entry)   ; Hide this entry's body
    ("l" hide-leaves)  ; Hide body lines in this entry and sub-entries
    ("d" hide-subtree) ; Hide everything in this entry and sub-entries
    ;; Show
    ("a" show-all)                      ; Show (expand) everything
    ("e" show-entry)                    ; Show this heading's body
    ("i" show-children) ; Show this heading's immediate child sub-headings
    ("k" show-branches) ; Show all sub-headings under this heading
    ("s" show-subtree) ; Show (expand) everything in this heading & below
    ("<tab>" org-cycle)
    ;; Move
    ("u" outline-up-heading)               ; Up
    ("n" outline-next-visible-heading)     ; Next
    ("p" outline-previous-visible-heading) ; Previous
    ("f" outline-forward-same-level)       ; Forward - same level
    ("b" outline-backward-same-level)      ; Backward - same level
    ("z" nil "leave"))

  (defhydra multiple-cursors-hydra (:hint nil)
    "
      ^Up^            ^Down^        ^Other^
 ----------------------------------------------
 [_p_]   Next    [_n_]   Next    [_l_] Edit lines
 [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
 [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
 ^ ^             ^ ^             [_q_] Quit
 "
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))

  (defhydra hydra-origami (:color red)
    "
  _o_pen node    _n_ext fold       toggle _f_orward    _t_oggle recursively
  _c_lose node   _p_revious fold   toggle _a_ll
  "
    ("o" origami-open-node)
    ("t" origami-recursively-toggle-node)
    ("c" origami-close-node)
    ("n" origami-next-fold)
    ("p" origami-previous-fold)
    ("f" origami-forward-toggle-node)
    ("a" origami-toggle-all-nodes))
  )


;; matlab-mode
(setq matlab-verify-on-save-flag nil)
(defadvice! inhibit-real-only-a (oldfun &rest r)
  "Temporary remove read-only lines in shell buffer - fixes problems with completion"
  :around#'matlab-shell-collect-command-output
  (let ((inhibit-read-only t)) (apply oldfun r)))
(defun my-setup-matlab-mode()
  (doom/toggle-line-numbers)
  (visual-line-mode))
(add-hook! matlab-mode #'my-setup-matlab-mode)
(setq matlab-shell-command-switches '("-nodesktop -nosplash "))

(setq completion-ignore-case t)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(after! citar
  (setq! citar-bibliography (my/get-bib-file-list))
  (setq! citar-at-point-function 'embark-act)
  (setq! citar-file-note-org-include '(org-id org-roam-ref))
  (setq! citar-notes-paths literature-notes-dir)
  (setq! citar-library-paths (list "~/Documents/bibliography"))

(after! citar
  (setq citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                          (suffix . "         ${tags keywords keywords:*}   ${=key= id:15}    ${=type=:12}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note .
                                "Notes on \"${title}\", (${author})

* Summary and short reference

* Reading notes

* See also (notes, tags, related papers):

"))))

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

(use-package org-super-agenda
  :config
  (org-super-agenda-mode t)
  (add-to-list 'org-agenda-custom-commands
               '("r" "Categor: Research" todo ""
                 ((org-super-agenda-groups
                   '((:category ("research"))
                     ;; (:category ("research"))
                     (:discard (:anything))
                     ))
                  )))
  )



(use-package! lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ; or lsp-
  :config

  (setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
  (setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs"))
  )

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

(use-package! poetry)

(defun define-minibuffer-key (key &rest defs)
  "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
  (define-key minibuffer-local-map key
    (list 'menu-item nil defs :filter
          (lambda (d)
            (plist-get d (completion-metadata-get
                          (completion-metadata (minibuffer-contents)
                                               minibuffer-completion-table
                                               minibuffer-completion-predicate)
                          'category))))))

(define-minibuffer-key "\C-s"
  'file #'consult-find-for-minibuffer)

(setq ispell-personal-dictionary-en  "C:\\Users\\Jonathan\\programs\\hunspell\\share\\hunspell\\personal.en")
(setq ispell-personal-dictionary-heb  "C:\\Users\\Jonathan\\programs\\hunspell\\share\\hunspell\\personal.heb")
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
(add-to-list 'exec-path "C:\\Users\\Jonathan\\programs\\hunspell\\bin")

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
  :hook (company-mode . company-box-mode)
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
         ;; ("C-`"   . popper-toggle-latest)
         ("C-`"   . #'popper-kill-latest-popup)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Python\\*"
          "\\*MATLAB\\*"
          "\\*Ibuffer\\*"
          "\\*denote-backlinks"
          "\\*chatgpt "
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  (map! :map popper-mode-map :nv "`" #'popper-toggle-latest)
  )                ; For echo area hints

;; (require 'conda)
;; (conda-env-initialize-interactive-shells)
;; (conda-env-initialize-eshell)
;; (conda-env-autoactivate-mode t)
;; (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
;;                                          (conda-env-activate-for-buffer))))
;; (custom-set-variables
;;  '(conda-anaconda-home "c:/Users/Jonathan/miniconda3/"))

(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated      (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

;; (use-package citar-org-roam
;;   :after (citar org-roam)
;;   :config (citar-org-roam-mode))

(use-package! denote
  :config
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-known-keywords '("emacs" "thesis"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)


  ;; Read this manual for how to specify `denote-templates'.  We do not
  ;; include an example here to avoid potential confusion.


  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)

  (setq denote-date-format nil) ; read doc string

  ;; By default, we do not show the context of links.  We just display
  ;; file names.  This provides a more informative view.
  (setq denote-backlinks-show-context t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; showed in this manual.  We define it here and add it to a key binding
  ;; below.
  (defun my-denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal")))

  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:


  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; Also check the commands `denote-link-after-creating',
  ;; `denote-link-or-create'.  You may want to bind them to keys as well
  )

(add-to-list 'load-path (concat doom-emacs-dir (file-name-as-directory "dired-plus")))
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(add-to-list 'load-path (concat doom-emacs-dir (file-name-as-directory "orgnv")))
;;(require 'orgnv)


(add-to-list 'load-path (concat doom-emacs-dir (file-name-as-directory "gptel")))
;; (require 'gptel)
(setq gptel-api-key (getenv "OPENAI_API_KEY")
      gptel-use-curl nil
      gptel-default-mode 'org-mode)


(after! consult
  (consult-customize
   consult-buffer consult-buffer-other-window consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref consult-theme
   ;; consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key "C-.")

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)


  ;; From https://github.com/minad/consult/wiki#find-files-using-fd
  ;; Note: this requires lexical binding
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.

  ;; Optionally configure preview. Note that the preview-key can also be
  ;; configured on a per-command basis via `consult-config'. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; Optionally add the `consult-flycheck' command.
  (use-package! consult-flycheck
    :bind (:map flycheck-command-map
                ("!" . consult-flycheck)))


  :config

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  (map! :map evil-normal-state-map
   "M-e" #'consult-isearch-forward
   )
  )

(use-package! consult-company
  :config
  ;; (define-key company-mode-map [remap completion-at-point] #'consult-company)
  )

(use-package! consult-notes
  :after denote
  :init
  (consult-notes-denote-mode)
  :commands (consult-notes)
  :config
  (setq consult-notes-denote-display-id nil)
    )
  ;; (consult-customize consult-notes
  ;;                  :preview-key "C-.")
  ;;                  ;; '("M-."
  ;;                  ;;   :debounce 0.5 "<up>" "<down>"
  ;;                  ;;   :debounce 0.5 any))
  ;; )

;; (map! :after consult-notes :leader :prefix "n" :nv "f" #'consult-notes)

(map!
   ;; :after denote
   (:map org-mode-map :leader
         (:map org-mode-map :leader
               ;; (:prefix "n"
               ;;          (:prefix ("d" ."dired")
               ;;           :nv "r" #'denote-rename-file
               ;;           :nv "R" #'denote-rename-file-using-front-matter)
               ;;          (:prefix ("k" . "keywords")
               ;;                   "a" #'denote-keywords-add
               ;;                   "r" #'denote-keywords-remove)
               ;;          ))
         (:prefix "n"
                :nv "o" #'denote-open-or-create
                :nv "f" #'denote-open-or-create
                ;; :nv "f" #'my/consult-notes
                :nv "j" #'my-denote-journal ; our custom command
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
                :nv "a" #'denote-link-add-links
                :nv "b" (lambda nil (interactive) (denote-link-backlinks) (windmove-down)) ;;(revert-buffer-with-coding-system 'utf-8))
                :nv "F" #'denote-link-find-file
                :nv "B" #'denote-link-find-backlink))
   )

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
         "C-c n i" #'denote-link-or-create ; "insert" mnemonic
         "[[" #'denote-link-or-create
         "C-c n a" #'denote-link-add-links
         "C-c n b" #'denote-link-backlinks
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
         "b" #'denote-link-backlinks
         "f f" #'denote-link-find-file
         "f b" #'denote-link-find-backlink
         "k a" #'denote-keywords-add
         "k r" #'denote-keywords-remove
         ;; Note that `denote-rename-file' can work from any context, not just
         ;; Dired bufffers.  That is why we bind it here to the `global-map'.
         "r" #'denote-rename-file
         "R" #'denote-rename-file-using-front-matter))

(use-package! citar-denote
  :init
  (citar-denote-mode)
  (defun citar-denote-link-reference ()
    "Insert a Denote link to a bibliographic note."
    (interactive)
    (if-let* ((citekey (citar-select-refs
                        :filter (citar-denote-has-notes)
                        :multiple nil))
              (files (gethash (car citekey)
                              (citar-denote-get-notes citekey)))
              (file (if (= (length files) 1)
                        (car files)
                      (funcall project-read-file-name-function
                               "Select note: "
                               files nil nil nil))))
        (denote-link file)))
  :bind
  (
   :map org-mode-map
   ("C-c n c c" . citar-create-note)
   ("C-c n c o" . citar-denote-open-note)
   ("C-c n c d" . citar-denote-dwim)
   ("C-c n c a" . citar-denote-add-citekey)
   ("C-c n c k" . citar-denote-remove-citekey)
   ("C-c n c e" . citar-denote-open-reference-entry)
   ("C-c n c r" . citar-denote-find-reference)
   ("C-c n c f" . citar-denote-find-citation)
   ("C-c n c n" . citar-denote-cite-nocite)
   ("C-c n c m" . citar-denote-reference-nocite)

   ("C-n c c" . citar-create-note)
   ("C-n c o" . citar-denote-open-note)
   ("C-n c d" . citar-denote-dwim)
   ("C-n c a" . citar-denote-add-citekey)
   ("C-n c k" . citar-denote-remove-citekey)
   ("C-n c e" . citar-denote-open-reference-entry)
   ("C-n c r" . citar-denote-find-reference)
   ("C-n c f" . citar-denote-find-citation)
   ("C-n c n" . citar-denote-cite-nocite)
   ("C-n c m" . citar-denote-reference-nocite)
   ("C-n c i" . citar-denote-link-reference)
   ("C-n c I" . org-cite-insert)
   ))

(map!
 :leader :map evil-org-mode-map "nc" nil)


(map!
 ;; :after citar-denote
 :leader
 :map evil-org-mode-map
 (:prefix "n"
          (:prefix "c"
                   :desc "opent the bibliography list" "c" #'citar-open
                   :desc "create a note for ref" "n" #'citar-create-note
                   :desc "open a note belonging to ref" "o" #'citar-denote-open-note
                   :desc "act on this note's ref" "d" #'citar-denote-dwim
                   :desc "add ref to this note" "a" #'citar-denote-add-citekey
                   :desc "remove ref from this note" "k" #'citar-denote-remove-citekey
                   :desc "go to bibtex entry" "e" #'citar-denote-open-reference-entry
                   :desc "notes citing the current ref" "r" #'citar-denote-find-reference
                   :desc "notes citing a given ref"  "R" #'citar-denote-find-citation
                   :desc "open a note belonging to ref"  "f" #'citar-denote-open-note
                   ;; "n" #'citar-denote-cite-nocite
                   ;; "m" #'citar-denote-reference-nocite
                   "i" #'citar-denote-link-reference
                   "I" #'org-cite-insert)))

;; (use-package! org-transclusion
;;   :after org
;;   :config
;; (defun denote-org-transclusion-add (link plist)
;;   (when (string= "denote" (org-element-property :type link))
;;     (let* ((denote-id (org-element-property :path link))
;;            (text-after-double-colon (car (cdr (split-string denote-id "::"))))
;;            ;; (text-after-double-colon nil)
;;            (denote-id (car (split-string denote-id "::")))
;;            (file-path (denote-get-path-by-id denote-id))

;;            (new-link (with-temp-buffer
;;           (insert "file:")
;;                        (insert file-path)
;;                        (when text-after-double-colon
;;                          (insert "::")
;;                          (insert text-after-double-colon))
;;                        (beginning-of-buffer)
;;                        (org-element-link-parser))))
;;       (org-transclusion-add-org-file new-link plist))))
;;   (map!
;;    :map global-map "<f12>" #'org-transclusion-add
;;    :leader
;;    :prefix "n"
;;    :desc "Org Transclusion Mode" "t" #'org-transclusion-mode)

;;         (setq org-transclusion-exclude-elements '(property-drawer keyword))
;;         (cl-pushnew 'denote-org-transclusion-add                     ;; register the org transclusion 'plugin'
;;                     org-transclusion-add-functions)
;;         (org-transclusion-mode 1)
;;   )

(add-hook! 'helm-minibuffer-set-up-hook '+zen/toggle)
;; (use-package! org-remark)
;; (org-remark-global-tracking-mode +1)
;; (map! :map global-map :leader (:prefix "n" :prefix "m" :nv "M" #'org-remark-mark))
;; (map! :after org-remark  :map org-remark-mode-map :leader (:prefix "n" :prefix "m"
;;                                                     :nv "M" #'org-remark-open
;;                                                     :nv "M" #'org-remark-view-next
;;                                                     :nv "M" #'org-remark-view-prev
;;                                                     :nv "M" #'org-remark-remove))


(use-package! org-modern
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

;; (use-package! mono-complete
;;   :config
;;   (setq mono-complete-fallback-command 'tab-to-tab-stop)
;;   (define-key mono-complete-mode-map (kbd "<tab>") 'mono-complete-expand-or-fallback)
;;   (evil-define-key* 'insert mono-complete-mode-map  "C-;" #'mono-complete-expand-or-fallback)
;;   (map! :map mono-complete-mode-map :i "C-;" nil)
;;   (map! :map mono-complete-mode-map :i "C-;" #'mono-complete-expand-or-fallback)
;;   (map! :map mono-complete-mode-map :i "RET" #'mono-complete-expand-or-fallback)

;;   :commands (mono-complete-mode)
;;   ;; :hook
;;          ;; ((prog-mode) . mono-complete-mode)
;;          ;; ((text-mode) . mono-complete-mode)
;;   )

(setq external-packages-dir (concat doom-emacs-dir (file-name-as-directory "packages")))

(add-to-list 'load-path (concat external-packages-dir (file-name-as-directory "dired-plus")))
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)

(add-to-list 'load-path (concat external-packages-dir (file-name-as-directory "orgnv")))
(require 'orgnv)


;; (add-to-list 'load-path (concat external-packages-dir (file-name-as-directory "gptel")))
;; (require 'gptel)

;; (add-to-list 'load-path (concat external-packages-dir (file-name-as-directory "chatgpt-shell")))
;; (require 'chatgpt-shell)
;; (setq chatgpt-shell-openai-key (getenv "OPENAI_API_KEY"))

(use-package! gptel
 :config
(setq gptel-api-key (getenv "OPENAI_API_KEY")
      gptel-use-curl 't
      gptel-default-mode 'org-mode)
 )

(defvar gptel-quick--history nil)

(defun gptel-quick (prompt)
  (interactive (list (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       "")))
  (let ((user-prompt (read-string "Ask ChatGPT: " nil 'gptel-quick--history))))
  ;; (interactive (list (read-string "Ask ChatGPT: " nil gptel-quick--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  (gptel-request
   prompt
   :callback
   (lambda (response info)
     (if (not response)
         (message "gptel-quick failed with message: %s" (plist-get info :status))
       (with-current-buffer (get-buffer-create "*gptel-quick*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert response))
         (special-mode)
         (display-buffer (current-buffer)
                         `((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . ,#'fit-window-to-buffer))))))))

(defun gptel-rewrite-and-replace (bounds &optional directive)
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     ((derived-mode-p 'text-mode)
      (list (bounds-of-thing-at-point 'sentence)))
     (t (cons (line-beginning-position) (line-end-position))))
    (and current-prefix-arg
         (read-string "ChatGPT Directive: "
                      "You are a prose editor. Rewrite my prompt more professionally."))))
  (gptel-request
   (buffer-substring-no-properties (car bounds) (cdr bounds)) ;the prompt
   :system (or directive "You are a prose editor. Rewrite my prompt more professionally.")
   :buffer (current-buffer)
   :context (cons (set-marker (make-marker) (car bounds))
                  (set-marker (make-marker) (cdr bounds)))
   :callback
   (lambda (response info)
     (if (not response)
         (message "ChatGPT response failed with: %s" (plist-get info :status))
       (let* ((bounds (plist-get info :context))
              (beg (car bounds))
              (end (cdr bounds))
              (buf (plist-get info :buffer)))
         (with-current-buffer buf
           (save-excursion
             (goto-char beg)
             (kill-region beg end)
             (insert response)
             (set-marker beg nil)
             (set-marker end nil)
             (message "Rewrote line. Original line saved to kill-ring."))))))))

;; (use-package! shackle
;; :config
;; (setq shackle-rules '(;; Customize rules here
;;                       (compilation-mode :align below :size 0.3 :select t)
;;                       ("*shell*" :align below :size 0.3 :select t)
;;                       ("*MATLAB*" :align below :size 0.3 :select t)
;;                       ("*Ibuffer*" :align below :size 0.3 :select t)
;;                       ("*Help*" :popup t :align right :size 0.4 :select t)
;;                       ("*Apropos*" :popup t :size 0.3 :align below)
;;                       )))
