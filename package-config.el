;;; ~/.doom.d/package-config.el -*- lexical-binding: t; -*-

(use-package! org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("\u200b" "\u200b" "*" "\u200b" "●" "○" "✿"  "●" )
        inhibit-compacting-font-caches t)
  )

;; (use-package helm
;;   :init
;;   (setq helm-ff-fuzzy-matching t))

 ;; (after! helm
;;   (setq helm-ff-fuzzy-matching t))

;; (use-package! pdf-tools
;;   :config
;;   (setq pdf-view-display-size 'fit-width
;;         pdf-view-resize-factor 1.1))


(use-package org
  :config
  (setq
    bidi-paragraph-direction nil
    org-id-link-to-org-use-id 'create-if-interactive
    org-id-method 'ts
    org-outline-path-complete-in-steps nil
    org-goto-interface 'outline-path-completion
    org-cycle-separator-lines 1
    ;; org-image-actual-width nil
    org-export-with-toc 'nil
    org-image-actual-width (list 550)
    org-ellipsis "…"
    ;; ➡, ⚡, ▼, ↴, ∞, ⬎, ⤷, ⤵, …
    org-deadline-warning-days 7
    org-agenda-breadcrumbs-separator " ❱ "
    org-odd-levels-only  t
    org-startup-with-inline-images t
    org-hide-emphasis-markers t
    org-list-indent-offset 2
    org-list-demote-modify-bullet
       '(("+" . "*") ("-" . "+") ("*" . "-"))
    ;; org-agenda-files '(
    ;;                    "~/google_drive/notes/gtd/inbox.org"
    ;;                    "~/google_drive/notes/gtd/reminders.org"
    ;;                    "~/google_drive/notes/gtd/projects.org"
    ;;                    "~/google_drive/notes/gtd/someday.org"
    ;;                    "~/google_drive/notes/gtd/writing_inbox.org")

    org-refile-targets '(
                         ( org-capture-projects-file :maxlevel . 1)
                         ( org-capture-someday-file :level . 1)
                         ( org-capture-inbox-file :maxlevel . 2)
                         (nil . (:maxlevel . 9)) ;; current buffer
                         ( org-capture-reminders-file :maxlevel . 1))

    org-todo-keywords '(
                        (sequence "TODO(t)" "NEXT(n)" "READ(r)" "NOTE(N)" "|" "DONE(d)")
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

      ("l" "literature follow up"
       (
        (todo "READ" (
                           (org-agenda-overriding-header "\n⚡ Next up:\n")
                           (org-agenda-remove-tags t)
                           ;; (org-agenda-prefix-format (concat " %b  %-2i  %t%s" ))
                           (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                                       (timeline . "  % s")
                                                       (todo .
                                                             " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
                                                       (tags .
                                                             " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
                                                       (search . " %i %-12:c"))
                                                     )
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

                            ("p" "Paper ref to read "
                             entry
                             (file+headline org-capture-papers-file "Misc")
                             "* TODO %? %i")

                            ("n" "Note"
                               entry
                               (file+headline org-capture-writing-inbox-file "Notes")
                               "* NOTE %? \n")

                              ("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")

                              ("i" "Interesting things"
                               entry
                               (file+headline org-capture-someday-file "To read/watch")
                               "** %? :bucket_list:\n")

                              ("a" "Anki basic"
                               entry
                               (file+headline org-my-anki-file "Waiting for export")
                               ;; "* %<%H:%M> \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: %^{deck?|School|Master|Research}\n:END:\n** Front\n\t%?\n** Back\n\t%i\n** Extra\n\t- source:")
                               "* %<%H:%M> \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: School\n:END:\n** Front\n\t%?\n** Back\n\t%i\n** Extra\n\t- source:")

                              ("A" "Anki close"
                               entry
                               (file+headline org-my-anki-file "Waiting for export")
                               "* %<%H:%M> \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: School\n:END:\n** Text\n\t%?\n** Extra\n\t- source:")

                              )
    )

  (setq org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
         ("begin" "$1" "$" "$$" "\\(" "\\["))))
  )

(use-package! org-roam
  :init
  (map!
        :leader
        :prefix "k"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today

        :map org-roam-preview-map :desc "universal argument" "C-u" #'universal-argument
        :map org-roam-mode-map :desc "universal argument" "C-u" #'universal-argument
        )
  (setq
   org-roam-directory "~/Documents/notes/slip-box"
   ;; org-roam-directory (file-truename slip-box-dir)
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (dedicated . t)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  :config
  ;; (setq org-roam-mode-sections
  ;;       (list #'org-roam-backlinks-insert-section
  ;;             #'org-roam-reflinks-insert-section
  ;;             ;; #'org-roam-unlinked-references-insert-section
  ;;             ))
  (org-roam-db-autosync-mode)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y-%m-%d-%H%M%S>-${slug}.org"
                              ;; "${title}\n- tags :: \n")
                              "#+title: ${title}\n- tags :: \n")
           :unnarrowed t)))

  (defun my/org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
)

;; Since the org module lazy loads org-protocol (waits until an org URL is
;; detected), we can safely chain `org-roam-protocol' to it.
;; (use-package org-roam-protocol
;;   :after org-protocol)

(defun my/get-bib-file-list ()
  "Get the list of all the bib files containing my bib database."
  (mapcan (lambda (dir) (directory-files dir t "\\.bib\\'"))
'("~/Documents/bibliography")))


(use-package hydra
  :defer 5
  :bind (("C-c C-w" . hydra-window-resize/body)
         ("C-c  C-u" . hydra-outline/body)
         ("C-x  C-m " . multiple-cursors-hydra/body)
         ("C-x  C-'" . hydra-fold/body))
  :config
  ;; (defhydra hydra-expand-region ()
  ;;   "region: "
  ;;   ("k" er/expand-region "expand")
  ;;   ("j" er/contract-region "contract"))
  ;; (general-def 'visual 'global "v" 'hydra-expand-region/body)

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


;; (set-company-backend! 'matlab-shell-mode '(company-capf company-matlab-shell company-dabbrev))
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

(defun my-generic-ispell-company-complete-setup ()
  (when ispell-complete-word-dict
    (let*
      (
        (has-dict-complete
          (and ispell-complete-word-dict (file-exists-p ispell-complete-word-dict)))
        (has-dict-personal
          (and ispell-personal-dictionary (file-exists-p ispell-personal-dictionary)))
        (is-dict-outdated
          (and
            has-dict-complete has-dict-personal
            (time-less-p
              (nth 5 (file-attributes ispell-complete-word-dict))
              (nth 5 (file-attributes ispell-personal-dictionary))))))

      (when (or (not has-dict-complete) is-dict-outdated)
        (with-temp-buffer

          ;; Optional: insert personal dictionary, stripping header and inserting a newline.
          (when has-dict-personal
            (insert-file-contents ispell-personal-dictionary)
            (goto-char (point-min))
            (when (looking-at "personal_ws\-")
              (delete-region (line-beginning-position) (1+ (line-end-position))))
            (goto-char (point-max))
            (unless (eq ?\n (char-after))
              (insert "\n")))

          (call-process "aspell" nil t nil "-d" "en_US" "dump" "master")
          ;; Case insensitive sort is important for the lookup.
          (let ((sort-fold-case t))
            (sort-lines nil (point-min) (point-max)))
          (write-region nil nil ispell-complete-word-dict))))))

(after! citar
  (setq! citar-bibliography (my/get-bib-file-list))
  (setq! citar-at-point-function 'embark-act)
  (setq! citar-file-note-org-include '(org-id org-roam-ref))
  (setq! citar-notes-paths literature-notes-dir)
  (setq! citar-library-paths (list "~/Documents/bibliography"))
  (setq citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                                   (suffix . "${tags keywords keywords:*}   ${=key= id:15}    ${=type=:12}")
                                   (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                                   (note .
"Notes on \"${title}\" (${author})

* General notes


* Summary and short reference


* See also (notes, tags, related papers):

")))

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

  )

(after! consult
(consult-customize
 consult-buffer consult-buffer-other-window consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 ;; consult--source-file consult--source-project-file consult--source-bookmark
 :preview-key (kbd "M-."))
  ;; Replace bindings. Lazily loaded due by `use-package!'.
  ;; :bind (;; C-c bindings (mode-specific-map)
  ;;        ("C-c h" . consult-history)
  ;;        ("C-c m" . consult-mode-command)
  ;;        ("C-c b" . consult-bookmark)
  ;;        ("C-c k" . consult-kmacro)
  ;;        ;; C-x bindings (ctl-x-map)
  ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
  ;;        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ("s-b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ;;        ("C-s-b" . consult-buffer-other-window)
  ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;;        ;; Custom M-# bindings for fast register access
  ;;        ("M-#" . consult-register-load)
  ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;        ("C-M-#" . consult-register)
  ;;        ;; Other custom bindings
  ;;        ("M-y" . consult-yank-from-kill-ring)                ;; orig. yank-pop
  ;;        ("<help> a" . consult-apropos)            ;; orig. apropos-command
  ;;        ;; M-g bindings (goto-map)
  ;;        ("M-g e" . consult-compile-error)
  ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ;;        ("M-g o" . consult-outline)
  ;;        ("M-g m" . consult-mark)
  ;;        ("M-g k" . consult-global-mark)
  ;;        ("C-x C-SPC" . consult-mark)
  ;;        ("M-g i" . consult-imenu)
  ;;        ("M-g I" . consult-project-imenu)
  ;;        ;; M-s bindings (search-map)
  ;;        ("M-s f" . consult-find)
  ;;        ("M-s L" . consult-locate)
  ;;        ("M-s g" . consult-grep)
  ;;        ("M-s G" . consult-git-grep)
  ;;        ("M-s r" . consult-ripgrep)
  ;;        ("C-c f" . consult-ripgrep)
  ;;        ("M-s l" . consult-line)
  ;;        ("M-s m" . consult-multi-occur)
  ;;        ("M-s k" . consult-to-ivy)
  ;;        ("s-r" . consult-recent-file)
  ;;        ("C-c o" . consult-file-externally)
  ;;        ("s-4" . consult-bookmark)
  ;;        ("C-y" . yank)
  ;;        ("C-s" . consult-line) ;; I've long favored Swiper mapped to c-s
  ;;        ("s-l" . consult-goto-line)
  ;;        ;; Isearch integration
  ;;        ("M-s e" . consult-isearch)
  ;;        ;; ("s-t" . jnf/consult-find-using-fd)
  ;;        ("s-3" . consult-imenu)
  ;;        ("s-#" . consult-project-imenu)
  ;;        :map isearch-mode-map
  ;;        ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
  ;;        ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
  ;;        ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch

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

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from:
  ;; * projectile-project-root
  ;; * vc-root-dir
  ;; * project-roots
  ;; * locate-dominating-file
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  ;; (setq consult-project-root-function
  ;;       (lambda ()
  ;;         (when-let (project (project-current))
  ;;           (car (project-roots project)))))
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;; (setq consult-project-root-function
  ;;       (lambda () (locate-dominating-file "." ".git")))


;; Optionally add the `consult-flycheck' command.
(use-package! consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))
 )

(use-package! consult-company
  :config
  (define-key company-mode-map [remap completion-at-point] #'consult-company)
  )
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (use-package! dired-narrow
;;   :bind (:map dired-mode-map
;;               ("/" . dired-narrow)))
;; fix org-open-file for wsl by temporarily replacing start-process-shell-command with call-process-shell-command
;; if we don't do this, emacs on WSL will block forever trying to open exported file with windows handler
(defun wsl-fix-org-open-file (orig-org-open-file &rest args)
  ;; temporarily replace function,
  ;; see https://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'start-process-shell-command) #'call-process-shell-command))
    (apply orig-org-open-file args)))

(advice-add #'org-open-file :around #'wsl-fix-org-open-file)
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

(with-eval-after-load 'citar

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
  )

(with-eval-after-load 'citar
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


(use-package! lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))  ; or lsp-
  :config 
						  
	(setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
	(setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs"))
        )

(use-package! company
  :config

  ;; completion
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t)
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  (map! :map org-mode-map :i
        "C-;" #'+company/complete
        "M-;" #'+company/complete)
  )

(use-package! poetry
  :config
  )


(defun consult-find-for-minibuffer ()
  "Search file with find, enter the result in the minibuffer."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (default-directory (file-name-directory (minibuffer-contents)))
         (file (consult--find
                (replace-regexp-in-string
                 "\\s-*[:([].*"
                 (format " (via find in %s): " default-directory)
                 (minibuffer-prompt))
                #'consult--find-builder
                (file-name-nondirectory (minibuffer-contents)))))
    (delete-minibuffer-contents)
    (insert (expand-file-name file default-directory))
    (exit-minibuffer)))

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


(setq ispell-local-dictionary-alist '("english-hunspell"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "en_US" "-p" "C:\\Users\\Jonathan\\programs\\hunspell\\share\\hunspell\\personal.en")
                                              nil
                                              iso-8859-1))
(add-to-list 'exec-path "C:\\Users\\Jonathan\\programs\\hunspell\\bin")

(setq ispell-program-name (locate-file "hunspell"
                                  exec-path exec-suffixes 'file-executable-p))

(setq ispell-dictionary   "en_US") ; Default dictionary to use

(use-package! company-box
  :hook (company-mode . company-box-mode))


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

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
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
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

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
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

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

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

(use-package! denote
  :config
   ;; Remember to check the doc strings of those variables.
   (setq denote-directory (expand-file-name "~/notes/"))
   (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
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

(map!
 (:map org-mode-map :leader :nv
     "n j" #'my-denote-journal ; our custom command
     "n n" #'denote
     ;; "n N" #'denote-type
     "n d" #'denote-date
     "n z" #'denote-signature ; "zettelkasten" mnemonic
     "n s" #'denote-subdirectory
     "n t" #'denote-template
     ;; If you intend to use Denote with a variety of file types, it is
     ;; easier to bind the link-related commands to the `global-map', as
     ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
     ;; `markdown-mode-map', and/or `text-mode-map'.
     "n I" #'my/denote-link
     "n i" #'denote-link-or-create ; denote-link ; "insert" mnemonic
     "n a" #'denote-link-add-links
     "n b" #'denote-link-backlinks
     "n F" #'denote-link-find-file
     "n B" #'denote-link-find-backlink
     ;; Note that `denote-rename-file' can work from any context, not just
     ;; Dired bufffers.  That is why we bind it here to the `global-map'.
     "n r" #'denote-rename-file
     "n R" #'denote-rename-file-using-front-matter)

   ;; Key bindings specifically for Dired.
 (:map dired-mode-map :leader :nv
                         "ni" #'denote-link-dired-marked-notes
                         "nr" #'denote-dired-rename-marked-files
                         "nR" #'denote-dired-rename-marked-files-using-front-matter))

(map! :map org-mode-map :nvi
     "C-c n j" #'my-denote-journal ; our custom command
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
     "C-c n i" #'my/denote-link-or-create ; "insert" mnemonic
     "[[" #'my/denote-link-or-create ; "insert" mnemonic
     "C-c n I" #'denote-link-add-links
     "C-c n b" #'denote-link-backlinks
     "C-c n f f" #'denote-link-find-file
     "C-c n f b" #'denote-link-find-backlink
     ;; Note that `denote-rename-file' can work from any context, not just
     ;; Dired bufffers.  That is why we bind it here to the `global-map'.
     "C-c n r" #'denote-rename-file
     "C-c n R" #'denote-rename-file-using-front-matter

   ;; Key bindings specifically for Dired.
   :map dired-mode-map
     "C-c C-d C-i" #'denote-link-dired-marked-notes
     "C-c C-d C-r" #'denote-dired-rename-marked-files
     "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter)

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


(use-package! consult-notes
  :config
  (
   map! :map global-map :leader :nv  "nf" #'consult-notes
   )
  )

(add-to-list 'load-path (concat doom-emacs-dir (file-name-as-directory "gptel")))
(require 'gptel)
(setq gptel-api-key (getenv "OPENAI_API_KEY"))
(setq gptel-use-curl nil)
