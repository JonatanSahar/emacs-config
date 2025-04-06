;;; ~/.doom.d/keybindings.el -*- lexical-binding: t; -*-

;; Custom Functions & Macros
;; =========================

(defun my-evil-end-of-visual-line ()
  "Wrapper for evil-end-of-visual-line that preserves visual selection."
  (interactive)
  (evil-end-of-visual-line))

(defun my/save-and-change-to-normal ()
  (interactive)
  (evil-normal-state)
  (save-buffer))

(defun my/python-eval-line-or-defun ()
  (interactive)
  (if (and (or (eq major-mode 'python-mode)
               (eq major-mode 'python-ts-mode))
           (or (eq evil-state 'normal)
               (eq evil-state 'insert))
           (save-excursion
             (beginning-of-line)
             (looking-at "^[ \t]*def ")))
      (jupyter-eval-defun)
    (jupyter-eval-line-or-region)))

(defun my-shell nil (interactive) (shell) (popper-toggle-type) (evil-normal-state))

(defun make-bold()
  (interactive)
  (if (use-region-p) (evil-surround-region (region-beginning) (region-end) t *)))

(defun my/force-delete-frame ()
  "Force close a frame without prompting."
  (interactive)
  (delete-frame nil t)) ;; The second argument (force) makes it close without confirmation.

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

(fset 'copy-with-square-brackets
      (kmacro [?y ?a ?\]] 0 "%d"))


;; Keybindings
;; ===========

;; Global Bindings (Apply Everywhere)
;; ----------------------------------
(map! :nvi ; These apply in normal, visual, insert states
      ;; Basic Editing & Navigation
      "C-z" #'evil-undo
      "C-y" #'evil-redo
      "C-s" #'my/save-and-change-to-normal ; Save and return to normal mode
      [C-o] #'better-jumper-jump-backward
      [C-i] #'better-jumper-jump-forward
      ;; Window Management
      "M-q" #'+workspace/other ; Switch workspace/window
      "C-p" #'delete-other-windows ; Close other windows
      ;; Input & System
      "C-\\" #'toggle-input-method
      "M-;" #'toggle-input-method
      ;; Projectile
      "C-S-p" #'projectile-find-file
      ;; Yank/Paste/Delete (using C-c prefix)
      "C-c c" #'evil-yank
      "C-c y" #'evil-yank
      "C-c v" #'consult-yank-from-kill-ring
      "C-c d" #'evil-delete
      "C-c x" #'evil-delete
      "C-c C-r" #'evil-redo ; Redo via C-c prefix
      ;; Paste Ring
      "M-p" #'evil-paste-pop
      "M-n" #'evil-paste-pop-next
      ;; Drag Stuff
      "M-j" #'drag-stuff-down
      "M-k" #'drag-stuff-up
      ;; Tab Navigation
      "M-1" #'tab-bar-select-tab
      "M-2" #'tab-bar-select-tab
      "M-3" #'tab-bar-select-tab
      "M-4" #'tab-bar-select-tab
      "M-5" #'tab-bar-select-tab
      ;; Embark
      "C-;" #'embark-act
      ;; GPTel Quick Send (Region aware)
      "C-c C-<return>" #'gptel-send
      ;; Smerge Hydra Activation
      "C-c s" #'hydra-smerge/body
      "C-c m" #'hydra-smerge/body
      ;; Flyspell Correction (Global Key)
      [f11] #'flyspell-correct-at-point
      ;; Unbind potentially conflicting insert mode keys
      :i "C-S-j" nil
      :i "C-S-k" nil
      ;; Unbind C-x C-n
      "C-x C-n" nil)

;; Evil Mode States (Normal, Visual, Insert specific)
;; --------------------------------------------------
(map! ;; Normal Mode (:n)
      :n "k" #'evil-previous-visual-line
      :n "j" #'evil-next-visual-line
      :n "<up>"   (lambda () (interactive) (scroll-down-command 1)) ; Scroll view down
      :n "<down>" (lambda () (interactive) (scroll-up-command 1)) ; Scroll view up
      :n "h" #'left-char ; Ensure h/l move by char even with visual-line-mode
      :n "l" #'right-char
      :n [left] #'left-char
      :n [right] #'right-char
      :n "J" #'evil-join ; Join lines (standard Vim J)
      :n "K" #'join-line ; Join lines without space (like Emacs default)
      :n "gO" #'+evil/insert-newline-above
      :n "go" #'+evil/insert-newline-below
      :n "gK" #'+evil/insert-newline-above ; Alternative binding
      :n "gJ" #'+evil/insert-newline-below ; Alternative binding
      :n "g]" #'kmacro-end-an-call-macro ; Run last keyboard macro
      :n "g." #'er/expand-region ; Expand region
      :n "gh" #'avy-goto-char-timer ; Avy jump
      :n "gf" #'execute-extended-command ; M-x
      :n "g+" #'evil-numbers/inc-at-pt ; Increment number
      :n "z=" #'my/indent-buffer ; Indent entire buffer
      :n "!" #'+workspace/close-window-or-workspace ; Close window/workspace (vterm override)
      ;; Flyspell Correction (g prefix)
      :n "gl" #'(lambda () (interactive) (call-interactively #'evil-next-flyspell-error) (call-interactively #'flyspell-correct-at-point))
      :n "gh" #'(lambda () (interactive) (call-interactively #'evil-prev-flyspell-error) (call-interactively #'flyspell-correct-at-point))
      ;; Flyspell Correction (z prefix)
      (:prefix "z"
       :nv "z" #'(lambda () (interactive) (call-interactively #'evil-next-flyspell-error) (call-interactively #'flyspell-correct-at-point))
       :nv "Z" #'(lambda () (interactive) (call-interactively #'evil-prev-flyspell-error) (call-interactively #'flyspell-correct-at-point)))
      ;; Multiple Cursors Hydra (g prefix)
      (:when (modulep! :editor multiple-cursors)
        :prefix "g"
        :nv "z" #'my/mc-hydra/body)

      ;; Visual Mode (:v)
      :v "<up>" #'evil-previous-visual-line
      :v "<down>" #'evil-next-visual-line
      :vn "gs" nil ; Unbind gs (often used for surround)

      ;; Normal & Visual Mode (:nv)
      :nv "E" #'evil-forward-WORD-end
      :nv "W" #'evil-forward-WORD-begin
      :nv "'" #'evil-goto-mark ; Jump to mark
      :nv "C-e" #'my-evil-end-of-visual-line ; Go to end of visual line
      ;; Windmove
      :nv "C-l" #'windmove-right
      :nv "C-h" #'windmove-left
      :nv "C-j" #'windmove-down
      :nv "C-k" #'windmove-up
      ;; Paste with C-c prefix
      :nv "C-c p" #'evil-paste-after
      :nv "C-c P" #'evil-paste-before

      ;; Insert Mode (:i)
      :i "C-Z" #'evil-emacs-state ; Enter Emacs state
      :i "C-M-SPC" #'evil-normal-state ; Enter Normal state
      :i "S-<return>" #'evil-normal-state ; Enter Normal state
      ;; Basic navigation in Insert mode (using Control+Shift or Control)
      :i "C-S-j" #'next-line ; Move down (might conflict with MC binding below, check behavior)
      :i "C-S-k" #'previous-line ; Move up (might conflict with MC binding below, check behavior)
      :i "C-'" #'right-char
      :i "C-;" #'left-char
      :i "C-k" #'previous-line
      :i "C-j" #'next-line
      :i "C-l" #'right-char
      :i "C-S-l" #'right-word
      :i "C-h" #'left-char
      :i "C-S-h" #'left-word
      ;; Paste with C-c prefix / C-v
      :i "C-v" #'(lambda () (interactive) (backward-char) (evil-paste-after 1))
      :i "C-c p" #'(lambda () (interactive) (backward-char) (evil-paste-after 1))
      :i "C-c P" #'(lambda () (interactive) (backward-char 2) (evil-paste-after 1))
      ;; Multiple Cursors in Insert mode
      :ni "C-S-j" #'evil-mc-make-cursor-move-next-line
      :ni "C-S-k" #'evil-mc-make-cursor-move-prev-line
      )

;; Mode Specific Bindings
;; ======================

;; Prog Mode (Base for programming modes)
;; --------------------------------------
(map! :map prog-mode-map
      ;; Flyspell correction (C-c prefix) - Inherited by many prog modes
      :ni "C-c +" #'(lambda () (interactive) (call-interactively #'evil-next-flyspell-error) (call-interactively #'flyspell-correct-at-point))
      :ni "C-c =" #'(lambda () (interactive) (call-interactively #'evil-prev-flyspell-error) (call-interactively #'flyspell-correct-at-point)))

;; Text Mode (Base for text modes)
;; -------------------------------
(map! :map text-mode-map
      ;; Flyspell correction (C-c prefix) - Inherited by Org, Markdown etc.
      :ni "C-c +" #'(lambda () (interactive) (call-interactively #'evil-next-flyspell-error) (call-interactively #'flyspell-correct-at-point))
      :ni "C-c =" #'(lambda () (interactive) (call-interactively #'evil-prev-flyspell-error) (call-interactively #'flyspell-correct-at-point)))

;; Org Mode
;; --------
(map! :map (org-mode-map evil-org-mode-map) ; Bindings for both maps
      ;; Basic Navigation & Editing (Inherited/Overridden from Global/Evil)
      :nvi [C-o] #'better-jumper-jump-backward
      :nvi [C-i] #'better-jumper-jump-forward
      :nvi "C-l" #'windmove-right ; Ensure windmove works in Org states
      :nvi "C-h" #'windmove-left
      :nvi "C-j" #'windmove-down
      :nvi "C-k" #'windmove-up
      :nv "k" #'evil-previous-visual-line ; Visual line movement
      :nv "j" #'evil-next-visual-line
      :nv "gk" #'evil-previous-visual-line ; Alternative visual line movement
      :nv "gj" #'evil-next-visual-line ; Alternative visual line movement
      :nv "gj" #'org-forward-element ; Override global `gj` for Org element navigation
      :nv "V" #'evil-visual-screen-line ; Visual line selection (screen lines)
      :i "M-h" #'org-beginning-of-line ; Move to beginning of line in insert
      :i "M-l" #'org-end-of-line ; Move to end of line in insert
      :i "C-h" #'evil-window-left ; Window navigation in insert
      :i "C-l" #'evil-window-right
      :i "C-k" #'evil-window-up
      :i "C-j" #'evil-window-down
      ;; Org Structure & Elements
      :vn "zh" #'org-up-element ; Move up structurally in visual mode
      :vn "zj" #'org-forward-heading-same-level ; Move to next heading in visual mode
      :vn "zk" #'org-backward-heading-same-level ; Move to previous heading in visual mode
      :vn "zl" #'org-next-visible-heading ; Move to next visible heading in visual mode
      :nvi "C-c h" #'org-toggle-heading ; Toggle heading/text
      :nvi "C-c i" #'org-toggle-item ; Toggle list item checkbox/bullet
      :ni "M-j" #'my/org-meta-down ; Custom move line/heading down
      :ni "M-k" #'my/org-meta-up ; Custom move line/heading up
      ;; Capture & Agenda
      :nvi "C-c a" #'(lambda () (interactive) (org-capture nil "a")) ; Capture with 'a' template
      :nvi "C-c o" #'(lambda () (interactive) (org-agenda nil "o")) ; Open agenda with 'o' view
      :nvi "C-c k" #'org-capture ; Default org-capture binding
      :nvi "C-c C-C" #'org-capture ; Another org-capture binding
      "C-x C-x" #'org-capture ; Global-like capture binding
      "C-x C-n" #'org-capture ; Alternative global-like capture binding
      ;; Org Roam / Ref / Cite / Denote
      :ni "C-{" #'org-roam-node-insert ; Insert Org Roam node link
      :ni "C-}" #'org-ref-insert-link ; Insert Org Ref link (legacy?)
      :ni "C-c I" #'org-cite-insert ; Insert citation (Org Cite)
      :nvi "C-S-l" #'org-roam-insert ; Insert Org Roam node link (alternative)
      :n "g[" #'denote-link-or-create ; Create or link Denote note (g prefix)
      :ni "C-c [" #'denote-link-or-create ; Create or link Denote note (C-c prefix)
      ;; Flyspell
      :n "z=" #'flyspell-correct-at-point ; Correct word at point
      :n "g=" #'flyspell-correct-at-point ; Correct word at point (alternative)
      ;; Other Org Functionality
      :nvi "C-c S" #'my/org-screenshot ; Take screenshot and insert link
      :nvi "C-c s" #'evil-Surround-region ; Surround region (useful in Org)
      "<f9>" #'+eval/region ; Evaluate code block/region (Doom specific?)
      :localleader :n "E" #'my/export-org-to-docx ; Custom export
      :localleader :n "n" #'org-narrow-to-subtree ; Narrow view to subtree
      :localleader :n "N" #'widen ; Widen view
      )
(map! :map org-roam-backlinks-mode-map "return" #'org-open-at-point) ; Open link in backlinks buffer

;; Python
;; ------
(map! :map (python-mode-map python-ts-mode-map)
      ;; Jupyter Integration
      :nv "C-<return>" #'jupyter-eval-line-or-region
      :nv "S-<return>" #'jupyter-eval-line-or-region ; Alternative
      :v "C-c <return>" #'python-shell-send-region ; Send region to shell (standard python.el)
      :localleader
      :n :desc "eval buffer" "eb" #'jupyter-eval-buffer
      :n :desc "eval function" "ed" #'jupyter-eval-defun
      :nv :desc "eval region" "er" #'jupyter-eval-region)
(map! :map inferior-python-mode ; Bindings for the Python REPL buffer
      :nvi "C-k" #'windmove-up ; Allow windmove in REPL
      :nvi "C-j" #'windmove-down
      :nv "C-h" #'windmove-left)
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-j") nil) ; Unbind C-j in Jupyter REPL insert
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-k") nil) ; Unbind C-k in Jupyter REPL insert
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-S-j") #'evil-mc-make-cursor-move-next-line) ; MC in Jupyter REPL
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-S-k") #'evil-mc-make-cursor-move-prev-line) ; MC in Jupyter REPL

;; Matlab
;; ------
(map! :map matlab-mode-map
      :nv "C-S-m" (lambda () (interactive) (org-switch-to-buffer-other-window "*MATLAB*")) ; Switch to Matlab shell
      :ni "C-c h" #'matlab-shell-help-at-point ; Help for function at point
      :localleader
      :desc "eval buffer" :n "eb" #'(lambda () (interactive) (evil-goto-first-line) (evil-visual-line) (evil-goto-line) (matlab-shell-run-region))
      :desc "eval line" :n "el" #'(lambda () (interactive) (evil-visual-line) (matlab-shell-run-region))
      :desc "eval region" :n "er" #'matlab-shell-run-region
      :n "f" #'matlab-shell-help-at-point ; Help (localleader)
      :n "s" #'matlab-shell) ; Start shell (localleader)
(map! :map matlab-shell-mode-map
      :ni "C-c l" #'comint-clear-buffer ; Clear shell buffer
      :nv "C-l" #'windmove-right ; Windmove in shell
      :nv "C-h" #'windmove-left
      :nv "C-j" #'windmove-down
      :nv "C-k" #'windmove-up)

;; PDF View Mode
;; -------------
(map! :map pdf-view-mode-map
      :nvi "go" nil ; Unbind default go
      :nvi "C-j" #'windmove-down ; Use C-j/k for windmove
      :nvi "C-k" #'windmove-up
      :nvi "k" (lambda () (interactive) (pdf-view-scroll-down-or-previous-page 1)) ; Scroll/Page up
      :nvi "j" (lambda () (interactive) (pdf-view-scroll-up-or-next-page 1)) ; Scroll/Page down
      :nvi "h" (lambda () (interactive) (image-backward-hscroll 25)) ; Horizontal scroll left
      :nvi "l" (lambda () (interactive) (image-forward-hscroll 25)) ; Horizontal scroll right
      :nvi "f" #'my/open-pdf-externally ; Open externally
      :nvi "e" #'my/open-pdf-externally ; Open externally (alternative)
      :nvi "i" #'org-noter-insert-precise-note ; Org Noter integration
      :nvi "I" #'org-noter-insert-note ; Org Noter integration
      :nvi "C-c O" (lambda () (interactive) (dired-jump) (dired-open)) ; Open dired in PDF's directory
      :nvi "go" (kbd "SPC o - & RET") ; Custom 'go' binding? (Check command)
      :vin "gl" nil ; Unbind gl in visual mode
      :nvi "gl" nil) ; Unbind gl
(map! :map pdf-occur-buffer-mode-map
      :nv "C-j" #'next-error-no-select ; Navigate occurrences
      :nv "C-k" #'previous-error-no-select)

;; Shell / Terminal Modes
;; ----------------------
(map! :map shell-mode-map
      "C-l" #'comint-clear-buffer) ; Clear shell buffer
(map! :map vterm-mode-map
      :nv "p" #'term-paste ; Paste in vterm
      :nvi "C-c p" #'term-paste
      :nvi "C-c v" #'term-paste
      :nvi "C-c c" #'evil-yank) ; Yank from vterm

;; Dired Mode
;; ----------
(map! :map dired-mode-map
      ;; Unbind potentially conflicting keys
      :v "u" nil
      :n "=" nil
      :n "s" nil
      :n "/" nil
      ;; Custom bindings
      :nv "C-c C-y" #'diredp-copy-abs-filenames-as-kill ; Copy filenames
      :nv "q" #'+dired/quit-all ; Quit dired (Doom specific)
      :n [f5] #'revert-buffer ; Refresh dired buffer
      :n "=" #'diredp-ediff) ; Ediff files

;; Minibuffer / Completion Frameworks
;; ----------------------------------
(map! :map minibuffer-mode-map :nvi ";" #'embark-act) ; Embark in minibuffer
(map! :map company-active-map "C-s" #'my/save-and-change-to-normal) ; Save while completion is active
(map! :map helm-map
      "C-j" #'helm-next-line
      "C-k" #'helm-previous-line
      [control-backspace] #'backward-kill-word
      "ESC" #'helm-exit-minibuffer)
(map! :map helm-find-files-map
      [control-backspace] #'backward-kill-word)
(map! :map vertico-map
      "C-;" #'embark-act
      "C-b" #'embark-become
      "C-e" #'embark-export
      "C-j" #'vertico-next ; Use C-j/k for navigation
      "C-k" #'vertico-previous)

;; Citar (Bibliography)
;; --------------------
(map! :map citar-citation-map
      :desc "copy cite link"  "c" #'copy-with-square-brackets ; Custom copy format
      "d" #'citar-org-delete-citation) ; Delete citation

;; Wordnut Mode
;; ------------
(evil-define-key 'normal wordnut-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal wordnut-mode-map (kbd "RET") 'wordnut-lookup-current-word)
(evil-define-key 'normal wordnut-mode-map (kbd "h") 'wordnut-history-backward)
(evil-define-key 'normal wordnut-mode-map (kbd "l") 'wordnut-history-forward)
(evil-define-key 'normal wordnut-mode-map (kbd "H") 'wordnut-history-lookup)
(evil-define-key 'normal wordnut-mode-map (kbd "/") 'wordnut-search)
(evil-define-key 'normal wordnut-mode-map (kbd "o") 'wordnut-show-overview)

;; GPTel Mode
;; ----------
(map! :map gptel-mode-map
      "C-c <return>" :desc "send the current region to GPTel" #'gptel-send) ; Send from gptel buffer

;; Denote (Note Taking) - Org Mode Specific Bindings
;; ------------------------------------------------
;; Note: Global Denote bindings are handled via leader keys or C-c n prefix below
(after! denote
  (map!
   ;; Denote bindings within Org Mode using C-c n prefix
   (:map org-mode-map :nvi
         "C-c n j" #'my-denote-journal ; Custom journal command
         "C-c n o" #'denote-open-or-create
         "C-c n n" #'denote
         "C-c n N" #'denote-type
         "C-c n d" #'denote-date
         "C-c n z" #'denote-signature ; "zettelkasten" mnemonic
         "C-c n s" #'denote-subdirectory
         "C-c n t" #'denote-template
         "C-c n I" #'denote-link ; "insert" mnemonic
         "C-c n L" #'denote-link-after-creating
         "C-c n i" #'denote-link-or-create ; "insert" mnemonic
         "[[" #'denote-link-or-create ; Wiki-style linking
         "C-c n a" #'denote-link-add-links
         "C-c n b" #'denote-backlinks
         "C-c n f f" #'denote-link-find-file
         "C-c n f b" #'denote-link-find-backlink
         "C-c n k a" #'denote-keywords-add
         "C-c n k r" #'denote-keywords-remove
         "C-c n r" #'denote-rename-file
         "C-c n R" #'denote-rename-file-using-front-matter)

   ;; Denote bindings within Dired Mode
   (:map dired-mode-map
         "C-c C-d C-i" #'denote-link-dired-marked-notes
         "C-c C-d C-r" #'denote-dired-rename-marked-files
         "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter)

   ;; Denote bindings within Org Mode using C-n prefix (Evil Normal/Visual/Insert)
   (:map evil-org-mode-map :prefix "C-n" :nvi
         "j" #'my-denote-journal ; our custom command
         "o" #'denote-open-or-create
         "n" #'denote
         "N" #'denote-type
         "d" #'denote-date
         "z" #'denote-signature ; "zettelkasten" mnemonic
         "s" #'denote-subdirectory
         "t" #'denote-template
         "I" #'denote-link; "insert" mnemonic
         "i" #'denote-link-or-create ; "insert" mnemonic
         "[[" #'denote-link-or-create
         "]]" #'denote-link-or-create ; Alternative wiki-style
         "a" #'denote-link-add-links
         "b" #'denote-backlinks
         "f f" #'denote-link-find-file
         "f b" #'denote-link-find-backlink
         "k a" #'denote-keywords-add
         "k r" #'denote-keywords-remove
         "r" #'denote-rename-file
         "R" #'denote-rename-file-using-front-matter))
  ) ; End after! denote

;; Leader Key Bindings (SPC)
;; =========================
(map! :leader
      :nv ; Unbind some default Doom leader keys first
      "bb" nil ; Replaced by consult-buffer
      "is" nil ; Replaced by search leader keys
      "ir" nil ; Replaced by org-ref leader key
      "fc" nil ; Replaced by custom fc
      "TAB" nil ; Replaced by custom TAB prefix map

      ;; Top Level Leader Keys
      :desc "M-x" :n "x" #'execute-extended-command
      :desc "scratch buffer" :n "z" #'doom/open-scratch-buffer
      :desc "consult buffer" "SPC" #'consult-buffer ; Easy access to buffer list
      :desc "consult buffer to new window" "S-SPC" #'consult-buffer-other-window
      :desc "consult buffer" "z" #'consult-buffer ; Alternative
      :desc "consult buffer to new window" "Z" #'consult-buffer-other-window
      :desc "ace-window" "-" #'ace-window ; Select window visually
      :desc "ace-delete-window" "D" #'ace-delete-window ; Delete window visually

      ;; Buffer Management ("b" prefix)
      (:prefix ("b" . "buffer")
       :desc "switch to buffer" "b" #'consult-buffer
       :desc "buffer to new window" "B" #'consult-buffer-other-window
       :desc "buffer to new frame" "F" #'consult-buffer-other-frame
       :desc "reload buffer" "r" #'my/revert-buffer-no-confirm
       :desc "revert buffer (confirm)" "R" #'revert-buffer
       :desc "open buffer and switch tabspace" "t" #'tabspaces-switch-buffer-and-tab)

      ;; File / Project ("f" prefix - using consult where possible)
      (:prefix ("f" . "file/find")
       :desc "copy buffer name"  "c" #'my/get-buffer-name
       :desc "search for file in dir (fd)"  "f" #'consult-fd
       :desc "Open project buffer in other window" "F" #'projectile-find-file-dwim-other-window)

      ;; Git ("g" prefix)
      (:prefix ("g" . "git")
       :nv "F" #'magit-pull ; Pull changes
       :nv "p" #'magit-push) ; Push changes

      ;; GPTel ("G" prefix)
      (:prefix ("G" . "GPT")
       :desc "open the GPTel buffer" "G" #'gptel
       :desc "send the current region to GPTel" "R" #'gptel-send)

      ;; Insert Stuff ("i" prefix)
      (:prefix ("i" . "insert")
       :nv :desc "copy and comment line(s)" "C" #'evilnc-copy-and-comment-lines
       :desc "make evil-mc-cursor here" "c" #'my/make-cursor-here
       :desc "add line above" "k" #'+evil/insert-newline-above
       :desc "add line below" "j" #'+evil/insert-newline-below
       (:prefix ("a" . "anki cloze") :map org-mode-map
        :nv "0" #'anki-editor-reset-cloze-number
        :nv "C" #'anki-editor-cloze-region-auto-incr
        :nv "c" #'anki-editor-cloze-region-dont-incr
        :nv "w" #'anki-editor-cloze-word-under-cursor-auto-incr)
       (:prefix ("l" . "latex symbols")
        :desc "right double arrow"  "r" (kbd "$\\Rightarrow$"))
       (:prefix ("s" . "surround")
        :desc "surround object with bold"  "*" (kbd "ysio*")
        :desc "surround object with quotes"  "\"" (kbd "ysio\"")
        :desc "surround object with single quotes"  "\'" (kbd "ysio\'")
        :desc "surround object with parens" "\)" (kbd "ysio\)")
        :desc "surround object with brackets" "\]" (kbd "ysio\]")
        :desc "surround object with curlies"  "}" (kbd "ysio}")
        :desc "surround object with spaces"  "SPC" (kbd "ysio SPC")))

      ;; Jump ("j" prefix)
      (:prefix ("j" . "jump")
       :desc "avy timer" "j" 'evil-avy-goto-char-timer
       :desc "avy line" "l" 'evil-avy-goto-line)

      ;; My Custom Commands ("k" prefix)
      (:prefix ("k" . "kennedy")
       :desc "aidermacs transient" "a" #'aidermacs-transient-menu
       :desc "select header content" "y" #'my/visual-inside-org-header
       :desc "copy header content" "h" #'my/yank-org-headline
       :desc "gptel-send" "G" #'gptel ; Duplicate of G G?
       :desc "gptel-send" "g" #'gptel-send ; Duplicate of G R?
       :desc "gptel-rewrite" "r" #'gptel-rewrite
       :desc "kill all other windows" "o" 'delete-other-windows
       :desc "resize window to small" "F" 'my/make-small-frame
       :desc "resize window to medium" "m" 'my/make-medium-frame
       :desc "resize window to large" "M" 'my/make-large-frame
       :desc "switch to other frame" "f" 'other-frame
       :desc "writeroom mode" "w" #'writeroom-mode
       :desc "kill buffer and window" "D" #'kill-buffer-and-window
       :desc "kill buffer" "d" 'kill-current-buffer
       :desc "switch to previous buffer" "k" 'evil-switch-to-windows-last-buffer
       :desc "search and replace vim style" "s" #'my/search-replace
       :desc "search and replace vim style - in region" "S" #'my/search-replace-in-region
       :desc "copy figures dir for spatial" "R" #'my/scp-copy-figures
       :desc "register dwi" "p" #'consult-register-load
       :desc "point to register" "P" #'point-to-register ; Added Shift-p
       :desc "run macro" "e" #'kmacro-end-and-call-macro
       :desc "generate laTex previews" "L" #'org-latex-preview
       (:prefix ("b" . "references")
        :desc "refresh bibliography" "r" #'citar-refresh
        :desc "open bibliography" "b" #'citar-open))

      ;; Notes / Denote ("n" prefix) - Org Mode Specific
      (:prefix ("n" . "notes") :map org-mode-map
       :nv "o" #'denote-open-or-create
       :nv "f" #'consult-notes ; Find note using consult
       :nv "n" #'denote ; Create new note
       :nv "r" #'denote-rename-file
       :nv "R" #'denote-rename-file-using-front-matter
       :nv "k" #'denote-keywords-add
       :nv "K" #'denote-keywords-remove
       :nv "D" #'denote-date ; Add date to front matter
       :nv "z" #'denote-signature ; Add signature
       :nv "s" #'denote-subdirectory ; Move note to subdirectory
       :nv "t" #'denote-template ; Create note from template
       :nv "i" #'denote-link-or-create ; Insert link or create note
       :nv "I" #'denote-link ; Insert link to existing note
       :nv "L" #'denote-link-after-creating ; Create note then insert link
       :nv "a" #'denote-link-add-links ; Add links between marked notes
       :nv "b" #'denote-backlinks ; Show backlinks for note at point
       :nv "F" #'denote-link-find-file ; Find file for link at point
       :nv "B" #'denote-link-find-backlink) ; Find backlink for link at point

      ;; Org Mode ("o" prefix)
      (:prefix ("o" . "org")
       :desc "agenda for literature followup" "al" #'(lambda () (interactive) (org-agenda nil "l"))
       :desc "insert org-ref link" "r" #'org-ref-insert-link ; Moved from global ir
       ;; Shell related commands under 'o' prefix
       :nv :desc "windows cmd" "w" #'shell ; Start default shell
       :nv :desc "eshell" "s" #'my-shell ; Start eshell via custom function
       :nv :desc "dired in some dir" "d" #'consult-dir ; Open dired via consult
       :nv :desc "dired in a new window" "D" #'consult-dir) ; Open dired in other window

      ;; Quit / Frame ("q" prefix)
      (:prefix ("q" . "quit/frame")
       :desc "force delete frame" "f" #'my/force-delete-frame) ; Close frame without prompt

      ;; Search ("s" prefix)
      (:prefix ("s" . "search")
       :desc "search online" "G" #'+lookup/online ; Lookup online (Doom specific?)
       :desc "search org outline" "o" #'consult-outline ; Search headings in Org buffer
       :desc "search project/dir (rg)" "p" #'consult-ripgrep ; Ripgrep in project
       :desc "ripgrep in project/dir" "d" #'consult-ripgrep ; Ripgrep (alternative)
       :desc "grep in project/dir" "D" #'consult-grep ; Grep in project
       :desc "search buffer"  "s" #'consult-line ; Search lines in current buffer
       :desc "search for file in dir (fd)"  "f" #'consult-fd ; Find file (alternative)
       :desc "send query to gtp" "g" #'gptel-quick) ; Quick GPT query

      ;; Toggle / Treemacs ("t" prefix)
      (:prefix ("t" . "toggle/treemacs")
       :nv "t" #'treemacs ; Toggle Treemacs file tree
       :nv "T" #'popper-toggle ; Toggle Popper popup window
       :nv "j" #'popper-toggle ; Alternative toggle Popper
       :nv "<return>" #'popper-toggle ; Alternative toggle Popper
       :nv "s" #'shell ; Start shell (duplicate of o w?)
       :nv "i" #'my/toggle-org-timer ; Toggle Org timer
       :nv "I" #'tmr) ; Start/show timer (tmr package?)

      ;; Window Management ("w" prefix)
      (:prefix ("w" . "window")
       :desc "ace-window" "w" #'ace-window ; Select window visually (alternative)
       :desc "equate window sizes" "e" #'balance-windows
       :desc "minimize window" "mm" #'minimize-window
       :desc "minimize window" "O" #'minimize-window ; Alternative minimize
       :desc "maximize window" "mM" #'doom/window-maximize-buffer ; Maximize (Doom specific)
       :desc "window resize hydra" "." 'hydra-window-resize/body) ; Hydra for resizing

      ;; Tab Management ("TAB" prefix)
      (:prefix-map ("TAB" . "Tabs")
       :desc "Switch tab" "TAB" #'tab-bar-switch-to-recent-tab
       :desc "Show tab list" "l" #'tab-bar-select-tab-by-name
       :desc "New tab" "n" #'tab-bar-new-tab
       :desc "Rename tab" "r" #'tab-bar-rename-tab
       :desc "Rename tab by name" "R" #'tab-bar-rename-tab-by-name
       :desc "Close tab" "d" #'tab-bar-close-tab
       :desc "Close tab by name" "D" #'tab-bar-close-tab-by-name
       :desc "Close other tabs" "1" #'tab-bar-close-other-tabs
       :desc "Previous tab" "j" #'tab-previous ; Use j/k for prev/next
       :desc "Next tab" "k" #'tab-next)
      ) ; End Leader Key Bindings

;; Hydras
;; ======
(defhydra hydra-window (:color red :columns 3)
  "Window Management"
  ("h" windmove-left "←")
  ("j" windmove-down "↓")
  ("k" windmove-up "↑")
  ("l" windmove-right "→")
  (">" (window-resize nil 15 1) "Expand →")
  ("<" (window-resize nil -15 1) "Shrink ←")
  ("+" (window-resize nil 15 nil) "Expand ↓")
  ("-" (window-resize nil -15 nil) "Shrink ↑")
  ("|" (lambda () (interactive) (split-window-right) (windmove-right)) "Split →")
  ("_" (lambda () (interactive) (split-window-below) (windmove-down)) "Split ↓")
  ("q" nil "quit" :color blue))
;; Note: hydra-window-resize/body is bound via leader key "w ." above

(defhydra hydra-smerge (:color pink :hint nil :pre (smerge-mode 1) :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _R_esolve
^^           _l_ower              _>_: base/lower        _K_ill current
^^           _a_ll                _r_efine
^^           _RET_: current       _e_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" (lambda () (interactive) (smerge-keep-base) (quit-hydra)))
  ("u" (lambda () (interactive) (smerge-keep-upper) (quit-hydra)))
  ("l" (lambda () (interactive) (smerge-keep-lower) (quit-hydra)))
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("r" smerge-refine)
  ("e" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("R" smerge-resolve)
  ("K" smerge-kill-current)
  ("q" nil "cancel" :color blue))
;; Note: hydra-smerge/body is bound via global keys "C-c s" and "C-c m" above


(map!
 :nvi "M-q" #'+workspace/other
 :ni "C-{" #'org-roam-node-insert
 :ni "C-}" #'org-ref-insert-link)


(map!
 :map pdf-view-mode-map
 :nvi "go" nil
 :nvi "C-j" nil
 :nvi "C-k" nil)

(map!
 :map pdf-occur-buffer-mode-map

 :nv "C-j" #'next-error-no-select
 :nv "C-k" #'previous-error-no-select
 :map pdf-view-mode-map
 :nvi "C-c O" (lambda () (interactive) (dired-jump) (dired-open))
 :nvi "go" (kbd "SPC o - & RET")
 :vin "gl" nil
 :nvi "C-j" #'windmove-down
 :nvi "C-k" #'windmove-upnil
 :map company-active-map "C-s" #'my/save-and-change-to-normal
 )

(map!
 (:map matlab-mode-map
  :nv "C-S-m" (lambda ()
                (interactive)
                (org-switch-to-buffer-other-window "*MATLAB*"))
  :ni "C-c h" #'matlab-shell-help-at-point
  )
 (:map matlab-shell-mode-map
  :ni "C-c l" #'comint-clear-buffer
  :nv "C-l" #'windmove-right
  :nv "C-h" #'windmove-left
  :nv "C-j" #'windmove-down
  :nv "C-k" #'windmove-up
  )
 )


(map!
 :map helm-map
 "C-j" #'helm-next-line
 "C-k" #'helm-previous-line
 [control-backspace] #'backward-kill-word
 "ESC" #'helm-exit-minibuffer
 :map helm-find-files-map
 [control-backspace] #'backward-kill-word
 )


(map! :map evil-org-mode-map :localleader :n
      "E" #'my/export-org-to-docx
      "n" #'org-narrow-to-subtree
      "N" #'widen
      )

(map! :nvi
      [C-o] #'better-jumper-jump-backward
      [C-i] #'better-jumper-jump-forward

      :map org-mode-map
      :nvi
      [C-o] #'better-jumper-jump-backward
      [C-i] #'better-jumper-jump-forward

      :map evil-org-mode-map
      :nvi
      [C-o] #'better-jumper-jump-backward
      [C-i] #'better-jumper-jump-forward
      )

(map! :map org-roam-backlinks-mode-map "return" #'org-open-at-point)
(map! :map pdf-view-mode-map
      :nvi "gl" nil
      :nvi "k" (lambda ()
                 (interactive)
                 (pdf-view-scroll-down-or-previous-page  1))
      :nvi "j" (lambda ()
                 (interactive)
                 (pdf-view-scroll-up-or-next-page 1))
      :nvi "h" (lambda ()
                 (interactive)
                 (image-backward-hscroll 25))
      :nvi "l" (lambda ()
                 (interactive)
                 (image-forward-hscroll 25))
      :nvi "f" #'my/open-pdf-externally
      :nvi "e" #'my/open-pdf-externally
      :nvi "i" #'org-noter-insert-precise-note
      :nvi "I" #'org-noter-insert-note)

(map! :leader
      :nv
      "bb" nil
      "is" nil
      "ir" nil
      "fc" nil)

(map! :leader
      :nv
      "ir" #'org-ref-insert-link
      )

;; (map! :map evil-motion-state-map :nv "j" #'evil-next-visual-line)

(map! :leader
      :nv
      :desc "copy buffer name"  "fc" #'my/get-buffer-name
      ;; :desc "helm-bibtex"  "nB" #'helm-bibtex
      ;; :desc "citar references"  "nb" #'citar-open
      :desc "agenda for literature followup" "oal" #'(lambda () (interactive) (org-agenda nil "l"))
      :desc "M-x" :n "x" #'execute-extended-command
      :desc "scratch buffer" :n "z" #'doom/open-scratch-buffer
      )


;; (map! :map minibuffer-mode-map
;;  :niv "fg" #'abort-recursive-edit)
(map!
 :n "gO" #'+evil/insert-newline-above
 :n "go" #'+evil/insert-newline-below
 :n "gK" #'+evil/insert-newline-above
 :n "gJ" #'+evil/insert-newline-below

 :n "g[" #'denote-link-or-create
 :n "g]" #'kmacro-end-an-call-macro
 :n "g." #'er/expand-region

 :n "gh" #'avy-goto-char-timer
 :n "gf" #'execute-extended-command
 ;; :niv "fg" #'evil-force-normal-state

 ;; :n "gh" #'windmove-left
 ;; :n "gj" #'windmove-down
 ;; :n "gk" #'windmove-up
 ;; :n "gl" #'windmove-right

 ;; :nv "gj" #'avy-goto-char-timer
 :nv "gj" #'org-forward-element
 ;; :n "gh" #'
 ;; :n "gk" #'
 ;; :n "gl" #'

 :n "g+" #'evil-numbers/inc-at-pt
 :vn "gs" nil
 ;; :nv "gf" #'evil-repeat
 :nv "E" #'evil-forward-WORD-end
 :nv "W" #'evil-forward-WORD-begin
 :nvi "C-S-l" #'org-roam-insert
 :nv "C-l" #'windmove-right
 :nv "C-h" #'windmove-left
 :nv "C-j" #'windmove-down
 :nv "C-k" #'windmove-up
 ;; :nvi "C-c l" #'toggle-input-method
 :nvi "M-;" #'toggle-input-method

 :n "z=" #'my/indent-buffer
 :map vterm-mode-map
 :nv "p" #'term-paste
 :nvi "C-c p" #'term-paste
 :nvi "C-c v" #'term-paste
 :nvi "C-c c" #'evil-yank
 :n "!" #'+workspace/close-window-or-workspace
 ;; :nv "YY" #'(lambda ()
 ;;              (interactive)
 ;;              (dired-copy-filename-as-kill 0))

 :map org-mode-map
 ;; :nv "E" #'evil-end-of-visual-line
 ;; :nv "W" #'evil-beginning-of-visual-line

 :vn "zh" #'org-up-element
 :vn "zj" #'org-forward-heading-same-level
 :vn "zk" #'org-backward-heading-same-level
 :vn "zl" #'org-next-visible-heading

 :n "z=" #'flyspell-correct-at-point
 :n "g=" #'flyspell-correct-at-point

 ;; :nv "gf" #'evil-repeat
 :nvi "C-S-l" #'org-roam-insert
 :nvi "C-l" #'windmove-right
 :nvi "C-h" #'windmove-left
 :nvi "C-j" #'windmove-down
 :nvi "C-k" #'windmove-up
 )

(map! :prefix "z"
      :nv "z" (lambda ()
                (interactive)
                (call-interactively #'evil-next-flyspell-error)
                (call-interactively #'flyspell-correct-at-point)
                )
      :nv "Z" (lambda ()
                (interactive)
                (call-interactively #'evil-prev-flyspell-error)
                (call-interactively #'flyspell-correct-at-point)
                )
      )

(map! :leader
      :nv
      :desc "searcj online" "sG" #'+lookup/online
      :desc "search org outline" "so" #'consult-outline
      :desc "search project/dir" "sp" #'consult-ripgrep
      :desc "ripgrep in project/dir" "sd" #'consult-ripgrep
      :desc "grep in project/dir" "sD" #'consult-grep
      :desc "search buffer"  "ss" #'consult-line
      :desc "search for file in dir"  "sf" #'consult-fd
      :desc "equate window sizes" "we" #'balance-windows
      :desc "minimize window" "wmm" #'minimize-window
      :desc "minimize window" "wO" #'minimize-window
      :desc "maximize window" "wmM" #'doom/window-maximize-buffer
      :desc "ace-window" "-" #'ace-window
      :desc "ace-delete-window" "D" #'ace-delete-window
      :desc "switch to buffer" "bb" #'consult-buffer
      :desc "buffer to new window" "bB" #'consult-buffer-other-window)

(map! :localleader
      :map (python-mode-map python-ts-mode-map)
      :n :desc "eval buffer" "eb" #'jupyter-eval-buffer
      :n :desc "eval function" "ed" #'jupyter-eval-defun
      :nv :desc "eval region" "er" #'jupyter-eval-region
      )
(map! :map (python-mode-map python-ts-mode-map)
      :nv "C-<return>"  nil
      :v "C-c <return>" #'python-shell-send-region)

(map! :map (inferior-python-mode)
      :nvi "C-k" nil
      :nvi "C-j" nil)

(map! :map (inferior-python-mode)
      :nvi "C-k" #'windmove-up
      :nvi "C-j" #'windmove-down
      ;; :nvi "C-K" #'evil-mc-make-cursor-move-prev-line
      ;; :nvi "C-J" #'evil-mc-make-cursor-move-next-line
      )

(map! :map (inferior-python-mode)
      :nv "C-k" #'windmove-up
      :nv "C-h" #'windmove-left
      )
(map! :after python
      :map (python-mode-map python-ts-mode-map)
      :nv "C-<return>" #'jupyter-eval-line-or-region
      :nv "S-<return>" #'jupyter-eval-line-or-region
)

(map! :localleader
      :map matlab-mode-map
      :desc "eval buffer" :n "eb" #'(lambda () (interactive) (evil-goto-first-line) (evil-visual-line) (evil-goto-line) (matlab-shell-run-region))
      :desc "eval line" :n "el" #'(lambda () (interactive) (evil-visual-line) (matlab-shell-run-region))
      :desc "eval region" :n "er" #'matlab-shell-run-region
      :n "f" #'matlab-shell-help-at-point
      :n "s" #'matlab-shell
      )

(map! :leader
      (:prefix ("j" . "navigation")
       :desc "avy timer" "j" 'evil-avy-goto-char-timer
       :desc "avy line" "l" 'evil-avy-goto-line)

      (:prefix "t"
       :nv "t" #'treemacs
       :nv "T" #'popper-toggle
       :nv "j" #'popper-toggle
       :nv "<return>" #'popper-toggle
       :nv "s" #'shell
       :nv "i" #'my/toggle-org-timer
       :nv "I" #'tmr
       )

      (:prefix "o"
       :nv :desc "windows cmd" "w" #'shell
       :nv :desc "eshell" "s" #'my-shell
       :nv :desc "dired in some dir" "d" #'consult-dir
       :nv :desc "dired in a new window" "D" #'consult-dir
       )

      (:prefix "g"
       :nv "F" #'magit-pull
       :nv "p" #'magit-push
       )

      (:prefix ("b")
       :desc "reload buffer" "r" #'my/revert-buffer-no-confirm
       :desc "revert buffer" "R" #'revert-buffer
       )

      (:prefix ("i" . "insert stuff")
       :nv :desc "copy and comment line(s)" "C" #'evilnc-copy-and-comment-lines
       :desc "make evil-mc-cursor here" "c" #'my/make-cursor-here
       :desc "add line above" "k" #'+evil/insert-newline-above
       :desc "add line below" "j" #'+evil/insert-newline-below
       ;; :desc "surround object with bold"  "sb" (kbd "jkysio*"))
       (:prefix ("a" . "anki cloze")
        :map org-mode-map
        :nv "0" #'anki-editor-reset-cloze-number
        :nv "C" #'anki-editor-cloze-region-auto-incr
        :nv "c" #'anki-editor-cloze-region-dont-incr
        :nv "w" #'anki-editor-cloze-word-under-cursor-auto-incr
        )
       (:prefix ("l" . "latex symbols")
        :desc "right double arrow"  "r" (kbd "$\\Rightarrow$"))
       (:prefix ("s" . "surround stuff")
        :desc "surround object with bold"  "*" (kbd "ysio*")
        :desc "surround object with quotes"  "\"" (kbd "ysio\"")
        :desc "surround object with single quotes"  "\'" (kbd "ysio\'")
        :desc "surround object with parens" "\)" (kbd "ysio\)")
        :desc "surround object with brackets" "\]" (kbd "ysio\]")
        :desc "surround object with curlies"  "}" (kbd "ysio}")
        :desc "surround object with spaces"  "SPC" (kbd "ysio SPC")
        ))

      (:prefix ("k" . "my commands")
       :desc "aidermacs transient" "a" #'aidermacs-transient-menu
       :desc "select header content" "y" #'my/visual-inside-org-header
       :desc "copy header content" "h" #'my/yank-org-headline
       :desc "gptel-send" "G" #'gptel
       :desc "gptel-send" "g" #'gptel-send
       :desc "gptel-rewrite" "r" #'gptel-rewrite
       :desc "kill all other windows" "o" 'delete-other-windows
       :desc "resize window to small" "F" 'my/make-small-frame
       :desc "resize window to medium" "m" 'my/make-medium-frame
       :desc "resize window to large" "M" 'my/make-large-frame
       ;; :desc "make new frame" "F" 'make-frame-command
       :desc "switch to other frame" "f" 'other-frame
       :desc "writeroom mode" "w" #'writeroom-mode
       :desc "kill buffer and window" "D" #'kill-buffer-and-window
       :desc "kill buffer" "d" 'kill-current-buffer
       :desc "switch to previous buffer" "k" 'evil-switch-to-windows-last-buffer
       :desc "search and replace vim style" "s" #'my/search-replace
       :desc "search and replace vim style - in region" "S" #'my/search-replace-in-region
       :desc "copy figures dir for spatial" "R" #'my/scp-copy-figures
       ;; :desc "paste from kill-ring" "p" #'consult-register
       :desc "register dwi" "p" #'consult-register-load
       :desc "point to register" "p" #'point-to-register
       ;; :desc "paste from kill-ring" "p" 'helm-show-kill-ring
       ;; :desc "helm org rifle" "R" 'helm-org-rifle
       :desc "run macro" "e" #'kmacro-end-and-call-macro
       :desc "generate laTex previews" "L" #'org-latex-preview

       ;; (:prefix ("r" . "rectangle operations")
       ;;  "r" #'replace-rectangle
       ;;  :desc "paste rectangle" "p" #'yank-rectangle
       ;;  :desc "copy rectangle" "y"  #'copy-rectangle-as-kill
       ;;  :desc "cut rectangle" "x"  #'kill-rectangle
       ;;  :desc "push rectangle right" "r"  #'open-rectangle
       ;;  :desc "delete rectangle" "d"  #'clear-rectangle
       ;;  )

       (:prefix ("b" . "references")
        :desc "refresh bibliography" "r" #'citar-refresh
        :desc "open bibliography" "b" #'citar-open
        )
       ;; (:prefix ("i" . "insert stuff")
       ;;         (:prefix ("l" . "latex symbols")
       ;;          :desc "right double arrow"  "R" (kbd "$\\Rightarrow$")))
       ))

(defhydra hydra-window (:color red
                        :columns 3)
  ("h" windmove-left "window-left")
  ("j" windmove-down "window-down")
  ("k" windmove-up "window-up")
  ("l" windmove-right "right-left")
  (">" (window-resize nil 15 1) "increase window size horizontally")
  ("<" (window-resize nil -15 1) "decrease window size horizontally")
  ("+" (window-resize nil 15 nil) "increase window size vertically")
  ("-" (window-resize nil -15 nil) "increase window size vertically")
  ("|" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) "split right")
  ("_" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) "split down")
  ("q" nil "quit" :color blue))

(map! :leader
      :desc "ace delete window"  "d" #'ace-delete-window
      :desc "window resize hydra" "w." 'hydra-window-resize/body
      :desc "Open project buffer in other window" "pF" #'projectile-find-file-dwim-other-window
      :desc "open a buffer and switch to its tabspace" "bt" #'tabspaces-switch-buffer-and-tab
      :desc "consult buffer" "z" #'consult-buffer
      :desc "consult buffer to new window" "Z" #'consult-buffer-other-window
      :desc "consult buffer" "SPC" #'consult-buffer
      :desc "consult buffer to new window" "S-SPC" #'consult-buffer-other-window
      )

(defhydra hydra-smerge (:color pink
                            :hint nil
                            :pre (smerge-mode 1)
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _R_esolve
^^           _l_ower              _>_: base/lower        _K_ill current
^^           _a_ll                _r_efine
^^           _RET_: current       _e_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" (lambda () (interactive) (smerge-keep-base) (quit-hydra)))
      ("u" (lambda () (interactive) (smerge-keep-upper) (quit-hydra)))
      ("l" (lambda () (interactive) (smerge-keep-lower) (quit-hydra)))
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("r" smerge-refine)
      ("e" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("R" smerge-resolve)
      ("K" smerge-kill-current)
      ("q" nil "cancel" :color blue)
      )


(define-key evil-normal-state-map (kbd "J") 'evil-join)
(define-key evil-normal-state-map (kbd "K") 'join-line)

(global-set-key [f11] 'flyspell-correct-at-point)

(evil-define-key 'normal wordnut-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal wordnut-mode-map (kbd "RET") 'wordnut-lookup-current-word)
(evil-define-key 'normal wordnut-mode-map (kbd "h") 'wordnut-history-backward)
(evil-define-key 'normal wordnut-mode-map (kbd "l") 'wordnut-history-forward)
(evil-define-key 'normal wordnut-mode-map (kbd "H") 'wordnut-history-lookup)
(evil-define-key 'normal wordnut-mode-map (kbd "/") 'wordnut-search)
(evil-define-key 'normal wordnut-mode-map (kbd "o") 'wordnut-show-overview)


(map! :map org-mode-map
      :nvi "C-c  C-y" #'evil-yank
      :nvi "C-c  C-x" #'evil-delete
      :nvi "C-c  d" #'evil-delete
      :nvi "C-c  c" #'evil-yank
      :nvi "C-c  y" #'evil-yank
      :i "C-v" #'(lambda () (interactive) (backward-char) (evil-paste-after 1))
      :i "C-c p" #'(lambda () (interactive) (backward-char) (evil-paste-after 1))
      :i "C-c P" #'(lambda () (interactive) (backward-char 2) (evil-paste-after 1))
      :nvi "C-c  y" #'evil-yank
      :nv "C-c  p" #'evil-paste-after
      :nv "C-c  P" #'evil-paste-before
      :nvi "C-c  C-r" #'evil-redo
      :nvi "C-c  C-d" #'evil-delete
      :nvi "C-c  x" #'evil-delete
      :nvi "C-c  S" #'my/org-screenshot
      :nvi "C-c  s" #'evil-Surround-region
      )



(map! :map minibuffer-mode-map :nvi ";" #'embark-act)

(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-j") nil)
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-k") nil)
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-S-j") #'evil-mc-make-cursor-move-next-line)
(evil-define-key 'insert jupyter-repl-mode-map (kbd "C-S-k") #'evil-mc-make-cursor-move-prev-line)

(map! :map vertico-map
      ;; "C-." #'embark-act
      ;; "C-," #'embark-become
      ;; "C-/" #'embark-export
      "C-;" #'embark-act
      "C-b" #'embark-become
      "C-e" #'embark-export
      "C-j" #'vertico-previous
      "C-j" #'vertico-next)

;; (map! :map org-mode-map
;;       :nvi "C-c p h" 'org-hide-properties
;;       :nvi "C-c p s" 'org-show-properties
;;       :nvi "C-c p t" 'org-toggle-properties)
;; (map! :map (evil-org-mode-map emacs-lisp-mode-map) :n "<up>" (lambda nil (scroll-down-command 1)))
;; (map! :map (evil-org-mode-map emacs-lisp-mode-map) :n "<up>" #'evil-scroll-line-up)
;; (map! :map (evil-org-mode-map emacs-lisp-mode-map) :n "<down>" #'evil-scroll-line-down)


;; (map! :map
;;    :i "RETURN" #'comint-send-input)


;;(map! :map dired-mode-map "C-d" nil)
;; (map! :map dired-mode-map "Q" #'(lambda ()
;;                                   (interactive)
;;                                   (+dired/quit-all)
;;                                   (if  (not (one-window-p))
;;                                                   (+workspace/close-window-or-workspace))))


(map! :map dired-mode-map
      :v "u" nil
      :n "=" nil
      :n "s" nil
      :n "/" nil)


(map! :map dired-mode-map
      :nv "C-c C-y" #'diredp-copy-abs-filenames-as-kill
      ;; :nv "q" (lambda nil
      ;;           (interactive)
      ;;           (add-hook 'kill-buffer-query-functions 'my/prompt-on-dired-buffer-kill)
      ;;           (+dired/quit-all)
      ;;           (remove-hook 'kill-buffer-query-functions 'my/prompt-on-dired-buffer-kill)
      ;;           )
      :nv "q" #'+dired/quit-all
      :n [f5] #'revert-buffer
      :n "=" #'diredp-ediff)


;; (map! :leader
;;       :prefix "TAB"
;;       "TAB" #'tab-bar-switch-to-recent-tab
;;       ";" #'tab-bar-select-tab-by-name
;;       "r" #'tab-rename
;;       "n" #'tab-bar-new-tab
;;       "t" #'tab-new
;;       "w" #'tab-close
;;       "d" #'tab-close
;;       "1" #'tab-bar-select-tab
;;       "2" #'tab-bar-select-tab
;;       "3" #'tab-bar-select-tab
;;       "4" #'tab-bar-select-tab
;;       "5" #'tab-bar-select-tab
;;       "h" #'tab-previous
;;       "l" #'tab-next
;;       )

(map! :leader
      (:prefix-map ("TAB" . "Tabs")
       :desc "Switch tab" "TAB" #'tab-bar-switch-to-recent-tab
       :desc "Show tab list" "l" #'tab-bar-select-tab-by-name
       :desc "New tab" "n" #'tab-bar-new-tab
       :desc "Rename tab" "r" #'tab-bar-rename-tab
       :desc "Rename tab by name" "R" #'tab-bar-rename-tab-by-name
       :desc "Close tab" "d" #'tab-bar-close-tab
       :desc "Close tab by name" "D" #'tab-bar-close-tab-by-name
       :desc "Close other tabs" "1" #'tab-bar-close-other-tabs
       ;; :desc "Previous tab" "h" #'tab-previous
       ;; :desc "Next tab" "l" #'tab-next
       :desc "Previous tab" "j" #'tab-previous
       :desc "Next tab" "k" #'tab-next
       ))


(map!
 :nvi "M-1" #'tab-bar-select-tab
 :nvi "M-2" #'tab-bar-select-tab
 :nvi "M-3" #'tab-bar-select-tab
 :nvi "M-4" #'tab-bar-select-tab
 :nvi "M-5" #'tab-bar-select-tab
 )

;; (map!
;;  :nvi "M-1" #'centaur-tabs-select-visible-tab
;;  :nvi "M-2" #'centaur-tabs-select-visible-tab
;;  :nvi "M-3" #'centaur-tabs-select-visible-tab
;;  :nvi "M-4" #'centaur-tabs-select-visible-tab
;;  :nvi "M-5" #'centaur-tabs-select-visible-tab
;;  :nv "gN" #'centaur-tabs-backward
;;  :nv "gn" #'centaur-tabs-forward
;;  :nvi "C-<prior>" #'centaur-tabs-backward
;;  :nvi "C-<next>" #'centaur-tabs-forward
;;  ;; :nvi "C-`" #'popper-toggle-latest
;;  :nvi "M-`" #'popper-cycle
;;  )

(map! :map evil-normal-state-map
      :n "h" 'left-char
      :n "l" 'right-char
      :n [left] 'left-char
      :n [right] 'right-char
      :n "h" 'left-char
      :n "l" 'right-char
      :n [left] 'left-char
      :n [right] 'right-char)

(map! :map evil-org-mode-map
      "<f9>" #'+eval/region
      "C-c  k" #'org-capture
      "C-c  C-C" #'org-capture)


(map! :map gptel-mode-map
      "C-c  <return>" :desc "send the current region to GPTel" #'gptel-send
      )

(map!
 "C-x  C-x" #'org-capture
 "C-x  C-n" #'org-capture
 "C-c  C-<return>" :desc "send the current region to GPTel" #'gptel-send
 ;; "C-c  <return>" :desc "run command with encoding" (lambda nil (interactive) (universal-coding-system-argument 'utf-8))
 )

(map! :leader :prefix "G"
      :desc "open the GPTel buffer" "G" #'gptel
      :desc "send the current region to GPTel" "R" #'gptel-send
      :leader :prefix "s"
      :desc "send query to gtp, include region if active" "g" #'gptel-quick)


(map! :map global-map
      :nvi "C-p" #'delete-other-windows
      :nvi "C-S-p" #'projectile-find-file
      :nv "'" #'evil-goto-mark)

(map! :map citar-citation-map
      :desc "copy cite link"  "c" #'copy-with-square-brackets
      "d" #'citar-org-delete-citation)

(map! :map evil-org-mode-map
      :nv "k" #'evil-previous-visual-line
      :nv "j" #'evil-next-visual-line
      :nv "gk" #'evil-previous-visual-line
      :nv "gj" #'evil-next-visual-line
      :nv "V" #'evil-visual-screen-line)


(map! :map evil-org-mode-map
  :ni "M-j" #'my/org-meta-down
  :ni "M-k" #'my/org-meta-up)


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
