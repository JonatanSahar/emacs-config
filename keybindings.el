;;; ~/.doom.d/keybindings.el -*- lexical-binding: t; -*-
;;
;; Layout:
;;   1. Global bindings and evil motions (normal / visual)
;;   2. Insert state
;;   3. Mode maps (prog/text, org, python REPL, pdf, shells, dired,
;;      minibuffer/vertico, citar, evil-ex, agent-shell)
;;   4. Hydras
;;   5. Leader (SPC) -- one `map! :leader', prefixes in alphabetical order
;;   6. Function keys
;;
;; Not here: python/jupyter bindings live in repl.el; vterm C-j/C-k and
;; tabspaces bindings live in ui.el.  Doom defaults: ~/.emacs.d/sources/doom+/modules/config/default/+evil-bindings.el

;;; ============================================================
;;; 1. Global bindings and evil motions
;;; ============================================================

(map! :nvi
      ;; Undo / redo / save
      "C-z" #'evil-undo
      "C-y" #'evil-redo
      "C-s" #'my/save-and-change-to-normal
      ;; Jump list
      [C-o] #'better-jumper-jump-backward
      [C-i] #'better-jumper-jump-forward
      ;; Input method
      "C-\\" #'toggle-input-method
      "M-;" #'toggle-input-method
      ;; Projectile
      "C-S-p" #'projectile-find-file
      ;; Paste ring
      "M-p" #'evil-paste-pop
      "M-n" #'evil-paste-pop-next
      ;; Tab bar: M-1..M-5 select tab by number
      "M-1" #'tab-bar-select-tab
      "M-2" #'tab-bar-select-tab
      "M-3" #'tab-bar-select-tab
      "M-4" #'tab-bar-select-tab
      "M-5" #'tab-bar-select-tab
      ;; Embark
      "C-;" #'embark-act
      ;; Flyspell
      [f11] #'flyspell-correct-at-point
      ;; Unbind C-x C-n (set-goal-column)
      "C-x C-n" nil
      ;; drag-stuff
      "M-<up>"   #'drag-stuff-up
      "M-<down>" #'drag-stuff-down
      "M-k"      #'drag-stuff-up
      "M-j"      #'drag-stuff-down
      ;; Word-wise kill (whitespace-aware)
      :ni "C-<backspace>" #'conditional-evil-kill-to-prev-word-end
      :ni "C-<delete>"    #'conditional-evil-kill-to-next-word-start

      ;; Window
      :nv "C-p" #'delete-other-windows

      ;; Yank / paste / delete on the C-c prefix (insert + visual only)
      :iv "C-c c"   #'evil-yank
      :iv "C-c y"   #'evil-yank
      :iv "C-c v"   #'consult-yank-from-kill-ring
      :iv "C-c d"   #'evil-delete
      :iv "C-c x"   #'evil-delete
      :iv "C-c C-r" #'evil-redo
      :iv "C-c p"   #'evil-paste-after
      :iv "C-c P"   #'evil-paste-before)

;; Normal / visual motions
(map!
 ;; Visual-line movement
 :n "k" #'evil-previous-visual-line
 :n "j" #'evil-next-visual-line
 :v "<up>"   #'evil-previous-visual-line
 :v "<down>" #'evil-next-visual-line
 :n "<up>"   (cmd! (scroll-down-command 1))
 :n "<down>" (cmd! (scroll-up-command 1))
 ;; h/l move by char even with visual-line-mode
 :n "h" #'left-char
 :n "l" #'right-char
 :n [left]  #'left-char
 :n [right] #'right-char
 ;; Join
 :n "J" #'evil-join
 :n "K" #'join-line
 ;; Insert blank line above/below, stay in normal state
 :n "go" #'+evil/insert-newline-below
 :n "gO" #'+evil/insert-newline-above
 ;; g-prefixed helpers
 :n "g]" #'kmacro-end-and-call-macro
 :n "g." #'er/expand-region
 :n "gf" #'execute-extended-command
 :n "g+" #'evil-numbers/inc-at-pt
 :n "!"  #'+workspace/close-window-or-workspace
 ;; Flyspell: next/prev error and correct
 :n "gl" #'my/flyspell-next-and-correct
 :n "gh" #'my/flyspell-prev-and-correct
 (:prefix "z"
  :nv "z" #'my/flyspell-next-and-correct
  :nv "Z" #'my/flyspell-prev-and-correct)
 ;; Unbind gs (used by surround)
 :vn "gs" nil
 ;; WORD motions and marks
 :nv "E" #'evil-forward-WORD-end
 :nv "W" #'evil-forward-WORD-begin
 :nv "'" #'evil-goto-mark
 :nv "C-e" #'evil-end-of-visual-line
 ;; Windmove
 :nv "C-l" #'windmove-right
 :nv "C-h" #'windmove-left
 :nv "C-j" #'windmove-down
 :nv "C-k" #'windmove-up
 ;; Multiple cursors
 :ni "C-S-j" #'evil-mc-make-cursor-move-next-line
 :ni "C-S-k" #'evil-mc-make-cursor-move-prev-line)

;;; ============================================================
;;; 2. Insert state
;;; ============================================================

(map! :i "C-Z"     #'evil-emacs-state
      :i "C-M-SPC" #'evil-normal-state
      :i "S-<return>" #'evil-normal-state
      ;; Char/line motions (global; org does not override these)
      :i "C-'"   #'right-char
      :i "C-;"   #'left-char
      :i "C-k"   #'previous-line
      :i "C-j"   #'next-line
      :i "C-l"   #'right-char
      :i "C-h"   #'left-char
      :i "C-S-l" #'right-word
      :i "C-S-h" #'left-word
      ;; Paste
      :i "C-v" #'evil-paste-after)

;;; ============================================================
;;; 3. Mode maps
;;; ============================================================

;; Prog / text modes
(map! :map (prog-mode-map text-mode-map)
      :n "gj" #'evil-avy-goto-char-timer)

;; Org
(after! evil-org
  (map! :map (org-mode-map evil-org-mode-map)
        :nvi [C-o] #'better-jumper-jump-backward
        :nvi [C-i] #'better-jumper-jump-forward
        :nv "C-l" #'windmove-right
        :nv "C-h" #'windmove-left
        :nv "C-j" #'windmove-down
        :nv "C-k" #'windmove-up
        :nv "gj" #'org-forward-element
        :nv "V"  #'evil-visual-screen-line
        :i "M-h" #'org-beginning-of-line
        :i "M-l" #'org-end-of-line
        :nvi "C-M-y" #'my/org-copy-image-at-point-to-clipboard
        ;; Structure navigation
        :vn "zh" #'org-up-element
        :vn "zj" #'org-forward-heading-same-level
        :vn "zk" #'org-backward-heading-same-level
        :vn "zl" #'org-next-visible-heading
        :nvi "C-c h" #'org-toggle-heading
        :nvi "C-c i" #'org-toggle-item
        :ni "M-j" #'my/org-meta-down
        :ni "M-k" #'my/org-meta-up
        ;; Cite / denote links
        :ni "C-c I" #'org-cite-insert
        :n  "g["  #'denote-link-or-create
        :ni "C-c [" #'denote-link-or-create
        :n  "[["  #'denote-link-or-create
        :n  "]]"  #'denote-link-or-create
        ;; Flyspell
        :n "z=" #'flyspell-correct-at-point
        :n "g=" #'flyspell-correct-at-point
        ;; Misc
        :nvi "C-c s" #'evil-surround-region
        "<f9>" #'+eval/region
        :localleader
        :n "n" #'org-narrow-to-subtree
        :n "N" #'widen
        :n "L" #'org-latex-preview))

;; Python REPL buffer: windmove
(map! :map inferior-python-mode-map
      :nvi "C-k" #'windmove-up
      :nvi "C-j" #'windmove-down
      :nv  "C-h" #'windmove-left)

;; PDF view
(map! :map pdf-view-mode-map
      :nvi "C-j" #'windmove-down
      :nvi "C-k" #'windmove-up
      :nvi "k" (cmd! (pdf-view-scroll-down-or-previous-page 1))
      :nvi "j" (cmd! (pdf-view-scroll-up-or-next-page 1))
      :nvi "h" (cmd! (image-backward-hscroll 25))
      :nvi "l" (cmd! (image-forward-hscroll 25))
      :nvi "i" #'org-noter-insert-precise-note
      :nvi "I" #'org-noter-insert-note
      :nvi "C-c O" (cmd! (dired-jump) (dired-open))
      :vin "gl" nil
      :nvi "gl" nil)
(map! :map pdf-occur-buffer-mode-map
      :nv "C-j" #'next-error-no-select
      :nv "C-k" #'previous-error-no-select)

;; Shell / terminal
(map! :map shell-mode-map
      "C-l" #'comint-clear-buffer)
(map! :map vterm-mode-map
      :nv  "p"     #'term-paste
      :nvi "C-c p" #'term-paste
      :nvi "C-c v" #'term-paste
      :nvi "C-c c" #'evil-yank)

;; Dired
(map! :map dired-mode-map
      :v "u" nil
      :n "=" nil
      :n "s" nil
      :n "/" nil
      :nv "q"  #'quit-window
      :n  [f5] #'revert-buffer
      ;; denote
      "C-c C-d C-i" #'denote-link-dired-marked-notes
      "C-c C-d C-r" #'denote-dired-rename-marked-files
      "C-c C-d C-R" #'denote-dired-rename-marked-files-using-front-matter)

;; Minibuffer / vertico
(map! :map minibuffer-mode-map :nvi ";" #'embark-act)
(map! :map vertico-map
      "C-;" #'embark-act
      "C-b" #'embark-become
      "C-e" #'embark-export
      "C-j" #'vertico-next
      "C-k" #'vertico-previous)

;; Citar
(map! :map citar-citation-map
      :desc "copy cite link" "c" #'copy-with-square-brackets
      "d" #'citar-org-delete-citation)

;; Evil ex command line
(map! :map evil-ex-map
      "M-y" #'yank
      "C-v" #'yank)

;; agent-shell buffer: `agent-shell-mode-map' binds bare letters (n p r TAB + - 0),
;; which evil's normal state shadows.  Only chorded / g- / bracket-prefixed
;; re-bindings here: :n scoping does not hold reliably in comint-derived
;; buffers (q was quitting the window mid-insert).  Upstream C-c bindings
;; still apply: C-c C-c interrupt, C-c C-m mode, C-c C-v model,
;; C-c C-t thought level, C-c C-s config.
(map! :map agent-shell-mode-map
      :n "gj"    #'agent-shell-next-item
      :n "gk"    #'agent-shell-previous-item
      :n "C-M-u" #'agent-shell-backward-up-item
      ;; permission prompt navigation
      :n "]p"    #'agent-shell-next-permission-button
      :n "[p"    #'agent-shell-previous-permission-button
      :n "gp"    #'agent-shell-jump-to-latest-permission-button-row
      ;; queue/steer without leaving the shell
      ;; (C-c C-s is taken upstream by agent-shell-set-session-config-option)
      :ni "C-c C-q" #'agent-shell-prompt-queue
      :ni "C-c C-r" #'agent-shell-prompt-steer)

;; Diff review buffers.  Mirrors the upstream `agent-shell-diff-mode-map'
;; (y accept, C-c C-c reject, n/p hunks, RET open, q kill) into evil normal
;; state, where the bare letters would otherwise be shadowed.
(map! :map agent-shell-diff-mode-map
      :n "n"   #'diff-hunk-next
      :n "p"   #'diff-hunk-prev
      :n "y"   #'agent-shell-diff-accept-all
      :n "RET" #'agent-shell-diff-open-file
      :n "q"   #'kill-current-buffer)

;;; ============================================================
;;; 4. Hydras
;;; ============================================================

;; Bound on SPC w .
(defhydra hydra-window (:color red :columns 3)
  "Window Management"
  ("C-h" windmove-left "←")
  ("C-j" windmove-down "↓")
  ("C-k" windmove-up "↑")
  ("C-l" windmove-right "→")
  ("l" (window-resize nil 15 1) "Expand →")
  ("h" (window-resize nil -15 1) "Shrink ←")
  ("j" (window-resize nil 15 nil) "Expand ↓")
  ("k" (window-resize nil -15 nil) "Shrink ↑")
  ("|" (lambda () (interactive) (split-window-right) (windmove-right)) "Split →")
  ("_" (lambda () (interactive) (split-window-below) (windmove-down)) "Split ↓")
  ("q" nil "quit" :color blue))

;; Bound on SPC g m
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
  ("b" smerge-keep-base :exit t)
  ("u" smerge-keep-upper :exit t)
  ("l" smerge-keep-lower :exit t)
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

;;; ============================================================
;;; 5. Leader (SPC)
;;; ============================================================
;; Plain-string :prefix extends Doom's existing map; a cons (KEY . DESC)
;; only adds a which-key label.  Only :prefix-map creates a fresh map.

(map! :leader
      "TAB" nil                         ; replaced by the Tabs prefix below
      "z"   nil

      ;; Top level
      :desc "scratch buffer"               "Z"     #'doom/open-scratch-buffer
      :desc "consult buffer"               "SPC"   #'consult-buffer
      :desc "consult buffer to new window" "S-SPC" #'consult-buffer-other-window
      :desc "M-x"                          "x"     #'execute-extended-command
      :desc "consult buffer"               "z"     #'consult-buffer
      ;; Doom+ binds SPC a to embark-act; this file loads later and wins.
      :desc "Actions (embark)"             "A"     #'embark-act

      ;; --- a: agents ------------------------------------------------
      (:prefix ("a" . "agents")
       ;; agent-shell: session
       :desc "toggle shell"         "a" #'agent-shell-toggle
       :desc "new shell"            "n" #'agent-shell-new-shell
       :desc "switch shell buffer"  "b" #'agent-shell-switch-buffer
       :desc "fork session"         "F" #'agent-shell-fork
       :desc "resume session"       "R" #'agent-shell-resume-session
       :desc "open transcript"      "t" #'agent-shell-open-transcript
       ;; agent-shell: sending context
       :desc "send region/dwim"     "r" #'agent-shell-send-dwim
       :desc "send file"            "f" #'agent-shell-send-file
       :desc "send screenshot"      "S" #'agent-shell-send-screenshot
       :desc "send clipboard image" "p" #'agent-shell-send-clipboard-image
       ;; agent-shell: driving the turn
       :desc "queue prompt"         "q" #'agent-shell-prompt-queue
       :desc "steer (mid-turn)"     "s" #'agent-shell-prompt-steer
       :desc "interrupt"            "k" #'agent-shell-interrupt
       ;; agent-shell: session config
       :desc "set model"            "m" #'agent-shell-set-session-model
       :desc "set session mode"     "M" #'agent-shell-set-session-mode
       :desc "set thought level"    "T" #'agent-shell-set-session-thought-level
       ;; other agent frontends
       (:prefix ("o" . "other frontends")
        :desc "aidermacs"   "a" #'aidermacs-transient-menu
        :desc "claude-code" "c" #'claude-code-transient))

      ;; --- b: buffers -----------------------------------------------
      (:prefix "b"
       :desc "switch to buffer"                 "b" #'consult-buffer
       :desc "buffer to new window"             "B" #'consult-buffer-other-window
       :desc "buffer to new frame"              "F" #'consult-buffer-other-frame
       :desc "revert buffer"                    "r" #'revert-buffer-quick
       :desc "rename buffer"                    "R" #'rename-buffer
       :desc "open buffer and switch tabspace"  "t" #'tabspaces-switch-buffer-and-tab)

      ;; --- f: files -------------------------------------------------
      (:prefix "f"
       :desc "find file"                          "f" #'find-file
       :desc "project file in other window"       "F" #'projectile-find-file-dwim-other-window)

      ;; --- g: git ---------------------------------------------------
      (:prefix "g"
       :desc "Magit pull"   "p" #'magit-pull
       :desc "Magit push"   "P" #'magit-push
       :desc "Smerge hydra" "m" #'hydra-smerge/body)

      ;; --- i: insert ------------------------------------------------
      (:prefix "i"
       :desc "copy and comment line(s)"  "C" #'evilnc-copy-and-comment-lines
       :desc "make evil-mc cursor here"  "c" #'my/make-cursor-here
       :desc "add line above"            "k" #'+evil/insert-newline-above
       :desc "add line below"            "j" #'+evil/insert-newline-below
       (:prefix ("l" . "latex symbols")
        :desc "right double arrow" "r" (kbd "$\\Rightarrow$"))
       (:prefix ("S" . "surround")
        :desc "surround object with bold"          "*"   (kbd "ysio*")
        :desc "surround object with quotes"        "\""  (kbd "ysio\"")
        :desc "surround object with single quotes" "'"   (kbd "ysio'")
        :desc "surround object with parens"        ")"   (kbd "ysio)")
        :desc "surround object with brackets"      "]"   (kbd "ysio]")
        :desc "surround object with curlies"       "}"   (kbd "ysio}")
        :desc "surround object with spaces"        "SPC" (kbd "ysio SPC")))

      ;; --- j: jump --------------------------------------------------
      (:prefix ("j" . "jump")
       :desc "avy timer" "j" #'evil-avy-goto-char-timer
       :desc "avy line"  "l" #'evil-avy-goto-line)

      ;; --- k: my commands -------------------------------------------
      (:prefix ("k" . "My commands")
       :desc "kill all other windows"                   "o" #'delete-other-windows
       :desc "resize frame to small"                    "F" #'my/make-small-frame
       :desc "resize frame to medium"                   "m" #'my/make-medium-frame
       :desc "resize frame to large"                    "M" #'my/make-large-frame
       :desc "switch to other frame"                    "f" #'other-frame
       :desc "writeroom mode"                           "w" #'writeroom-mode
       :desc "kill buffer and window"                   "D" #'kill-buffer-and-window
       :desc "kill buffer"                              "d" #'kill-current-buffer
       :desc "switch to previous buffer"                "k" #'evil-switch-to-windows-last-buffer
       :desc "search and replace vim style"             "s" #'my/search-replace
       :desc "search and replace vim style - in region" "S" #'my/search-replace-in-region
       :desc "register dwim"                            "p" #'consult-register-load
       :desc "point to register"                        "P" #'point-to-register
       :desc "run macro"                                "e" #'kmacro-end-and-call-macro
       :desc "generate LaTeX previews"                  "L" #'org-latex-preview
       :desc "convert .py <-> .ipynb"                   "t" #'my/jupytext-file
       :desc "open bibliography"                        "b" #'citar-open)

      ;; --- n: notes (denote, global) --------------------------------
      ;; Doom's SPC n a (org-agenda) is left in place.
      (:prefix ("n" . "notes")
       :desc "new note"              "n" #'denote
       :desc "open or create note"   "o" #'denote-open-or-create
       :desc "rename note"           "r" #'denote-rename-file
       :desc "add keywords"          "k" #'denote-keywords-add
       :desc "remove keywords"       "K" #'denote-keywords-remove
       :desc "insert link"           "l" #'denote-link
       :desc "insert link or create" "i" #'denote-link-or-create
       :desc "backlinks"             "b" #'denote-backlinks
       :desc "note from template"    "t" #'denote-template
       :desc "find note (consult)"   "f" #'consult-notes
       :desc "search notes (grep)"   "s" #'denote-grep
       :desc "open bibliography"     "B" #'citar-open)

      ;; --- o: open --------------------------------------------------
      (:prefix "o"
       (:prefix "a"
        :desc "agenda: literature followup" "l" (cmd! (org-agenda nil "l")))
       :desc "dired in some dir" "d" #'consult-dir)

      ;; --- q: quit --------------------------------------------------
      (:prefix "q"
       :desc "force delete frame" "f" (cmd! (delete-frame nil t)))

      ;; --- s: search ------------------------------------------------
      (:prefix "s"
       :desc "search online"              "G" #'+lookup/online
       :desc "search org outline"         "o" #'consult-outline
       :desc "search project/dir (rg)"    "p" #'consult-ripgrep
       :desc "ripgrep in project/dir"     "d" #'consult-ripgrep
       :desc "grep in project/dir"        "D" #'consult-grep
       :desc "search buffer"              "s" #'consult-line
       :desc "search for file in dir (fd)" "f" #'consult-fd)

      ;; --- t: toggle ------------------------------------------------
      (:prefix "t"
       :desc "treemacs"         "t" #'treemacs
       :desc "popper toggle"    "p" #'popper-toggle
       :desc "toggle org timer" "i" #'my/toggle-org-timer
       :desc "tmr timer"        "I" #'tmr
       :desc "Toggle Laptop Mode" "L" #'my/laptop-mode)

      ;; --- TAB: tabs ------------------------------------------------
      (:prefix-map ("TAB" . "Tabs")
       :desc "Switch tab"         "TAB" #'tab-bar-switch-to-recent-tab
       :desc "Show tab list"      "l"   #'tab-bar-select-tab-by-name
       :desc "New tab"            "n"   #'tab-bar-new-tab
       :desc "Rename tab"         "r"   #'tab-bar-rename-tab
       :desc "Rename tab by name" "R"   #'tab-bar-rename-tab-by-name
       :desc "Close tab"          "d"   #'tab-bar-close-tab
       :desc "Close tab by name"  "D"   #'tab-bar-close-tab-by-name
       :desc "Close other tabs"   "1"   #'tab-bar-close-other-tabs
       :desc "Previous tab"       "j"   #'tab-previous
       :desc "Next tab"           "k"   #'tab-next))

;; --- w: windows -----------------------------------------------------
;; SPC w is Doom's `evil-window-map'; extend it in place so every default
;; survives (d delete, u/C-r winner undo/redo, s/v split, hjkl move,
;; HJKL relocate, o enlargen, = balance, | / _ set width/height).
(map! :map evil-window-map
      :desc "ace-window"           "w" #'ace-window ; replaces evil-window-next
      :desc "ace-delete-window"    "D" #'ace-delete-window
      :desc "equalize window sizes" "e" #'balance-windows
      :desc "minimize window"      "O" #'minimize-window
      :desc "window resize hydra"  "." #'hydra-window/body
      :desc "other frame"          "f" #'other-frame
      :desc "frame: small"         "F" #'my/make-small-frame
      :desc "frame: medium"        "M" #'my/make-medium-frame
      :desc "frame: large"         "S" #'my/make-large-frame)

;;; ============================================================
;;; 6. Function keys
;;; ============================================================

(map! "<f5>" #'revert-buffer
      "<f6>" #'modus-themes-toggle)

;;; ============================================================
;;; 7. Late bindings (need the leader map above to be built)
;;; ============================================================

;; C-c o opens the SPC a "agents" submenu (SPC is not leader while inserting).
;; Must come after section 5: `lookup-key' resolves now, and earlier in the
;; file "a" is still Doom+'s embark-act.  Org's own C-c o wins in org buffers.
(map! :i "C-c o" (lookup-key doom-leader-map "a"))
