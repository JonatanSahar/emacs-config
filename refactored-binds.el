;;; refactored-binds.el -*- lexical-binding: t; -*-

(map!
 :map (evil-org-mode-map prog-mode-map text-mode-map org-mode-map)

 :i "C-z" #'evil-undo
 :i "C-Z" #'evil-emacs-state
 :i "C-S-j" (lambda nil (interactive) (scroll-up-command 1))
 :i "C-S-k" (lambda nil (interactive) (scroll-down-command 1))
 :i "M-h" #'org-beginning-of-line
 :i "M-l" #'org-end-of-line
 :i "C-S-j" #'next-line
 :i "C-S-k" #'previous-line
 :i "C-'" #'right-char
 :i "C-;" #'left-char
 :i "C-k" #'previous-line
 :i "C-j" #'next-line
 :i "C-l" #'right-char
 :i "C-S-l" #'right-word
 :i "C-h" #'left-char
 :i "C-S-h" #'left-word

 :n "<up>" (lambda nil (interactive) (scroll-down-command 1))
 :n "<down>" (lambda nil (interactive) (scroll-up-command 1))
 :n "C-u S-v" #'evil-visual-screen-line
 :n "C-S-v" #'evil-visual-screen-line
 :n "k" #'evil-previous-visual-line
 :n "j" #'evil-next-visual-line

 :v "<up>" #'evil-previous-visual-line
 :v "<down>" #'evil-next-visual-line

 :ni "C-c +" (lambda ()
               (interactive)
               (call-interactively #'evil-next-flyspell-error)
               (call-interactively #'flyspell-correct-at-point)
               )
 :ni "C-c =" (lambda ()
               (interactive)
               (call-interactively #'evil-prev-flyspell-error)
               (call-interactively #'flyspell-correct-at-point)
               )

 :nv "C-e" #'evil-end-of-visual-line
 :nvi "C-z" #'evil-undo
 :nvi "C-y" #'evil-redo
 )

(map!
 :nvi "M-q" #'+workspace/other
 :ni "C-{" #'org-roam-node-insert
 :ni "C-}" #'org-ref-insert-link)

(defun my/save-and-change-to-normal ()
  (interactive)
  (evil-normal-state)
  (save-buffer)
  )

(map!
 :nvi "C-\\" #'toggle-input-method
 :nvi "C-s" (lambda ()
              (interactive)
              (evil-normal-state)
              (save-buffer)
              )
 :i "S-SPC" #'evil-normal-state
 :i "S-<return>" #'evil-normal-state

 :map evil-org-mode-map
 :i "C-h" #'evil-window-left
 :i "C-l" #'evil-window-right
 :i "C-k" #'evil-window-up
 :i "C-j" #'evil-window-down
 )
(map!
 :map pdf-view-mode-map
 :nvi "go" nil
 :nvi "C-j" nil
 :nvi "C-k" nil
 :nvi "C-c O" (lambda () (interactive) (dired-jump) (dired-open))
 )

(map!
 :map pdf-occur-buffer-mode-map
 :nv "C-j" #'next-error-no-select
 :nv "C-k" #'previous-error-no-select
 )

(map!
 :map matlab-mode-map
 :nv "C-S-m" (lambda ()
               (interactive)
               (org-switch-to-buffer-other-window "*MATLAB*"))
 :ni "C-c h" #'matlab-shell-help-at-point
 )

(map!
 :map matlab-shell-mode-map
 :ni "C-c l" #'comint-clear-buffer
 :nv "C-l" #'windmove-right
 :nv "C-h" #'windmove-left
 :nv "C-j" #'windmove-down
 :nv "C-k" #'windmove-up
 )

(map!
 :map helm-map
 "C-j" #'helm-next-line
 "C-k" #'helm-previous-line
 [control-backspace] #'backward-kill-word
 "ESC" #'helm-exit-minibuffer
 )

(map!
 :map helm-find-files-map
 [control-backspace] #'backward-kill-word
 )

(map!
 :map evil-org-mode-map
 :nvi "go" (kbd "SPC o - & RET")
 :vin "gl" nil
 :nvi "C-j" #'windmove-down
 :nvi "C-k" #'windmove-up
 :localleader :n
 "E" #'my/export-org-to-docx
 "n" #'org-narrow-to-subtree
 "N" #'widen
 )

(map!
 :map company-active-map
 "C-s" #'my/save-and-change-to-normal
 )

(map!
 :nvi
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

(map!
 :map org-roam-backlinks-mode-map
 "return" #'org-open-at-point
 )

(map!
 :map pdf-view-mode-map
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
 :nvi "I" #'org-noter-insert-note
 )

(map!
 :leader
 :nv
 "bb" nil
 "is" nil
 "ir" #'org-ref-insert-link
 "fc" #'my/get-buffer-name
 "nB" #'denote-link-backlinks
 "oal" (lambda () (interactive) (org-agenda nil "l"))
 :desc "M-x" :n "x" #'execute-extended-command
 :desc "scratch buffer" :n "z" #'doom/open-scratch-buffer
 :desc "search org outline" "sG" #'+lookup/online
 :desc "search org outline" "so" #'consult-outline
 :desc "search project/dir" "sp" #'consult-ripgrep
 :desc "search project/dir" "sd" #'consult-ripgrep
 :desc "search buffer"  "ss" #'consult-line
 :desc "equate window sizes" "we" #'balance-windows
 :desc "minimize window" "wmm" #'minimize-window
 :desc "minimize window" "wO" #'minimize-window
 :desc "maximize window" "wmM" #'doom/window-maximize-buffer
 :desc "switch to buffer" "bb" #'consult-buffer
 :desc "buffer to new window" "bB" #'consult-buffer-other-window
 )

(map!
 :n "gO" #'+evil/insert-newline-above
 :n "go" #'+evil/insert-newline-below
 :n "gK" #'+evil/insert-newline-above
 :n "gJ" #'+evil/insert-newline-below
 :n "g[" #'denote-link-or-create
 :n "g]" #'kmacro-end-an-call-macro
 :n "g." #'er/expand-region
 :n "gh" #'execute-extended-command
 :n "gf" #'execute-extended-command
 :n "g+" #'evil-numbers/inc-at-pt
 :nv "E" #'evil-forward-WORD-end
 :nv "W" #'evil-forward-WORD-begin
 :nvi "C-S-l" #'org-roam-insert
 :nv "C-l" #'windmove-right
 :nv "C-h" #'windmove-left
 :nv "C-j" #'windmove-down
 :nv "C-k" #'windmove-up
 :nvi "M-;" #'toggle-input-method
 :n "z=" #'my/indent-buffer
 :prefix "z"
 :nv "z" (lambda ()
           (interactive)
           (call-interactively #'evil-next-flyspell-error
                               (defun my-shell nil (interactive) (shell) (popper-toggle-type) (evil-normal-state))

                               (map! :leader
                                     (:prefix ("j" . "navigation")
                                      :desc "avy timer" "j" 'evil-avy-goto-char-timer
                                      :desc "avy line" "l" 'evil-avy-goto-line)

                                     (:prefix "t"
                                      :nv "T" #'treemacs
                                      :nv "t" #'popper-toggle-latest
                                      :nv "<return>" #'popper-toggle-latest
                                      :nv "s" #'shell)

                                     (:prefix "o"
                                      :nv :desc "windows cmd" "w" #'shell
                                      :nv :desc "eshell" "s" #'my-shell
                                      :nv :desc "dired in some dir" "d" #'consult-dir
                                      :nv :desc "dired in a new window" "D" #'consult-dir)

                                     (:prefix "g"
                                      :nv "p" #'magit-pull)

                                     (:prefix "b"
                                      :desc "reload buffer" "r" #'my/revert-buffer-no-confirm
                                      :desc "revert buffer" "R" #'revert-buffer)

                                     (:prefix ("i" . "insert stuff")
                                      :nv :desc "copy and comment line(s)" "C" #'evilnc-copy-and-comment-lines
                                      :desc "make evil-mc-cursor here" "c" #'my/make-cursor-here
                                      :desc "add line above" "k" #'+evil/insert-newline-above
                                      :desc "add line below" "j" #'+evil/insert-newline-below

                                      (:prefix ("a" . "anki cloze")
                                       :map org-mode-map
                                       :nv "0" #'anki-editor-reset-cloze-number
                                       :nv "C" #'anki-editor-cloze-region-auto-incr
                                       :nv "c" #'anki-editor-cloze-region-dont-incr
                                       :nv "w" #'anki-editor-cloze-word-under-cursor-auto-incr)

                                      (:prefix ("l" . "latex symbols")
                                       :desc "right double arrow"  "r" (kbd "$\\Rightarrow$"))

                                      (:prefix ("s" . "surround stuff")
                                       :desc "surround object with bold"  "*" (kbd "ysio*")
                                       :desc "surround object with quotes"  "\"" (kbd "ysio\"")
                                       :desc "surround object with single quotes"  "\'" (kbd "ysio\'")
                                       :desc "surround object with parens" "\)" (kbd "ysio\)")
                                       :desc "surround object with brackets" "\]" (kbd "ysio\]")))

                                     (:prefix ("k" . "my commands")
                                      :desc "embark act" "a" #'embark-act
                                      :desc "select header content" "y" #'my/visual-inside-org-header
                                      :desc "copy header content" "h" #'my/yank-org-headline
                                      :desc "orgnv" "g" #'orgnv-browse
                                      :desc "orgnv + rebuild DB" "G" #'(lambda () (interactive) (my/orgnv-update-db) (orgnv-browse))
                                      :desc "kill all other windows" "o" 'delete-other-windows
                                      :desc "resize window to small" "f" 'my/make-small-frameHere's the rest of your code:

                                      ```elisp
                                      :desc "resize window to medium" "m" 'my/make-medium-frame
                                      :desc "make new frame" "F" 'make-frame-command
                                      :desc "writeroom mode" "w" #'writeroom-mode
                                      :desc "kill buffer and window" "D" #'kill-buffer-and-window
                                      :desc "kill buffer" "d" 'kill-current-buffer
                                      :desc "switch to previous buffer" "k" 'evil-switch-to-windows-last-buffer
                                      :desc "search and replace vim style" "s" #'my/search-replace
                                      :desc "search and replace vim style - in region" "S" #'my/search-replace-in-region
                                      :desc "refile subtree" "R" 'org-refile
                                      :desc "register dwi" "p" #'consult-register-load
                                      :desc "point to register" "p" #'point-to-register
                                      :desc "run macro" "e" #'kmacro-end-and-call-macro
                                      :desc "generate laTex previews" "L" #'org-latex-preview

                                      (:prefix ("r" . "rectangle operations")
                                       "r" #'replace-rectangle
                                       :desc "paste rectangle" "p" #'yank-rectangle
                                       :desc "copy rectangle" "y"  #'copy-rectangle-as-kill
                                       :desc "cut rectangle" "x"  #'kill-rectangle
                                       :desc "push rectangle right" "r"  #'open-rectangle
                                       :desc "delete rectangle" "d"  #'clear-rectangle)

                                      (:prefix ("b" . "references")
                                       :desc "refresh bibliography" "r" #'citar-refresh
                                       :desc "open bibliography" "b" #'citar-open)
                                      )
                                     )
                               ;; Avy settings
                               (setq avy-style 'at-full
                                     avy-all-windows 't
                                     avy-single-candidate-jump 't)

                               ;; Hydra for window management
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

                               ;; Leader key mappings
                               (map! :leader
                                     :desc "window resize hydra" "w." 'hydra-window-resize/body
                                     :desc "Open project buffer in other window" "pF" #'projectile-find-file-dwim-other-window
                                     :desc "open a buffer and switch to its tabspace" "bt" #'tabspaces-switch-buffer-and-tab
                                     :desc "consult buffer" "z" #'consult-buffer
                                     :desc "consult buffer to new window" "Z" #'consult-buffer-other-window
                                     :desc "consult buffer" "SPC" #'consult-buffer
                                     :desc "consult buffer to new window" "S-SPC" #'consult-buffer-other-window)

                               ;; Key mappings for evil-normal-state
                               (define-key evil-normal-state-map (kbd "J") 'evil-join)
                               (define-key evil-normal-state-map (kbd "K") 'join-line)

                               ;; Key mappings for wordnut-mode
                               (evil-define-key 'normal wordnut-mode-map (kbd "q") 'quit-window)
                               (evil-define-key 'normal wordnut-mode-map (kbd "RET") 'wordnut-lookup-current-word)
                               (evil-define-key 'normal wordnut-mode-map (kbd "h") 'wordnut-history-backward)
                               (evil-define-key 'normal wordnut-mode-map (kbd "l") 'wordnut-history-forward)
                               (evil-define-key 'normal wordnut-mode-map (kbd "H") 'wordnut-history-lookup)
                               (evil-define-key 'normal wordnut-mode-map (kbd "/") 'wordnut-search)
                               (evil-define-key 'normal wordnut-mode-map (kbd "o") 'wordnut-show-overview)

                               ;; Some more mappings
                               (global-set-key [f11] 'flyspell-correct-at-point)

                               ;; Minibuffer mappings
                               (map! :map minibuffer-mode-map :nvi ";" #'embark-act)
