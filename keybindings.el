;;; ~/.doom.d/keybindings.el -*- lexical-binding: t; -*-

(map! :map (evil-org-mode-map emacs-lisp-mode-map prog-mode-map text-mode-map org-mode-map)
      :i "C-z" #'evil-undo
      :i "C-Z" #'evil-emacs-state
      :n "<up>"   (lambda nil (interactive) (scroll-down-command 1))
      :n "<down>" (lambda nil (interactive) (scroll-up-command 1))
      :v "<up>" #'evil-previous-visual-line
      :v "<down>" #'evil-next-visual-line
      :i "C-S-j" (lambda nil (interactive) (scroll-up-command 1))
      :i "C-S-k" (lambda nil (interactive) (scroll-down-command 1))
      :n "k" #'evil-previous-visual-line
      :n "j" #'evil-next-visual-line
      :ni "C-c +" #'(lambda ()
                      (interactive)
                      (call-interactively #'evil-next-flyspell-error)
                      (call-interactively #'flyspell-correct-at-point)
                      )
      :ni "C-c =" #'(lambda ()
                      (interactive)
                      (call-interactively #'evil-prev-flyspell-error)
                      (call-interactively #'flyspell-correct-at-point)
                      )
      :nv "C-e" #'evil-end-of-visual-line
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
 :i "C-M-SPC" #'evil-normal-state
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
      :desc "denote backlinks"  "nB" #'denote-link-backlinks
      ;; :desc "helm-bibtex"  "nB" #'helm-bibtex
      ;; :desc "citar references"  "nb" #'citar-open
      :desc "agenda for literature followup" "oal" #'(lambda () (interactive) (org-agenda nil "l"))
      :desc "M-x" :n "x" #'execute-extended-command
      :desc "scratch buffer" :n "z" #'doom/open-scratch-buffer
      )


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

;; (map!
;; (:prefix "g"
;;          (:prefix "f"
;;           :vn "j" #'evil-mc-make-and-goto-next-match
;;           :vn "J" #'evil-mc-skip-and-goto-next-match
;;           :vn "k" #'evil-mc-make-and-goto-prev-match
;;           :vn "K" #'evil-mc-skip-and-goto-prev-match)))

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
      :desc "buffer to new window" "bB" #'consult-buffer-other-window)

(map! :localleader
      :map (python-mode-map python-ts-mode-map)
      :n :desc "eval buffer" "eb" #'python-shell-send-buffer
      :n :desc "eval file" "ef" #'python-shell-send-file
      :n :desc "eval function" "ed" #'python-shell-send-defun
      :nv :desc "eval region" "er" #'python-shell-send-region
      )
(map! :map (python-mode-map python-ts-mode-map)
      :nv "C-<return>"  nil)

(map! :map (inferior-python-mode)
      :nv "C-k" #'windmove-up
      :nv "C-h" #'windmove-left
      )
(map! :after python
      :map (python-mode-map)
      :nv "C-<return>" #'python-shell-send-statement
      :nv "S-<return>" #'python-shell-send-statement
      :map (python-ts-mode-map)
      :nv "S-<return>" #'python-shell-send-statement
      :nv "C-<return>" #'python-shell-send-statement)

(map! :localleader
      :map matlab-mode-map
      :desc "eval buffer" :n "eb" #'(lambda () (interactive) (evil-goto-first-line) (evil-visual-line) (evil-goto-line) (matlab-shell-run-region))
      :desc "eval line" :n "el" #'(lambda () (interactive) (evil-visual-line) (matlab-shell-run-region))
      :desc "eval region" :n "er" #'matlab-shell-run-region
      :n "f" #'matlab-shell-help-at-point
      :n "s" #'matlab-shell
      )

(defun my-shell nil (interactive) (shell) (popper-toggle-type) (evil-normal-state))

(map! :leader
      (:prefix ("j" . "navigation")
       :desc "avy timer" "j" 'evil-avy-goto-char-timer
       :desc "avy line" "l" 'evil-avy-goto-line)

      (:prefix "t"
       :nv "T" #'treemacs
       :nv "t" #'popper-toggle
       :nv "<return>" #'popper-toggle
       :nv "s" #'shell
       :nv "i" #'my/toggle-org-timer
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
        ))

      (:prefix ("k" . "my commands")
       :desc "embark act" "a" #'embark-act
       :desc "select header content" "y" #'my/visual-inside-org-header
       :desc "copy header content" "h" #'my/yank-org-headline
       :desc "orgnv" "g" #'orgnv-browse
       :desc "orgnv + rebuild DB" "G" #'(lambda () (interactive) (my/orgnv-update-db) (orgnv-browse))
       :desc "kill all other windows" "o" 'delete-other-windows
       :desc "resize window to small" "f" 'my/make-small-frame
       :desc "resize window to medium" "m" 'my/make-medium-frame
       :desc "resize window to large" "M" 'my/make-large-frame
       :desc "make new frame" "F" 'make-frame-command
       :desc "writeroom mode" "w" #'writeroom-mode
       :desc "kill buffer and window" "D" #'kill-buffer-and-window
       :desc "kill buffer" "d" 'kill-current-buffer
       :desc "switch to previous buffer" "k" 'evil-switch-to-windows-last-buffer
       :desc "search and replace vim style" "s" #'my/search-replace
       :desc "search and replace vim style - in region" "S" #'my/search-replace-in-region
       :desc "refile subtree" "R" 'org-refile
       ;; :desc "paste from kill-ring" "p" #'consult-register
       :desc "register dwi" "p" #'consult-register-load
       :desc "point to register" "p" #'point-to-register
       ;; :desc "paste from kill-ring" "p" 'helm-show-kill-ring
       ;; :desc "helm org rifle" "R" 'helm-org-rifle
       :desc "run macro" "e" #'kmacro-end-and-call-macro
       :desc "generate laTex previews" "L" #'org-latex-preview

       (:prefix ("r" . "rectangle operations")
        "r" #'replace-rectangle
        :desc "paste rectangle" "p" #'yank-rectangle
        :desc "copy rectangle" "y"  #'copy-rectangle-as-kill
        :desc "cut rectangle" "x"  #'kill-rectangle
        :desc "push rectangle right" "r"  #'open-rectangle
        :desc "delete rectangle" "d"  #'clear-rectangle
        )

       (:prefix ("b" . "references")

        :desc "refresh bibliography" "r" #'citar-refresh
        :desc "open bibliography" "b" #'citar-open
        )
       ;; (:prefix ("i" . "insert stuff")
       ;;         (:prefix ("l" . "latex symbols")
       ;;          :desc "right double arrow"  "R" (kbd "$\\Rightarrow$")))
       ))

(setq
 avy-style 'at-full
 avy-all-windows 't
 avy-single-candidate-jump 't)

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
      :desc "window resize hydra" "w." 'hydra-window-resize/body
      :desc "Open project buffer in other window" "pF" #'projectile-find-file-dwim-other-window
      :desc "open a buffer and switch to its tabspace" "bt" #'tabspaces-switch-buffer-and-tab
      :desc "consult buffer" "z" #'consult-buffer
      :desc "consult buffer to new window" "Z" #'consult-buffer-other-window
      :desc "consult buffer" "SPC" #'consult-buffer
      :desc "consult buffer to new window" "S-SPC" #'consult-buffer-other-window
      )



(define-key evil-normal-state-map (kbd "J") 'evil-join)
(define-key evil-normal-state-map (kbd "K") 'join-line)

;; (setq key-chord-two-keys-delay 0.5)
;; (key-chord-define evil-insert-state-map "[[" #'org-roam-insert)
;; (key-chord-define term-mode-map "jk" #'evil-force-normal-state)
;; (key-chord-define evil-visual-state-map "jk" #'evil-force-normal-state)

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
(map!
 :nvi "C-;" #'embark-act
 :nvi "C-c  c" #'evil-yank
 :nvi "C-c  v" #'consult-yank-from-kill-ring
 ;; :nv "C-v" #'evil-paste-after
 :i "C-v" #'(lambda () (interactive) (backward-char) (evil-paste-after 1))
 :i "C-c p" #'(lambda () (interactive) (backward-char) (evil-paste-after 1))
 :i "C-c P" #'(lambda () (interactive) (backward-char 2) (evil-paste-after 1))
 :nvi "C-c  y" #'evil-yank
 :nv "C-c  p" #'evil-paste-after
 :nv "C-c  P" #'evil-paste-before
 :nvi "C-c  C-r" #'evil-redo
 :nvi "C-c  d" #'evil-delete
 :nvi "C-c  x" #'evil-delete

 :nvi "M-p" #'evil-paste-pop
 :nvi "M-n" #'evil-paste-pop-next
 :nvi "C-p" #'projectile-find-file

 :nvi "C-c  h" #'org-toggle-heading
 :nvi "C-c  i" #'org-toggle-item
 ;; :nvi "C-c  i" #'(lambda () (interactive) ((org-toggle-item) (org-end-of-line)))
 :nvi "C-c  a" #'(lambda () (interactive) (org-capture nil "a"))

 :nvi "C-c  o" #'(lambda () (interactive) (org-agenda nil "o"))
 ;; :i "C-c p" #'consult-yank-from-kill-ring
 ;; :i "C-c y" #'evil-yank
 :ni "C-c I" #'org-cite-insert
 ;; :ni "C-c I" #'org-ref-insert-cite-link
 :ni "C-c [" #'denote-link-or-create
 ;; :ni "C-c I" #'org-cite-insert

 :n "C-S-j" #'evil-mc-make-cursor-move-next-line
 :n "C-S-k" #'evil-mc-make-cursor-move-prev-line
 :nvi "M-j" #'drag-stuff-down
 :nvi "M-k" #'drag-stuff-up
 )

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

(setq scroll-preserve-screen-position 1)

(defun make-bold()
  (interactive)
  (if (use-region-p) (evil-surround-region (region-beginning) (region-end) t *)))

(map!
 (:when (modulep! :editor multiple-cursors)
   :prefix "g"
   :nv "z" #'my/mc-hydra/body))

;; (map! :map
;;    :i "RETURN" #'comint-send-input)

(map! :map shell-mode-map "C-l" #'comint-clear-buffer)

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
      :nv "q" (lambda nil
                (interactive)
                (add-hook 'kill-buffer-query-functions 'my/prompt-on-dired-buffer-kill)
                (+dired/quit-all)
                (remove-hook 'kill-buffer-query-functions 'my/prompt-on-dired-buffer-kill)
                )
      :n [f5] #'revert-buffer
      :n "=" #'diredp-ediff)

(map! :leader :nv "TAB" nil)

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

(setq visual-order-cursor-movement t)

(map! :map evil-org-mode-map
      "<f9>" #'+eval/region
      "C-c  k" #'org-capture
      "C-c  C-C" #'org-capture)

(map!
 "C-x  C-n" nil)

(map! :map gptel-mode-map
      "C-c  <return>" :desc "send the current region to GPTel" #'gptel-send
      )

(map!
 "C-x  C-x" #'org-capture
 "C-x  C-n" #'org-capture
 "C-c  C-<return>" :desc "send the current region to GPTel" #'gptel-send
 "C-c  <return>" :desc "run command with encoding" (lambda nil (interactive) (universal-coding-system-argument 'utf-8))
 )

(map! :leader :prefix "G"
      :desc "open the GPTel buffer" "G" #'gptel
      :desc "send the current region to GPTel" "R" #'gptel-send
      :leader :prefix "s"
      :desc "send query to gtp, include region if active" "g" #'gptel-quick)


(map! :map evil-org-mode-map :nvi "C-c k" #'evil-window-next
      :map global-map :nvi "C-c k" #'evil-window-next)

(map! :map global-map :nv "'" #'evil-goto-mark)

(fset 'copy-with-square-brackets
      (kmacro-lambda-form [?y ?a ?\]] 0 "%d"))

(map! :map citar-citation-map
      :desc "copy cite link"  "c" #'copy-with-square-brackets
      "d" #'citar-org-delete-citation)

(map! :map evil-org-mode-map
      :nv "k" #'evil-previous-visual-line
      :nv "j" #'evil-next-visual-line
      :nv "gk" #'evil-previous-visual-line
      :nv "gj" #'evil-next-visual-line
      :nv "V" #'evil-visual-screen-line)
