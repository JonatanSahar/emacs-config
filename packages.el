;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-download)
(package! denote)
(package! denote-markdown)
(package! denote-silo)
(package! consult-notes)
(package! citar-denote)
(package! captain) ;; auto capitalize words
(package! tabspaces)
(package! popper)
(package! tmr)
(package! gptel)
(package! jupyter)

(package! code-cells
  :recipe (:host github :repo "astoff/code-cells.el" :files ("*.el")))
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! magit-gptcommit)

(package! highlight-thing)
(package! dwim-shell-command)
(package! drag-stuff)
(package! spacious-padding)
(package! aidermacs)
(package! claude-code
  :recipe (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
           :files ("*.el" (:exclude "images/*"))))
(package! agent-shell)
