;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! org-roam)
(package! citar-org-roam)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! org-roam-ui)

;; (package! org :pin "73875939a8b5545")
(package! org-pretty-tags :pin "40fd72f3e7...")
(package! org-superstar)
(package! org-super-agenda :pin "dd0d104c26...")
(package! helm-org-rifle)
(package! org-download)

(package! simple-httpd)
(package! f)


(package! org-transclusion)

;; denote
(package! denote)
(package! consult-notes)
(package! citar-denote)



;; writing experience
(package! olivetti)
(package! captain) ;; auto capitalize words
(package! helm-flyspell)
(package! consult-company)
(package! company-statistics)
(package! company-wordfreq)
;; (package! flyspell-lazy :pin "3ebf68cc9e...")
(package! company-box)
(package! compat)

;; general utils
(package! wgrep)
(package! helm-ag)
(package! multiple-cursors)
(package! counsel)
(package! general)
(package! emms)
(package! ov)
(package! s)
(package! promise)
(package! org-mime)
(package! dired-hacks-utils)
(package! dired-filter)
(package! dired-ranger)
(package! tabspaces)
(package!  popper)
(package! ibuffer-projectile)
(package! consult-projectile)

(package! evil-better-visual-line)


;; ivy/helm alternative
(package! consult)
(package! consult-flycheck)
(package! embark)
(package! embark-consult)
(package! prescient)
(package! selectrum-prescient)
(package! marginalia)
(package! ripgrep)
(package! orderless)
(package! consult-dir)

;; reference/bibliography
(package! org-ref)
(package!  helm-bibtex)
(package!  bibtex-completion)
(package!  consult-bibtex
:recipe (:host github
         :repo "mohkale/consult-bibtex"))

;; code related
(package! matlab-mode)
(package! lsp-pyright)
(package! pyenv)
(package! poetry)
(package! virtualenvwrapper)
(package! conda)
(package! emacs-aio)


;; themes
(package! modus-themes)
(package! ef-themes)
