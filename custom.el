(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5718ea4d49350ab88e353fd66f3d4d3954e890b4d3dd7928f9b6dcb0e76ac8bd" "929d5aa75ac40de51b6fccc766f5d431c16306c754e8dde599bd8d5623ff0ced" default))
 '(evil-digit-bound-motions '(evil-beginning-of-visual-line))
 '(linum-format 'dynamic)
 '(matlab-shell-command-switches '("-nodesktop -nosplash ") t)
 '(org-agenda-files
   '("~/notes/20230323T111555--paper-summaries-from-guy__thesis.org"))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path 'file)
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)" "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(tab-bar-new-tab-choice "*doom*")
 '(tab-bar-tab-name-function 'my/name-tab-by-project-or-default)
 '(warning-suppress-log-types '(((yasnippet zombie)) ((yasnippet zombie)) (defvaralias)))
 '(warning-suppress-types
   '(((yasnippet zombie))
     ((yasnippet zombie))
     ((yasnippet zombie))
     (defvaralias))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit default :foreground "steel blue" :strike-through nil :underline nil :slant normal :weight semi-bold :family "Roboto Mono"))))
 '(line-number-current-line ((t (:inherit (hl-line default) :foreground "light steel blue" :strike-through nil :underline nil :slant normal :weight semi-bold :family "Roboto Mono"))))
 '(org-default ((t (:family "Heebo")))))
