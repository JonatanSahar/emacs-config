(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-digit-bound-motions '(evil-beginning-of-visual-line))
 '(helm-ag-base-command "rg --no-heading")
 '(helm-ag-success-exit-status '(0 2))
 '(linum-format 'dynamic)
 '(matlab-shell-command-switches '("-nodesktop -nosplash ") t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path 'file)
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io\\(?:/\\|\\'\\)" "\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(package-selected-packages '(aio popper emacsql-sqlite3 emacsql-sqlite all-the-icons))
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
