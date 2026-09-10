;;; $DOOMDIR/repl.el -*- lexical-binding: t; -*-
;; jupyter, code-cells, python, eglot, conda, format/flycheck, jupytext, jupyter cell-status overlay

(use-package! jupyter
  :config
  (load! "jupyter-timer-fix")
  (load! "jupyter-claude-bridge")
  (setq jupyter-eval-short-result-max-lines 1)
  (map! :map jupyter-repl-mode-map
        :i "C-k" #'jupyter-repl-history-previous
        :i "C-j" #'jupyter-repl-history-next
        :nvi "C-e" #'evil-end-of-line-or-visual-line
        :i "<up>" #'jupyter-repl-history-previous
        :n  "gj" #'evil-avy-goto-char-timer
        :i "<down>" #'jupyter-repl-history-next
        ;; Kernel disconnection commands
        :nvi "C-c C-d" #'jupyter-repl-disconnect-kernel
        :nvi "C-c C-s" #'jupyter-repl-connection-status)

  (map! :map (python-mode-map python-ts-mode-map ess-mode-map)
        ;; Jupyter Integration
        :nv "C-<return>" #'jupyter-eval-line-or-region
        :nv "S-<return>" #'jupyter-eval-line-or-region ; Alternative
        ;; :v "C-c <return>" #'python-shell-send-region ; Send region to shell (standard python.el)
        :localleader
        :n :desc "eval buffer" "eb" #'jupyter-eval-buffer
        :n :desc "eval function" "ed" #'jupyter-eval-defun
        :nv :desc "eval region" "er" #'jupyter-eval-region
        :n :desc "eval enclosing block" "ef" #'my/jupyter-eval-enclosing-block)

  ;; DONE I want to map "ef" to a new function that goes to the prev open paren
  ;; being either ( or {, then evil-visual-line and evil-jump-item, and then
  ;; calls jupyter-eval-region
  (defun my/jupyter-eval-enclosing-block ()
    "Eval the enclosing paren/brace block, expanded to full lines."
    (interactive)
    (save-excursion
      (backward-up-list 1 t t)
      (let* ((beg (line-beginning-position))
             (end (progn (forward-sexp) (line-end-position))))
        (jupyter-eval-region nil beg end))))

  (add-hook! 'jupyter-repl-mode-hook #'electric-pair-mode))

;; code cells
(use-package! code-cells
  :config

  (defun my/insert-code-cell()
    (interactive)
    (evil-open-above 1)
    (insert "#%%")
    )

  (defun my/insert-markdown-cell()
    (interactive)
    (evil-open-above 1)
    (insert "#%% [markdown]")
    )

  (defun my/delete-code-cell()
    (interactive)
    (code-cells-mark-cell)
    (let ((beg (region-beginning))
          (end (region-end)))
      (evil-delete beg end)))

  (defun my/code-cell-to-md()
    (interactive)
    (beginning-of-line)
    (when (not (looking-at "^#%%.*"))
      (code-cells-backward-cell))
    (evil-append-line 1)
    (insert " [markdown]")
    (evil-force-normal-state)
    )

  (defun my/md-cell-to-code()
    (interactive)
    (beginning-of-line)
    (when (not (looking-at "^#%%.*"))
      (code-cells-backward-cell))
    (evil-end-of-visual-line)
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "\\[markdown\\]" (line-end-position) t)
        (replace-match ""))))

  (defun my/eval-code-cell-and-next()
    (interactive)
    (call-interactively #'code-cells-eval)
    (call-interactively #'code-cells-forward-cell)
    (evil-force-normal-state)
    )

  (defun my/code-cells-eval-text-object()
    (interactive)
    (when (evil-insert-state-p)
      (evil-normal-state))
    (unless (evil-visual-state-p)
      (evil-visual-char))
    (call-interactively #'evil-inner-symbol)
    (evil-visual-expand-region)
    (code-cells-eval (region-beginning) (region-end))
    )

  (defun my/code-cells-eval-line ()
    (interactive)
    ;; Get the beginning and end positions of the current line
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      ;; Call the eval function with the positions
      (code-cells-eval beg end)))

  (defun my/tag-cell ()
    (interactive)
    (beginning-of-line)
    (when (not (looking-at "^#%%.*"))
      (code-cells-backward-cell))
    (let* ((tag-regex "# %%.*tags=\\[\\(.*?\\)\\]")
           (all-tags (save-excursion
                       (goto-char (point-min))
                       (let (tags)
                         (while (re-search-forward tag-regex nil t)
                           (let ((tag-str (match-string 1)))
                             (setq tags (append tags (split-string tag-str ", " t "\"")))))
                         tags)))
           (tag (completing-read "Enter tag: " (delete-dups all-tags))))
      (save-excursion
        (end-of-line)
        (if (looking-back "# %% tags=\\[\\(.*?\\)\\]" (line-beginning-position))
            (progn
              (backward-char 1)
              (unless (looking-back "\\[" (1- (point)))
                (insert ", "))
              (insert (format "\"%s\"" tag)))
          (insert (format " tags=[\"%s\"]" tag))))))

  (defalias 'my-code-cells-eval-line-normal
    (kmacro "C-g V C-c C-c C-g"))
  (defalias 'my-code-cells-eval-line-insert
    (kmacro "C-g V C-c C-c C-g i"))


  (map! :map code-cells-mode-map
        :nvi "C-c C-k" #'code-cells-backward-cell
        :nvi "C-c C-j" #'code-cells-forward-cell
        :nvi "C-c C-<up>" #'code-cells-move-cell-up
        :nvi "C-c C-<down>" #'code-cells-move-cell-down
        :nvi "C-c E" #'code-cells-eval-above
        :nvi "C-c C-c" #'code-cells-eval
        :nvi "C-c C-\." #'code-cells-eval
        :v "C-<return>" #'code-cells-eval
        :ni "C-<return>" #'my/code-cells-eval-line
        ;; :n "C-<return>" #'my-code-cells-eval-line-normal
        ;; :i "C-<return>" #'my-code-cells-eval-line-insert
        :nvi "S-<return>" #'my/eval-code-cell-and-next
        :nvi "C-S-<return>" #'my/code-cells-eval-text-object
        :nvi "C-c C-o" #'my/code-cells-eval-text-object
        :nvi "C-c i" #'my/insert-code-cell
        :i "C-c i" #'my/insert-code-cell

        :nvi "C-c I" #'my/insert-markdown-cell
        :nvi "C-c k" #'jupyter-repl-pop-to-buffer
        :nvi "C-c m" #'my/code-cell-to-md
        :nvi "C-c M" #'my/md-cell-to-code
        :nvi "C-c d" #'my/delete-code-cell
        :nvi "C-c t" #'my/tag-cell
        :nvi "C-c C-v" (lambda () (interactive) (code-cells-mark-cell) (exchange-point-and-mark)))

  (map! :map python-mode-map
        :nvi "C-c C-o" #'jupyter-eval-line-or-region
        :nvi "C-c k" #'jupyter-repl-pop-to-buffer
        )
  )

(add-hook! python-mode #'code-cells-mode)

(after! conda
  (setopt conda-anaconda-home (expand-file-name "~/miniforge3")))

(after! python
  (setq python-check-command "ruff check"))
;; Run jupytext on save for Python files
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook #'my/run-jupytext-on-save nil t))))

(after! format
  (setq +format-on-save-enabled-modes '(prog-mode))
  (set-formatter! 'ruff-format
    '("ruff" "format" "--stdin-filename" "%file" "-")
    :modes '(python-mode python-ts-mode)))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(after! eglot
  (setq eglot-workspace-configuration
        '((:pylsp .
           (:plugins
            (:pycodestyle (:enabled :json-false)
             :flake8      (:enabled :json-false)
             :pylint      (:enabled :json-false)
             :pyflakes    (:enabled :json-false)
             :jedi_completion (:enabled t))))

          (:pyright .
                    (:typeCheckingMode "off"
                     :disableLanguageServices :json-true
                     :disableOrganizeImports :json-false)))))

(defun my/jupytext-file (prefix)
  "Run jupytext to set formats to py:percent,ipynb for the current file.
For .py files, only run if a corresponding .ipynb file exists, unless a prefix argument (C-u) is given, which forces creation."
  (interactive "P")
  (when buffer-file-name
    (let* ((force-create (consp prefix)) ; C-u makes prefix a list like '(4)
           (file-path buffer-file-name)
           (extension (file-name-extension file-path))
           (base-name (file-name-sans-extension file-path))
           (ipynb-path (concat base-name ".ipynb")))
      (cond
       ;; If it's an ipynb file, always run jupytext
       ((string= extension "ipynb")
        (message "Running jupytext on %s" file-path)
        (start-process "jupytext-process" "*jupytext-output*"
                       "jupytext" "--set-formats" "py:percent,ipynb" file-path)
        (message "Jupytext conversion initiated for %s" file-path))
       ;; If it's a py file, run if the corresponding ipynb exists OR if forced by prefix arg
       ((and (string= extension "py")
             (or (file-exists-p ipynb-path) force-create))
        (if force-create
            (message "Forcing jupytext on %s to create/pair with %s" file-path ipynb-path)
          (message "Running jupytext on %s (paired with %s)" file-path ipynb-path))
        (start-process "jupytext-process" "*jupytext-output*"
                       "jupytext" "--set-formats" "py:percent,ipynb" file-path)
        (message "Jupytext conversion initiated for %s" file-path))))))

;; Jupyter cell execution status (per buffer)
(defface my/jupyter-cell-status-running
  '((t :inherit warning))
  "Face for running Jupyter cell status.")

(defface my/jupyter-cell-status-done
  '((t :inherit success))
  "Face for completed Jupyter cell status.")

(defface my/jupyter-cell-status-idle
  '((t :inherit shadow))
  "Face for idle Jupyter cell status.")

(defvar-local my/jupyter-cell-status--overlay nil)
(defvar-local my/jupyter-cell-status--requests nil)
(defvar-local my/jupyter-cell-status--pending 0)
(defvar-local my/jupyter-cell-status--last-start nil)
(defvar-local my/jupyter-cell-status--last-finish nil)
(defvar-local my/jupyter-cell-status--timer nil)
(defvar-local my/jupyter-cell-status--cleanup-hook-added nil)

(defun my/jupyter-cell-status--ensure-overlay ()
  (unless (overlayp my/jupyter-cell-status--overlay)
    (setq my/jupyter-cell-status--overlay (make-overlay (point-min) (point-min) nil t t))
    (overlay-put my/jupyter-cell-status--overlay 'priority 9999)
    (overlay-put my/jupyter-cell-status--overlay 'evaporate t))
  (move-overlay my/jupyter-cell-status--overlay (point-min) (point-min))
  (unless my/jupyter-cell-status--cleanup-hook-added
    (add-hook 'kill-buffer-hook #'my/jupyter-cell-status--cleanup nil t)
    (add-hook 'change-major-mode-hook #'my/jupyter-cell-status--cleanup nil t)
    (setq my/jupyter-cell-status--cleanup-hook-added t)))

(defun my/jupyter-cell-status--format-elapsed (start-time)
  (when start-time
    (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
           (total (max 0 (floor elapsed)))
           (minutes (/ total 60))
           (seconds (% total 60))
           (hours (/ minutes 60))
           (minutes (% minutes 60)))
      (if (> hours 0)
          (format "%dh%02dm%02ds" hours minutes seconds)
        (format "%02dm%02ds" minutes seconds)))))

(defun my/jupyter-cell-status--format-line ()
  (cond
   ((> my/jupyter-cell-status--pending 0)
    (let* ((count (if (> my/jupyter-cell-status--pending 1)
                      (format " (%d)" my/jupyter-cell-status--pending)
                    ""))
           (elapsed (my/jupyter-cell-status--format-elapsed
                     my/jupyter-cell-status--last-start))
           (elapsed-suffix (if elapsed (format " %s" elapsed) "")))
      (format "Jupyter: running%s%s\n" count elapsed-suffix)))
   (my/jupyter-cell-status--last-finish
    (format "Jupyter: done at %s\n"
            (format-time-string "%H:%M:%S" my/jupyter-cell-status--last-finish)))
   (t
    "Jupyter: idle\n")))

(defun my/jupyter-cell-status--update ()
  (my/jupyter-cell-status--ensure-overlay)
  (let* ((text (my/jupyter-cell-status--format-line))
         (face (cond
                ((> my/jupyter-cell-status--pending 0) 'my/jupyter-cell-status-running)
                (my/jupyter-cell-status--last-finish 'my/jupyter-cell-status-done)
                (t 'my/jupyter-cell-status-idle))))
    (overlay-put my/jupyter-cell-status--overlay
                 'before-string
                 (propertize text 'face face))))

(defun my/jupyter-cell-status--ensure-timer ()
  (unless (timerp my/jupyter-cell-status--timer)
    (setq my/jupyter-cell-status--timer
          (run-with-timer 1 1 #'my/jupyter-cell-status--tick (current-buffer)))))

(defun my/jupyter-cell-status--stop-timer ()
  (when (timerp my/jupyter-cell-status--timer)
    (cancel-timer my/jupyter-cell-status--timer)
    (setq my/jupyter-cell-status--timer nil)))

(defun my/jupyter-cell-status--prune-requests ()
  (let (pending)
    (dolist (req my/jupyter-cell-status--requests)
      (when (and req (not (jupyter-request-idle-p req)))
        (push req pending)))
    (setq my/jupyter-cell-status--requests (nreverse pending))
    (setq my/jupyter-cell-status--pending (length my/jupyter-cell-status--requests))))

(defun my/jupyter-cell-status--tick (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((was-running (> my/jupyter-cell-status--pending 0)))
        (when my/jupyter-cell-status--requests
          (my/jupyter-cell-status--prune-requests))
        (when (and was-running (zerop my/jupyter-cell-status--pending))
          (setq my/jupyter-cell-status--last-finish (current-time))
          (my/jupyter-cell-status--stop-timer))
        (my/jupyter-cell-status--update)))))

(defun my/jupyter-cell-status--start (req buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/jupyter-cell-status--requests
            (cons req my/jupyter-cell-status--requests))
      (setq my/jupyter-cell-status--pending (length my/jupyter-cell-status--requests))
      (setq my/jupyter-cell-status--last-start (current-time))
      (setq my/jupyter-cell-status--last-finish nil)
      (my/jupyter-cell-status--ensure-timer)
      (my/jupyter-cell-status--update))))

(defun my/jupyter-cell-status--cleanup ()
  (my/jupyter-cell-status--stop-timer)
  (setq my/jupyter-cell-status--requests nil)
  (setq my/jupyter-cell-status--pending 0)
  (setq my/jupyter-cell-status--last-start nil)
  (setq my/jupyter-cell-status--last-finish nil)
  (when (overlayp my/jupyter-cell-status--overlay)
    (delete-overlay my/jupyter-cell-status--overlay)
    (setq my/jupyter-cell-status--overlay nil)))

(defun my/jupyter-cell-status--around-eval (orig-fn &rest args)
  (let ((buffer (current-buffer))
        (req (apply orig-fn args)))
    (when (and (buffer-live-p buffer) (jupyter-request-p req))
      (my/jupyter-cell-status--start req buffer))
    req))

(after! jupyter
  (advice-add 'jupyter-eval-string :around #'my/jupyter-cell-status--around-eval))
