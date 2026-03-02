;;; jupyter-claude-bridge.el --- Export Jupyter kernel connection for Claude Code -*- lexical-binding: t; -*-

(require 'jupyter-repl)
(require 'jupyter-server)

;;; Capture kernel connection file for Claude Code

(defvar jupyter-timer-fix--remote-runtime-dir nil
  "Cached remote Jupyter runtime directory path on chronos.")

(defun jupyter-timer-fix--get-remote-runtime-dir ()
  "Return (cached) remote Jupyter runtime dir, fetching via SSH if needed."
  (unless jupyter-timer-fix--remote-runtime-dir
    (let ((result (string-trim
                   (shell-command-to-string "ssh yonatan@chronos '/media/chronos/Env_Storage/yonatan/rapids_singlecell/bin/jupyter --runtime-dir'"))))
      (when (and result (not (string-empty-p result)))
        (setq jupyter-timer-fix--remote-runtime-dir result))))
  jupyter-timer-fix--remote-runtime-dir)

(defun jupyter-timer-fix--export-kernel-for-claude ()
  "Export ZMQ connection file for the current kernel so Claude Code can use it.
Gets the kernel ID from the REPL client, constructs the remote kernel JSON path,
and runs jup-attach-chronos to forward ZMQ ports locally.  Writes the resulting
local connection file path to ~/.cache/jupyter-current-kernel.txt."
  (when (and (boundp 'jupyter-current-client)
             jupyter-current-client
             (object-of-class-p jupyter-current-client 'jupyter-repl-client))
    (let ((client jupyter-current-client))
      (jupyter-kernel-action client
        (lambda (kernel)
          (when (jupyter-server-kernel-p kernel)
            (let* ((kernel-id (jupyter-server-kernel-id kernel))
                   (runtime-dir (jupyter-timer-fix--get-remote-runtime-dir))
                   (remote-path (and runtime-dir kernel-id
                                     (format "%s/kernel-%s.json"
                                             runtime-dir kernel-id))))
              (when remote-path
                (message "Setting up Claude kernel tunnel for %s..." kernel-id)
                (let ((proc (start-process "jup-attach-chronos" nil
                                           "jup-attach-chronos" "--no-nvim"
                                           "--kernel-file" remote-path))
                      (cache-file (expand-file-name "~/.cache/jupyter-current-kernel.txt")))
                  (make-directory (expand-file-name "~/.cache") t)
                  (set-process-filter
                   proc
                   (lambda (_proc output)
                     (message "jup-attach-chronos: %s" (string-trim output))
                     (when (string-match "Local kernel file: \\([^\n]+\\)" output)
                       (let ((local-path (string-trim (match-string 1 output))))
                         (with-temp-file cache-file
                           (insert local-path "\n"))
                         (message "Claude kernel ready: %s" local-path))))))))))))))

(add-hook 'jupyter-repl-mode-hook #'jupyter-timer-fix--export-kernel-for-claude)

(provide 'jupyter-claude-bridge)
