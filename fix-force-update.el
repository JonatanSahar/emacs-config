;;; fix-force-update.el --- Force update all associated buffers -*- lexical-binding: t; -*-

(defun jupyter-force-update-all-buffers ()
  "Force update all buffers associated with the current client."
  (when (and (boundp 'jupyter-current-client)
             jupyter-current-client)
    (let ((client jupyter-current-client))
      ;; Update REPL buffer
      (when (slot-boundp client 'buffer)
        (let ((repl-buf (oref client buffer)))
          (when (buffer-live-p repl-buf)
            (with-current-buffer repl-buf
              (force-mode-line-update t)))))

      ;; Update all buffers associated with this client
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (boundp 'jupyter-current-client)
                     (eq jupyter-current-client client)
                     (boundp 'jupyter-repl-interaction-mode)
                     jupyter-repl-interaction-mode)
            (force-mode-line-update t)))))))

;; Redefine the timer function to update ALL buffers
(defun jupyter-repl--update-execution-display ()
  "Called by timer to update modeline during execution."
  (condition-case err
      (when (and (derived-mode-p 'jupyter-repl-mode)
                 jupyter-repl--execution-start-time)
        ;; Instead of just force-mode-line-update, we update all related buffers
        (jupyter-force-update-all-buffers))
    (error
     (message "Error in timer update: %s" err))))

;; Redefine handle-status to update all buffers on transition
(defun jupyter-repl-force-update-wrapper (orig-fun &rest args)
  (apply orig-fun args)
  (jupyter-force-update-all-buffers))

(advice-add 'jupyter-handle-status :after #'jupyter-repl-force-update-wrapper)

(message "✓ Installed forced updates for all buffers")
(message "Try running a cell now!")

(provide 'fix-force-update)
