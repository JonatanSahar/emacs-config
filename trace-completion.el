;;; trace-completion.el --- Trace completion handling -*- lexical-binding: t; -*-

(defun trace-status-transitions (orig-fun client req msg)
  "Trace status transitions."
  (let* ((old-state (jupyter-execution-state client))
         (new-state (plist-get (plist-get msg :content) :execution_state)))

    (when (and (cl-typep client 'jupyter-repl-client) new-state)
      (message "[TRACE] Status change: %s -> %s" old-state new-state)

      ;; Check if this should trigger completion
      (when (and (equal new-state "idle") (equal old-state "busy"))
        (message "[TRACE]   → Should call on-execution-complete!")))

    (funcall orig-fun client req msg)

    ;; Check what happened
    (when (and (cl-typep client 'jupyter-repl-client) new-state)
      (with-current-buffer (oref client buffer)
        (message "[TRACE]   After: start-time=%s completion-status=%s"
                 jupyter-repl--execution-start-time
                 jupyter-repl--last-completion-status)))))

(defun trace-completion ()
  "Trace execution completion to see why timer doesn't reset."
  (interactive)

  (message "")
  (message "=== Enabling Completion Trace ===")
  (message "")

  (advice-add 'jupyter-handle-status :around #'trace-status-transitions)

  (message "✓ Tracing enabled")
  (message "Execute a cell and watch for completion messages")
  (message ""))

(defun untrace-completion ()
  "Disable completion tracing."
  (interactive)
  (advice-remove 'jupyter-handle-status #'trace-status-transitions)
  (message "Tracing disabled"))

(defun check-completion-state ()
  "Check the current completion state."
  (interactive)

  (unless (and (boundp 'jupyter-current-client) jupyter-current-client)
    (error "No jupyter client"))

  (let* ((client jupyter-current-client)
         (repl-buf (oref client buffer)))

    (message "")
    (message "=== Completion State ===")
    (message "")

    (with-current-buffer repl-buf
      (message "REPL Buffer: %s" (buffer-name))
      (message "")
      (message "Execution state: %s" (jupyter-execution-state client))
      (message "")
      (message "Timer variables:")
      (message "  start-time: %s" jupyter-repl--execution-start-time)
      (message "  timer: %s" (if jupyter-repl--execution-timer "RUNNING" "nil"))
      (message "  completion-status: %s" jupyter-repl--last-completion-status)
      (message "  completion-timestamp: %s" jupyter-repl--completion-timestamp)
      (message "")
      (message "Modeline shows: %S" (jupyter-repl-interaction-mode-line))
      (message ""))))

(provide 'trace-completion)

;;; Usage:
;;; M-x trace-completion - Enable tracing
;;; Execute a cell
;;; M-x check-completion-state - Check state after completion
;;; M-x untrace-completion - Disable tracing
