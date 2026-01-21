;;; fix-state-tracking.el --- Fix execution state tracking -*- lexical-binding: t; -*-

;; The bug: jupyter--update-execution-state is called BEFORE jupyter-handle-status,
;; so both old-state and new-state are the same. We need to track the previous state ourselves.

(defvar-local jupyter-repl--previous-execution-state nil
  "Previous execution state for detecting transitions.")

;; Override the jupyter-handle-status method with proper state tracking
(cl-defmethod jupyter-handle-status ((client jupyter-repl-client) req msg)
  (let* ((old-state (or jupyter-repl--previous-execution-state
                        (jupyter-execution-state client)))
         (new-state (jupyter-with-message-content msg (execution_state)
                      execution_state)))

    ;; Handle state transitions for execution timer
    (cond
     ;; Transition to busy - start timer
     ((and (equal new-state "busy")
           (not (equal old-state "busy")))
      (jupyter-with-repl-buffer client
        (jupyter-repl--on-execution-start)))

     ;; Transition to idle - stop timer
     ((and (equal new-state "idle")
           (equal old-state "busy"))
      (jupyter-with-repl-buffer client
        (jupyter-repl--on-execution-complete t))))

    ;; Store current state for next time
    (jupyter-with-repl-buffer client
      (setq jupyter-repl--previous-execution-state new-state))

    ;; Original idle handling
    (when (equal new-state "idle")
      (jupyter-with-repl-buffer client
        (save-excursion
          (when (ignore-errors
                  (progn (jupyter-repl-goto-cell req) t))
            (jupyter-repl-cell-unmark-busy))
          ;; Update the cell count and reset the prompt
          (goto-char (point-max))
          (jupyter-repl-update-cell-count (oref client execution-count))))))

  (force-mode-line-update))

(defun apply-state-tracking-fix ()
  "Apply the state tracking fix."
  (interactive)

  (message "")
  (message "=== Applying State Tracking Fix ===")
  (message "")

  (message "[1/2] Redefining jupyter-handle-status method...")

  ;; The method is already redefined above when this file is loaded
  (message "  ✓ Method redefined with state tracking")

  (message "")
  (message "[2/2] Initializing state in existing REPL buffers...")

  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'jupyter-repl-mode)
          (setq-local jupyter-repl--previous-execution-state "idle")
          (setq count (1+ count)))))
    (message "  ✓ Initialized %d buffer(s)" count))

  (message "")
  (message "=== Fix Applied ===")
  (message "")
  (message "Now execute a cell - the timer should start and stop correctly!")
  (message ""))

;; Apply it automatically
(apply-state-tracking-fix)

(provide 'fix-state-tracking)
