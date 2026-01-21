;;; debug-busy-handler.el --- Debug why busy handler doesn't work -*- lexical-binding: t; -*-

(defun debug-busy-handler ()
  "Debug why the busy handler doesn't set start-time."
  (interactive)

  (unless (and (boundp 'jupyter-current-client) jupyter-current-client)
    (error "No jupyter client"))

  (let* ((client jupyter-current-client)
         (repl-buf (oref client buffer)))

    (message "")
    (message "=== Debugging Busy Handler ===")
    (message "")

    ;; Check if the function exists
    (message "Checking jupyter-repl--on-execution-start:")
    (if (fboundp 'jupyter-repl--on-execution-start)
        (message "  ✓ Function exists")
      (message "  ✗ Function NOT DEFINED!"))

    ;; Check current state
    (message "")
    (message "Current execution state: %s" (jupyter-execution-state client))

    ;; Try calling the function directly
    (message "")
    (message "Calling jupyter-repl--on-execution-start directly in REPL buffer...")

    (with-current-buffer repl-buf
      (message "  Before: start-time = %s" jupyter-repl--execution-start-time)

      (condition-case err
          (progn
            (jupyter-repl--on-execution-start)
            (message "  ✓ Function called successfully"))
        (error (message "  ✗ Error: %s" err)))

      (message "  After: start-time = %s" jupyter-repl--execution-start-time)
      (message "  After: timer = %s" (if jupyter-repl--execution-timer "RUNNING" "nil")))

    ;; Now test the macro
    (message "")
    (message "Testing jupyter-with-repl-buffer macro:")
    (jupyter-with-repl-buffer client
      (message "  Inside macro, buffer = %s" (buffer-name))
      (message "  Calling on-execution-start...")
      (jupyter-repl--on-execution-start)
      (message "  start-time now = %s" jupyter-repl--execution-start-time))

    ;; Check final state
    (message "")
    (with-current-buffer repl-buf
      (message "Final state in REPL buffer:")
      (message "  start-time: %s" jupyter-repl--execution-start-time)
      (message "  timer: %s" (if jupyter-repl--execution-timer "RUNNING" "nil")))

    (message "")
    (message "If start-time is still nil, jupyter-repl--on-execution-start has a bug.")
    (message "")))

(defun check-on-execution-start-definition ()
  "Check what jupyter-repl--on-execution-start actually does."
  (interactive)

  (message "")
  (message "=== Checking on-execution-start Definition ===")
  (message "")

  (if (not (fboundp 'jupyter-repl--on-execution-start))
      (message "✗ Function not defined!")

    (message "Function is defined. Let me trace what it does...")

    ;; Add tracing
    (defun trace-on-execution-start (orig-fun)
      (message "[TRACE-START] Called")
      (message "[TRACE-START] Buffer: %s" (buffer-name))
      (message "[TRACE-START] Before: start-time = %s"
               (if (boundp 'jupyter-repl--execution-start-time)
                   jupyter-repl--execution-start-time
                 "UNBOUND"))
      (let ((result (funcall orig-fun)))
        (message "[TRACE-START] After: start-time = %s"
                 (if (boundp 'jupyter-repl--execution-start-time)
                     jupyter-repl--execution-start-time
                   "UNBOUND"))
        (message "[TRACE-START] After: timer = %s"
                 (if (boundp 'jupyter-repl--execution-timer)
                     jupyter-repl--execution-timer
                   "UNBOUND"))
        result))

    (advice-add 'jupyter-repl--on-execution-start :around #'trace-on-execution-start)

    (message "✓ Added tracing to jupyter-repl--on-execution-start")
    (message "Now run: M-x debug-busy-handler")
    (message "")))

(provide 'debug-busy-handler)

;;; Usage:
;;; M-x check-on-execution-start-definition - Add tracing
;;; M-x debug-busy-handler                  - Test the function
