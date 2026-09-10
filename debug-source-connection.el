;;; debug-source-connection.el --- Debug source buffer connection -*- lexical-binding: t; -*-

(defun debug-source-connection ()
  "Debug why source buffer doesn't see REPL timer."
  (interactive)

  (message "")
  (message "=== Debugging Source Connection ===")
  (message "Current Buffer: %s" (buffer-name))
  (message "Mode: %s" major-mode)
  (message "")

  (unless (and (boundp 'jupyter-current-client) jupyter-current-client)
    (error "No client associated with this buffer!"))

  (let* ((client jupyter-current-client)
         (repl-buf (oref client buffer)))

    (message "Client Object: %s" client)
    (message "Client Class: %s" (eieio-object-class client))
    (message "Associated REPL Buffer: %s" repl-buf)
    (message "REPL Buffer Live? %s" (buffer-live-p repl-buf))
    (message "")

    (with-current-buffer repl-buf
      (message "INSIDE REPL BUFFER (%s):" (buffer-name))
      (message "  Client Object: %s" jupyter-current-client)
      (message "  Are clients identical? %s" (eq client jupyter-current-client))
      (message "  jupyter-repl--execution-start-time: %s"
               (if (boundp 'jupyter-repl--execution-start-time)
                   jupyter-repl--execution-start-time
                 "UNBOUND"))
      (message "  jupyter-repl-show-execution-time: %s"
               (if (boundp 'jupyter-repl-show-execution-time)
                   jupyter-repl-show-execution-time
                 "UNBOUND")))

    (message "")
    (message "Test calling mode-line function from here:")
    (let ((result (with-current-buffer repl-buf
                    (jupyter-repl-interaction-mode-line))))
      (message "  Result: %S" result))

    (message "")
    (message "=== End Debug ===")))

(provide 'debug-source-connection)
