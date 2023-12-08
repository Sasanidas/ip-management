

;; (defun run-process ((command String) &key name output-callback output-callback-type directory)
;;   (declare (ignorable directory))
;;   (let* ((buffer-stream (make-string-output-stream))
;;          (process
;;            (make-process command
;;                          :name name
;;                          :buffer-stream buffer-stream
;;                          :output-callback output-callback
;;                          :output-callback-type output-callback-type))
;;          (thread (bt:make-thread
;;                   (lambda ()
;;                     (loop
;;                       (unless (process-alive-p process)
;;                         (return))
;;                       (alexandria:when-let ((string
;;                                              (process-get-last-output process)))
;;                         (send-event (lambda ()
;;                                       (write-to-simple-buffer process string))))))
;;                   :name (format nil "run-process ~A }" command))))
;;     (set-process-read-thread thread process)
;;     process))
