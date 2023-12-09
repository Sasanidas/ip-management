
(defpackage #:ip-management/interface
  (:use #:cl)
  (:nicknames #:imp/interface)
  (:export #:run-process
           #:process-close
           #:process-poll
           #:process-wait
           #:process-kill
           #:get-process-output-string
           #:write-to-buffer
           #:delete-process
           #:process-alive-p
           #:process-send-input
           #:process-flush
           #:process-stream-open-p
           #:process-send-line
           #:process-send-char
           #:process-get-last-output
           #:process-read-line))

(defpackage #:ip-management/impl
  (:use #:cl #:ip-management/interface)
  (:nicknames #:ipm/impl)
  (:export #:process-alive-p
           #:delete-process
           #:process-kill
	   #:process-buffer-stream
           #:process-close
           #:process-flush
           #:process-stream-open-p
           #:process-send-line
           #:process-send-char
           #:process-get-last-output
           #:process-read-line
           #:make-process

           #:process-name
           #:process-command
           #:process-read-thread
           #:set-process-read-thread
           #:process-output-callback
           #:process-output-callback-type
           #:process-pid
           #:process-input
           #:process-output
           #:process-error))

(defpackage #:ip-management/utils
  (:use #:cl :ip-management/impl)
  (:nicknames #:ipm/utils)
  (:export #:create-process
           #:open-process
           #:with-process
           #:with-process2
           #:with-process3
           #:with-process4))
