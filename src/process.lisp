(in-package #:ipm/impl)

(defclass process ()
  ((name
    :initarg :name
    :reader process-name)
   (command
    :initarg :command
    :reader process-command)
   (read-thread
    :initarg :read-thread
    :writer set-process-read-thread
    :reader process-read-thread)
   (buffer-stream
    :initarg :buffer-stream
    :reader process-buffer-stream)
   (output-callback
    :initarg :output-callback
    :reader process-output-callback)
   (callback-type
    :initarg :output-callback-type
    :reader process-output-callback-type)
   (pid
    :initarg :pid
    :reader process-pid)
   (stdin
    :initarg :stdin
    :initform nil
    :reader process-input)
   (stdout
    :initarg :stdout :initform nil
    :reader process-output)
   (stderr
    :initarg :stderr
    :initform nil
    :reader process-error)))

(defvar *shell* "sh")

(defvar *shell-full-path* "/bin/sh")


(defconstant +WNOHANG+ 1)

(defconstant +STDIN-FILENO+ 0)
(defconstant +STDOUT-FILENO+ 1)
(defconstant +STDERR-FILENO+ 2)

(defun make-process (cmd &key
                         stdin stdout stderr union-stdout-stderr
                         name output-callback output-callback-type
                         buffer-stream)
  (flet ((create-pipe (real)
           (if real
               (multiple-value-bind (p1 p2) (isys:pipe)
                 (vector p1 p2))))
         (pipe-rd (pipe)
           (aref pipe 0))
         (pipe-wr (pipe)
           (aref pipe 1)))
    (cffi:with-foreign-strings ((%arg0 *shell*)
                                (%arg1 "-c")
                                (%arg2 cmd))
      (cffi:with-foreign-object (%agrs :pointer 4)
        (setf (cffi:mem-aref %agrs :pointer 0) %arg0
              (cffi:mem-aref %agrs :pointer 1) %arg1
              (cffi:mem-aref %agrs :pointer 2) %arg2
              (cffi:mem-aref %agrs :pointer 3) (cffi:null-pointer))

        (let ((pin (create-pipe stdin))
              (pout (create-pipe (or stdout
                                     union-stdout-stderr)))
              (perr (create-pipe (and (not union-stdout-stderr)
                                      stderr)))
              (pid (isys:fork)))
          (case pid
            (0
             (when stdin
               (isys:close (pipe-wr pin))
               (isys:dup2 (pipe-rd pin) +STDIN-FILENO+)
               (isys:close (pipe-rd pin)))

             (cond
               (union-stdout-stderr
                (isys:close (pipe-rd pout))
                (isys:dup2 (pipe-wr pout) +STDOUT-FILENO+)
                (isys:dup2 (pipe-wr pout) +STDERR-FILENO+)
                (isys:close (pipe-wr pout)))

               (t (when stdout
                    (isys:close (pipe-rd pout))
                    (isys:dup2 (pipe-wr pout) +STDOUT-FILENO+)
                    (isys:close (pipe-wr pout)))
                  (when stderr
                    (isys:close (pipe-rd perr))
                    (isys:dup2 (pipe-wr perr) +STDERR-FILENO+)
                    (isys:close (pipe-wr perr)))))


             (isys:execv *shell-full-path* %agrs))
            (otherwise
             (when pin
               (isys:close (pipe-rd pin)))
             (when pout
               (isys:close (pipe-wr pout)))
             (when perr
               (isys:close (pipe-wr perr)))

             (make-instance 'process
                            :name name
                            :command cmd
                            :buffer-stream buffer-stream
                            :output-callback output-callback
                            :output-callback-type output-callback-type
                            :pid pid
                            :stdin (if pin
                                       (make-instance 'iolib.streams:dual-channel-gray-stream
                                                      :fd (pipe-wr pin)))
                            :stdout (if pout
                                        (make-instance 'iolib.streams:dual-channel-gray-stream
                                                       :fd (pipe-rd pout)))
                            :stderr (if perr
                                        (make-instance 'iolib.streams:dual-channel-gray-stream
                                                       :fd (pipe-rd perr)))))))))))
(defmethod process-kill ((process process) (signum Integer))
  (isys:kill (process-pid process) signum))

(defmethod process-close ((process process))
  (flet ((safe-close (stream)
           (when (and stream (iolib.streams:fd-of stream))
             (close stream :abort t))))
    (with-slots (pid stdin stdout stderr) process
      (safe-close stdin)
      (safe-close stdout)
      (safe-close stderr)
      (ignore-errors (isys:waitpid pid 0)))))

(defmethod process-avaiable-p ((process process))
  (and (open-stream-p (process-output process))
       (open-stream-p (process-input process))))

(defmethod process-flush ((process process))
  (finish-output (process-input process)))

(defmethod process-delete ((process process))
  (and (process-kill process 9)
       (process-close process)))

(defmethod process-stream-open-p ((process process))
  (process-flush process)
  (let ((possible-char (read-char-no-hang
                        (process-output process))))
    (when (characterp possible-char)
      (trivial-gray-streams:stream-unread-char
       (process-output process) possible-char)
      t)))

(defmethod process-send-line ((line String) (process process))
  (write-line line (process-input process)))

(defmethod process-send-char ((char character) (process process))
  (write-char char (process-input process)))

(defmethod process-get-last-output ((process process))
  (loop repeat 100
        do (process-flush process)
        if (process-stream-open-p process)
        return nil
        else do (sleep 0.001))

  (if (process-stream-open-p process)
      (loop with output = ""
            with poutput = (process-output process)
            for char = (read-char-no-hang poutput)
            if (null char)
            return output
            else
            do (setf output
                     (format nil "~a~a" output char)))
      ""))

(defmethod process-read-line ((process process))
  (read-line (process-output process)))

(defmethod process-alive-p ((process process))
  (and (ignore-errors
         (isys:kill (process-pid process) 0))
       t))
