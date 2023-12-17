(in-package #:ip-management/interface)

(defgeneric process-close (process)
  (:documentation
   "Close proccess streams and wait it terminated"))

(defgeneric process-poll (process)
  (:documentation
   "Check if child process has terminated. Returns returncode attribute or nil."))

(defgeneric process-wait (process)
  (:documentation
   "Wait for child process to terminate. Returns returncode attribute."))

(defgeneric process-kill (process signum)
  (:documentation
   "Send a signal to the current process."))

(defgeneric get-process-output-string (process)
  (:documentation
   "Returns the full process output string."))

(defgeneric write-to-buffer (process string)
  (:documentation
   ("Write the string to the process buffer.")))

(defgeneric delete-process (process)
  (:documentation
   "Remove the process and the thread associated with it."))

(defgeneric process-alive-p (process)
  (:documentation
   "Check if the process is alive, returning t if it's the case, otherwise nil."))

(defgeneric process-send-input (process string)
  (:documentation
   "Sends a string to the current process input."))

;; Stream related generics
(defgeneric process-flush (process)
  (:documentation
   "Flush the process output."))

(defgeneric process-stream-open-p (process)
  (:documentation
   "Check if the process output is ready to be read."))

(defgeneric process-send-line (line process)
  (:documentation
   "Send a string to the process but with a line terminator."))

(defgeneric process-send-string(line process)
  (:documentation
   "Send a string to the process but with no line terminator."))

(defgeneric process-send-char (character process)
  (:documentation
   "Send a single character to the process."))

(defgeneric process-get-last-output (process)
  (:documentation
   "Get the last returning output from the process."))

(defgeneric process-read-line (process)
  (:documentation
   "Read a single line of the process output."))
