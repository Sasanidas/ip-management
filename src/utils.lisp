
(in-package #:ipm/utils)

(defmacro with-process ((cmd process &key stdin stdout stderr union-stdout-stderr) &body body)
  `(let ((,process (make-process ,cmd
                                   :stdin ,stdin
                                   :stdout ,stdout
                                   :stderr ,stderr
                                   :union-stdout-stderr ,union-stdout-stderr)))
     (unwind-protect
          (progn ,@body)
       (process-close ,process))))

(defun open-process (cmd &key (buffer-size 4096))
  (with-process (cmd process :stdout t)
    (let ((*print-pretty* nil)
          (pout (process-output process))
          (buffer (make-array buffer-size :element-type 'character)))
      (with-output-to-string (out)
        (loop
          :for bytes-read = (read-sequence buffer pout)
          :do (write-sequence buffer out :start 0 :end bytes-read)
          :while (= bytes-read buffer-size))))))

(defmacro with-process2 ((cmd process stdin stdout) &body body)
  `(with-process (,cmd ,process :stdin t :stdout t)
     (let ((,stdin (process-input ,process))
           (,stdout (process-output ,process)))
       ,@body)))

(defmacro with-process3 ((cmd process stdin stdout stderr) &body body)
  `(with-process (,cmd ,process :stdin t :stdout t :stderr t)
     (let ((,stdin (process-input ,process))
           (,stdout (process-output ,process))
           (,stderr (process-error ,process)))
       ,@body)))

(defmacro with-process4 ((cmd process stdin stdout-and-stderr) &body body)
  `(with-process (,cmd ,process :stdin t :union-stdout-stderr t)
     (let ((,stdin (process-input ,process))
           (,stdout-and-stderr (process-output ,process)))
       ,@body)))
