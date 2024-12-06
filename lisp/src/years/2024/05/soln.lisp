(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/05
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/05)

(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((pages '())
          (updates '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (cond 
                ((cl-ppcre:scan "\\|" line)
                 (let ((parts (cl-ppcre:split "\\|" line)))
                   (push (mapcar #'parse-integer parts) pages)))
                ((cl-ppcre:scan "," line)
                 (let ((parts (cl-ppcre:split "," line)))
                   (push (mapcar #'parse-integer parts) updates))))))
    (values (reverse pages) (reverse updates)))))

(read-input t)
