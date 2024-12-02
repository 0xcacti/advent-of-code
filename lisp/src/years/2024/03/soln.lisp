(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/03
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/03)

(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((reports (make-array 0 :adjustable t :fill-pointer 0)))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (let ((parts (cl-ppcre:split " " line)))
                (let ((report '()))
                  (loop for part in parts do 
                        (push (parse-integer part) report))
                  (vector-push-extend (reverse report) reports))))
        reports))))


