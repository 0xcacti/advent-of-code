(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/10
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/10)

(defun read-input (is-test)
  (let ((elements (make-array 0 :adjustable t :fill-pointer 0)))
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line
              do 
              (loop for element in (cl-ppcre:split "" line)
                    do (vector-push-extend element elements)))))
    elements))

(read-input t)


