(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/13
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/13)

(defun read-input (is-test)
    (with-input (path :test is-test)
      (let ((grid (make-array 0 :adjustable t :fill-pointer 0)))
      (with-open-file (stream path :direction :input)
            (loop for line = (read-line stream nil)
              for i from 0 
              while line do
              (let ((parts (cl-ppcre:split "" line)))
                (vector-push-extend parts grid))))
        grid)))

(read-input t)


