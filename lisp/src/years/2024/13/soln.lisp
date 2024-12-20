(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/13
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/13)

(defun read-input (is-test)
    (with-input (path :test is-test)
      (let ((a-btns '())
            (b-btns '())
            (destinations '())))
      (with-open-file (stream path :direction :input)
            (loop for line = (read-line stream nil)
              for i from 0 
              while line do
              (cond 
              (setf str-puz (concatenate 'string str-puz line))))
        (format t "Read input: ~A~%" str-puz))))



(read-input t)

(defun solve-one ()
  "Solve part one day 13"

  )


