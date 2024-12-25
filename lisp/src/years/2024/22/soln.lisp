(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/22
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/22)


(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((lines '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line do
              (push (parse-integer line) lines)))
      (reverse lines))))

(read-input t)

(defun step (num)
  (setq num (mod (logxor num (* num 64)) 16777216))
  (setq num (mod (logxor num (floor num 32)) 16777216))
  (setq num (mod (logxor num (* num 2048)) 16777216))
  num)

(defun solve-one () 
  "Solve part one day 22"
  (let ((lines (read-input t)))
    (loop for num in lines 
          with buyer = (mod line 10)
          do 
          (loop for i from 0 below 2000 do 
            (setf num (step num))
    
(solve-one)
