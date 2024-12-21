(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/13
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/13)

(defstruct machine
  a-btn-x
  a-btn-y
  b-btn-x
  b-btn-y
  prize-x
  prize-y)

(defun parse-button-line (line)
  "Parse a button line like 'Button A: X+94, Y+34' into (x y) values"
  (let* ((parts (cl-ppcre:split "[,:]" line))
         (x-part (cl-ppcre:scan-to-strings "\\d+" (second parts)))
         (y-part (cl-ppcre:scan-to-strings "\\d+" (third parts))))
    (list (parse-integer x-part) 
          (parse-integer y-part))))

(defun parse-prize-line (line)
  "Parse a prize line like 'Prize: X=8400, Y=5400' into (x y) values"
  (let* ((parts (cl-ppcre:split "[,:]" line))
         (x-part (cl-ppcre:scan-to-strings "\\d+" (second parts)))
         (y-part (cl-ppcre:scan-to-strings "\\d+" (third parts))))
    (list (parse-integer x-part)
          (parse-integer y-part))))

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((machines '()))
      (with-open-file (stream path :direction :input)
        (loop
          for line1 = (read-line stream nil)
          for line2 = (read-line stream nil)
          for line3 = (read-line stream nil)
          while line1 do
          (let* ((a-btn (parse-button-line line1))
                 (b-btn (parse-button-line line2))
                 (prize (parse-prize-line line3))
                 (machine (make-machine 
                          :a-btn-x (first a-btn)
                          :a-btn-y (second a-btn)
                          :b-btn-x (first b-btn)
                          :b-btn-y (second b-btn)
                          :prize-x (first prize)
                          :prize-y (second prize))))
            (push machine machines)
            ;; Skip blank line if it exists
            (read-line stream nil)))
        (nreverse machines)))))

(defun solve-one ()
  "Solve part one day 13"
  (let ((machines (read-input t)))
    ;; Add solution logic here
    machines))

(solve-one)

