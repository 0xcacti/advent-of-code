(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/04
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/04)


(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((mul-ops '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
                (let ((operation (parse-mul-operation line part)))
                     (when operation 
                       (loop for op in operation do 
                             (push op mul-ops))))))
        (reverse mul-ops))))

(defun solve-one ()
  "Solve day four, part one" 
  ) 

(solve-one)


(defun solve-two ()
  "Solve day four, part two"
  )

(solve-two)
