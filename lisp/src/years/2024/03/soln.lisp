(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/03
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/03)


(defun parse-mul-operation (str part)
  (let ((result '()))
    (cond 
      ((= part 1)
       (cl-ppcre:do-register-groups ((#'parse-integer x y))
           ("mul\\(([1-9]\\d{0,2}),([1-9]\\d{0,2})\\)" str)
         (push (list x y) result)))
      ((= part 2)
       ;; Find all operations in order
       (cl-ppcre:do-matches-as-strings (match "don't\\(\\)|do\\(\\)|mul\\([1-9]\\d{0,2},[1-9]\\d{0,2}\\)" str)
         (format t "Found match: ~a~%" match)  ; Debug print
         (cond
           ((equal match "don't()")
            (format t "Found don't~%")
            (push :dont result))
           ((equal match "do()")
            (format t "Found do~%")
            (push :do result))
           (t  ; must be a multiplication
            ;; Extract numbers from mul match
            (cl-ppcre:register-groups-bind ((#'parse-integer x y))
                ("mul\\(([1-9]\\d{0,2}),([1-9]\\d{0,2})\\)" match)
              (format t "Found mul: ~a,~a~%" x y)
              (push (list x y) result)))))))
    (format t "Final result for line: ~a~%" (reverse result))
    (reverse result)))


(defun read-input (is-test part)
  (with-input (path :test is-test)  
    (let ((mul-ops '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (format t "line: ~a~%" line)
                (let ((operation (parse-mul-operation line part)))
                     (when operation 
                       (loop for op in operation do 
                             (push op mul-ops))))))
        (reverse mul-ops))))

(defun solve-one ()
  "Solve day three, part one" 
  (let ((mul-ops (read-input t 1))
        (result 0))
    (loop for op in mul-ops do
          (incf result (* (first op) (second op))))
    result))


(solve-one)


(defun solve-two ()
  "Solve day three, part two"
  (let ((mul-ops (read-input nil 2))
        (result 0)
        (enabled t))  ; multiplications start enabled
    (format t "~%All ops: ~a~%" mul-ops)
    (loop for op in mul-ops do
          (format t "~%Current op: ~a, enabled: ~a~%" op enabled)
          (cond
            ((eq op :dont)
             (setf enabled nil))
            ((eq op :do)
             (setf enabled t))
            (t  ; it's a multiplication
             (when enabled
               (incf result (* (first op) (second op)))))))
    result))

(solve-two)
