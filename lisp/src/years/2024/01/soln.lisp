(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/01
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/01)

(defun read-input (is-test)
  (with-input (path :test is-test)  ; this replaces your path construction
    (let ((left (make-array 0 :adjustable t :fill-pointer 0))
          (right (make-array 0 :adjustable t :fill-pointer 0)))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (let ((parts (cl-ppcre:split "   " line)))
                (vector-push-extend (parse-integer (first parts)) left)
                (vector-push-extend (parse-integer (second parts)) right))))
      (values left right))))


(defun test-input ()
        (format t "Testing normal input:~%")
        (with-input (path :test nil)
              (format t "Path: ~A~%" path))
        
        (format t "~%Testing test input:~%")
        (with-input (path :test t)
              (format t "Path: ~A~%" path)))

(test-input)

(defun part-one () 
  "Solve day one, part one"
  (multiple-value-bind (left right) (read-input t) 
    (setf left (sort left #'<))
    (setf right (sort right #'<))
    (loop for i from 0 below (length left) 
          sum (abs (- (aref left i) (aref right i))))))

(part-one)

(defun part-two () 
  "Solve day one, part two"
  (multiple-value-bind (left right) (read-input nil) 
    (format t "left: ~a~%" left)
    (format t "right: ~a~%" right)
    (let ((counts (make-hash-table :test #'equal)))
      (loop for i from 0 below (length right) do 
            (format t "elem: ~a~%" (aref right i))
            (incf (gethash (aref right i) counts 0)))
      (loop for i from 0 below (length left) sum
            (if (gethash (aref left i) counts) (* (aref left i) (gethash (aref left i) counts)) 0)))))

(part-two)
