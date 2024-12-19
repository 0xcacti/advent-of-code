(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/12
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/12)

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


(defun solve-one ()
  "Solve part one day 12" 
  (let* ((grid (read-input t))
         (rows (length grid))
         (cols (length (aref grid 0)))
         (seen (make-hash-table :test 'equal))
         (regions '())
         (q (make-queue)))

    (loop for r from 0 below rows do
      (loop for c from 0 below cols do 
        (cond 
          ((gethash (list r c) seen)) 
          (t 
           (setf (gethash (list r c) seen) t)
           (let ((region (make-hash-table :test 'equal)))

           )

        (if (gethash (list r c) seen)
            (continue)

    (format t "rows: ~a, cols: ~a~%" rows cols)))

(solve-one)
