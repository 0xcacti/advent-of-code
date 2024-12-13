(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/09
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/09)

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

(defun make-storage-map (elements)
  (let ((storage (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for element across elements 
          for i from 0 do 
          (cond ((evenp i) 
                 (loop for j from 0 below (parse-integer element) do 
                  (vector-push-extend (princ-to-string (/ i 2)) storage)))
                ((oddp i)
                  (loop for j from 0 below (parse-integer element) do
                    (vector-push-extend "." storage)))))
    storage))


(defun swap-elements (storage p1 p2)
  (let ((temp (aref storage p1)))
    (setf (aref storage p1) (aref storage p2))
    (setf (aref storage p2) temp))
  storage)

(defun remap (storage) 
  (let ((p1 0)
        (p2 (1- (length storage))))
    (loop while (< p1 p2) do
        (loop while (not (equal (aref storage p1) ".")) do 
          (incf p1))
        (loop while (equal (aref storage p2) ".") do 
          (decf p2))
        (when (< p1 p2)
          (setf storage (swap-elements storage p1 p2))))
  storage))


(defun count-up (storage)
  (let ((count 0)
        (counter 0))
    (loop for element across storage do 
          (when (not (equal element "."))
            (incf count (* (parse-integer element) counter)))
          (incf counter))
    count))

(defun solve-one () 
  "Solve part one day 9"
  (let* ((elements (read-input t))
         (storage (make-storage-map elements))
         (remapped (remap storage))
         (result (count-up remapped)))
    (format t "Storage: ~a~%" storage)
    (format t "Result: ~a~%" result)))

(solve-one)


(defun find-next-block-lens (storage p1 p2)
  (let* ((p3 p1)
        (p4 p2)
        (c (aref storage p4)))
    (loop while (equal (aref storage p3) ".") do 
      (incf p3))
    (loop while (and 
                  (not (equal (aref storage p4) ".")) 
                  (equal (aref storage p4) c)) do 
      (decf p4))
    (values (- p3 p1) (- p2 p4))))

(defun block-remap (storage)
  (let ((p1 0)
        (p2 (1- (length storage)))
        (counter  0))
    (format t "entering block remap~%")
    (loop while (and (< p1 p2) (< counter 8)) do 
        (loop while (not (equal (aref storage p1) ".")) do 
          (incf p1))
        (loop while (equal (aref storage p2) ".") do 
          (decf p2))
        (format t "p1: ~a p2: ~a~%" p1 p2)
        (multiple-value-bind (len1 len2) (find-next-block-lens storage p1 p2)

          (format t "len1: ~a len2: ~a~%" len1 len2)
          (if (<= len2 len1)
            (progn 
            (loop for i from 0 below len2 do 
                (setf storage (swap-elements storage (+ p1 i) (- p2 i))))
            (setf p1 (+ p1 len1))
            (setf p2 (- p2 len2)))
            (progn
              (setf p2 (- p2 len2)))))
        (incf counter)
        (setf p1 0))
    storage))


00...111...2...333.44.5555.6666.777.888899

(defun solve-two () 
  "Solve part two day 9"
  (let* ((elements (read-input t))
         (storage (make-storage-map elements))
         (remapped (block-remap storage)))
    (format t "Storage: ~a~%" storage)
    (format t "Result: ~a~%" remapped)))

(solve-two)

0099.111777244....333.5555.6666.....8888..
00992111777.44.333....5555.6666.....8888..

