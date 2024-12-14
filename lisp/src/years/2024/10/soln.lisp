(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/10
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/10)

(defun read-input (is-test)
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (let* ((first-line (read-line stream nil))
               (width (length first-line))
               (height 1))
          (loop while (read-line stream nil) 
                do (incf height))
          (file-position stream 0)
          (let ((grid (make-array (list height width))))
            (loop for r below height 
                  for line = (read-line stream)
                  do (loop for c below width 
                           for char = (char line c)
                           do (setf (aref grid r c) (digit-char-p char))))
            grid)))))

(read-input t)


(defun routes-to-9-unique (grid r c visited reachable-nines)
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (chary (aref grid r c)))
    (when (gethash (cons r c) visited)
      (return-from routes-to-9 0))
    (when (= chary 9)
      (setf (gethash (cons r c) reachable-nines) t)
      (return-from routes-to-9 1))

    (setf (gethash (cons r c) visited) t)
    (let ((neighbors (neighbors-4 r c)))
      (loop for (nr . nc) in neighbors 
            when 
              (and (>= nr 0) 
                   (< nr rows) 
                   (>= nc 0) 
                   (< nc cols)
                   (not (gethash (cons nr nc) visited))
                   (= (aref grid nr nc) (1+ chary)))
            do
            (routes-to-9 grid nr nc visited reachable-nines)))
    (remhash (cons r c) visited)
    (hash-table-count reachable-nines)))

(defun find-trails (grid) 
  (let ((trail-count 0))
  (loop for r below (array-dimension grid 0) do 
    (loop for c below (array-dimension grid 1) do 
        (when (= (aref grid r c) 0)
          (incf trail-count (routes-to-9-unique grid r c (make-hash-table :test #'equal) (make-hash-table :test #'equal))))))
  trail-count))

(defun solve-one ()
  "Solve part one day 10"
  (let ((grid (read-input nil)))
    (format t "Grid: ~a~%" grid)
    (find-trails grid)))

(solve-one)


(defun routes-to-9 (grid r c visited)
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1))
        (chary (aref grid r c))
        (trail-count 0))
    (when (gethash (cons r c) visited)
      (return-from routes-to-9 0))
    (when (= chary 9)
      (return-from routes-to-9 1))

    (setf (gethash (cons r c) visited) t)
    (let ((neighbors (neighbors-4 r c)))
      (loop for (nr . nc) in neighbors 
            when 
              (and (>= nr 0) 
                   (< nr rows) 
                   (>= nc 0) 
                   (< nc cols)
                   (not (gethash (cons nr nc) visited))
                   (= (aref grid nr nc) (1+ chary)))
            do
            (incf trail-count (routes-to-9 grid nr nc visited))))
    (remhash (cons r c) visited)
    trail-count))

(defun find-trails-two (grid)
  (let ((trail-count 0))
  (loop for r below (array-dimension grid 0) do 
    (loop for c below (array-dimension grid 1) do 
        (when (= (aref grid r c) 0)
          (incf trail-count (routes-to-9 grid r c (make-hash-table :test #'equal))))))
  trail-count))

(defun solve-two ()
  "Solve part two day 10"
  (let ((grid (read-input nil)))
    (format t "Grid: ~a~%" grid)
    (find-trails-two grid)))

(solve-two)
