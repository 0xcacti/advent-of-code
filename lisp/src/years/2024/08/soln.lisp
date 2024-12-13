(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/08
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/08)

(defun read-input (is-test)
  (let ((temp-grid (make-array 0 :adjustable t :fill-pointer 0)))
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line
              do (vector-push-extend 
                  (coerce (cl-ppcre:split "" line) 'vector)
                  temp-grid)))
    ;; Convert to 2D array
    (let* ((rows (length temp-grid))
           (cols (length (aref temp-grid 0)))
           (grid (make-array (list rows cols))))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (aref grid i j) (aref (aref temp-grid i) j))))
      grid))))

(read-input t)

(defun make-empty-grid (grid)
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1)))
    (make-array (list rows cols) :initial-element ".")))

(defun is-inline (pos1 pos2 pos3)
  (let ((x1 (first pos1))
        (y1 (second pos1))
        (x2 (first pos2))
        (y2 (second pos2))
        (x3 (first pos3))
        (y3 (second pos3)))
    (or 
      (and (equal x1 x2) (equal x2 x3))
      (and (equal y1 y2) (equal y2 y3)))
      (and (not (equal x1 x2))
           (not (equal x2 x3))
           (equal (* (- y2 y1) (- x3 x2))
                  (* (- y3 y2) (- x2 x1))))))


(defun find-antennas (grid)
  "Return a hashmap of antennas"
  (let ((nodes (make-hash-table :test 'equal)))
    (dotimes (i (array-dimension grid 0))
      (dotimes (j (array-dimension grid 1))
        (let* ((c (aref grid i j))
              (pos (list i j)))
          (when (not (equal c "."))
            (if (not (gethash c nodes))
              (setf (gethash c nodes) (list pos))
              (push pos (gethash c nodes)))))))
    nodes))

(defun print-antennas (nodes)
  (maphash (lambda (k v)
             (format t "Node: ~a, Pos: ~a~%" k v))
           nodes))

(defun double-distance (pos a b)
  (let ((x1 (first pos))
        (y1 (second pos))
        (x2 (first a))
        (y2 (second a))
        (x3 (first b))
        (y3 (second b)))
    ;; Calculate the two distances separately
    (let ((dist-to-a (+ (abs (- x1 x2))
                       (abs (- y1 y2))))
          (dist-to-b (+ (abs (- x1 x3))
                       (abs (- y1 y3)))))
      ;; Check if one distance is twice the other
      (or (= dist-to-a (* 2 dist-to-b))
          (= dist-to-b (* 2 dist-to-a))))))


(defun find-anti-nodes (pos nodes)
  (maphash (lambda (k v) 
    (loop for a in v do 
      (loop for b in v do 
        (when (not (equal a b))
                  (is-inline a b pos) 
                  (double-distance pos a b))  
          (when (and (is-inline a b pos)
                     (double-distance pos a b))
            (return-from find-anti-nodes t)))))) 
    nodes)
  nil)


(defun solve-one () 
  "Solve part one day 8" 
  (let* ((nodes (read-input nil))
         (antinode-count 0)
         (antennas (find-antennas nodes)))
    (print-antennas antennas)
    (loop for i from 0 to (array-dimension nodes 0) do 
      (loop for j from 0 to (array-dimension nodes 1) do 
        (let ((pos (list i j)))
          (when (find-anti-nodes pos antennas)
            (incf antinode-count)))))
    antinode-count))

(solve-one)
