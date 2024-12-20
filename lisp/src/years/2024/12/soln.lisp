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

(defun hash-table-keys (ht)
  (loop for key being the hash-keys of ht collect key))

(defun perimeter (region) 
  (let* ((keys (hash-table-keys region))
         (perimeter 0))
    (format t "Computing perimeter for region with keys: ~A~%" keys)
    (loop for key in keys do  ; Changed from destructuring since we know they're lists
        (incf perimeter 4)
        (let ((r (first key))
              (c (second key)))
          (loop for (nr . nc) in (neighbors-4 r c) 
                when (gethash (list nr nc) region) do
                (decf perimeter))))
    perimeter))

(defun solve-one ()
  "Solve part one day 12" 
  (let* ((grid (read-input nil))
         (rows (length grid))
         (cols (length (aref grid 0)))
         (seen (make-hash-table :test 'equal))
         (regions '()))

    (loop for r from 0 below rows do
      (loop for c from 0 below cols do 

        (cond 
          ((gethash (list r c) seen)) 
          (t 
           (setf (gethash (list r c) seen) t)
           (let ((region (make-hash-table :test 'equal))
                 (q (make-queue)))
             (setf (gethash (list r c) region) t)
             (enqueue q (list r c))
             (loop while (not (queue-empty-p q)) do 
                (let* ((pos (dequeue q))
                       (cr (first pos))
                       (cc (second pos))
                       (crop (nth cc (aref grid cr)))
                       (neighbors (neighbors-4 cr cc)))
                  (loop for (nr . nc) in neighbors 
                        when (and (>= nr 0) (>= nc 0) (< nr rows) (< nc cols))
                        when (string= (nth nc (aref grid nr)) crop)
                        when (not (gethash (list nr nc) region))
                        do 
                        (setf (gethash (list nr nc) region) t)
                        (setf (gethash (list nr nc) seen) t)
                        (enqueue q (list nr nc)))))

             (push region regions))))))
    
    (reduce #'+ 
        (loop for region in regions collect 
              (* (length (hash-table-keys region)) (perimeter region))))))  

(solve-one)

(defun get-sides (region) 
  (let ((edges (make-hash-table :test 'equal)))
    (loop for key in (hash-table-keys region) do 
      (let ((r (first key))
            (c (second key)))
        (loop for (nr . nc) in (neighbors-4 r c) 
              when (not (gethash (list nr nc) region)) do
              (let ((er (nr (/ (+ nr r) 2)))
                    (ec (nc (/ (+ nc c) 2))))
              (setf (gethash (list nr nc) edges) 
                    (list (- er r) (-ec c)))))))
    (let ((seen (make-hash-table :test 'equal))
          (edge-count 0))
    (loop for edge being the hash-keys of hash-table
      using (hash-value direction)
      when (not (gethash edge seen)) do 
      (setf (gethash edge seen) t)
      (incf edge-count)
      (let ((edge-row (first edge))
            (edge-col (second edge)))
        (when (mod edge-row 1)
          (loop for dr in '(-1 1) do 
                (setf cr (+ edge-row dr))

          )
      



  )


(defun solve-two ()
  "Solve part two day 12"
  (let* ((grid (read-input t))
         (rows (length grid))
         (cols (length (aref grid 0)))
         (seen (make-hash-table :test 'equal))
         (regions '()))

    (loop for r from 0 below rows do
      (loop for c from 0 below cols do 

        (cond 
          ((gethash (list r c) seen)) 
          (t 
           (setf (gethash (list r c) seen) t)
           (let ((region (make-hash-table :test 'equal))
                 (q (make-queue)))
             (setf (gethash (list r c) region) t)
             (enqueue q (list r c))
             (loop while (not (queue-empty-p q)) do 
                (let* ((pos (dequeue q))
                       (cr (first pos))
                       (cc (second pos))
                       (crop (nth cc (aref grid cr)))
                       (neighbors (neighbors-4 cr cc)))
                  (loop for (nr . nc) in neighbors 
                        when (and (>= nr 0) (>= nc 0) (< nr rows) (< nc cols))
                        when (string= (nth nc (aref grid nr)) crop)
                        when (not (gethash (list nr nc) region))
                        do 
                        (setf (gethash (list nr nc) region) t)
                        (setf (gethash (list nr nc) seen) t)
                        (enqueue q (list nr nc)))))

             (push region regions))))))
    (reduce #'+ 
        (loop for region in regions collect 
              (* (length (hash-table-keys region)) (get-sides region))))))  
  )
