(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/16
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/16)

(defun read-input (is-test)
  (let ((grid (make-array 0 :adjustable t :fill-pointer 0)))
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line
              do 
              (vector-push-extend (coerce (cl-ppcre:split "" line) 'vector) grid))))
    grid))

(read-input t)

(defun print-grid (grid)
  (loop for row across grid do
        (format t "~{~a~}~%" row)))

(defstruct (heapq (:constructor create-heapq))
  (elements (make-array 10 :adjustable t :fill-pointer 0)))

(defun heapify-up (heap idx)
  "Maintain heap property upwards after insertion"
  (when (> idx 0)
    (let* ((parent-idx (floor (1- idx) 2))
           (parent-element (aref (heapq-elements heap) parent-idx))
           (current-element (aref (heapq-elements heap) idx)))
      (when (< (car current-element) (car parent-element))
        (rotatef (aref (heapq-elements heap) idx)
                 (aref (heapq-elements heap) parent-idx))
        (heapify-up heap parent-idx)))))

(defun heapify-down (heap idx)
  "Maintain heap property downwards after removal"
  (let* ((length (length (heapq-elements heap)))
         (left-idx (+ (* idx 2) 1))
         (right-idx (+ (* idx 2) 2))
         (smallest idx))
    (when (and (< left-idx length)
               (< (car (aref (heapq-elements heap) left-idx))
                  (car (aref (heapq-elements heap) smallest))))
      (setf smallest left-idx))
    (when (and (< right-idx length)
               (< (car (aref (heapq-elements heap) right-idx))
                  (car (aref (heapq-elements heap) smallest))))
      (setf smallest right-idx))
    (when (/= smallest idx)
      (rotatef (aref (heapq-elements heap) idx)
               (aref (heapq-elements heap) smallest))
      (heapify-down heap smallest))))

(defun heap-push (heap priority item)
  "Push an item with given priority onto heap"
  (vector-push-extend (cons priority item) (heapq-elements heap))
  (heapify-up heap (1- (length (heapq-elements heap))))
  heap)

(defun heap-pop (heap)
  "Remove and return item with lowest priority"
  (let ((elements (heapq-elements heap)))
    (when (= (length elements) 0)
      (error "Heap is empty"))
    (let ((min-item (aref elements 0)))
      (setf (aref elements 0) (aref elements (1- (length elements))))
      (vector-pop elements)
      (unless (= (length elements) 0)
        (heapify-down heap 0))
      (cdr min-item))))

(defun heap-empty-p (heap)
  "Check if heap is empty"
  (= (length (heapq-elements heap)) 0))

(defun find-start (grid)
  "Find the starting position marked with 'S' in the grid"
  (loop for r from 0 below (length grid)
        do (loop for c from 0 below (length (aref grid 0))
                 when (string= (aref (aref grid r) c) "S")
                 return (return-from find-start (list r c))))
  nil)


(defun solve-one ()
  "Solve part one of day 16"
  (let* ((grid (read-input nil))
         (rows (length grid))
         (cols (length (aref grid 0)))
         (start (find-start grid))
         (sr (first start))
         (sc (second start))
         (pq (create-heapq))
         (seen (make-hash-table :test #'equal))
         (path nil))  ; Track the path
    
    (format t "Grid size: ~Ax~A~%" rows cols)
    (format t "Start at: (~A,~A)~%" sr sc)
    
    ;; Initial state: (cost r c dr dc path)
    (heap-push pq 0 (list 0 sr sc 0 1 nil))
    (setf (gethash (list sr sc 0 1) seen) t)
    
    (loop while (not (heap-empty-p pq))
          do (let* ((current (heap-pop pq))
                    (cost (first current))
                    (r (second current))
                    (c (third current))
                    (dr (fourth current))
                    (dc (fifth current))
                    (current-path (sixth current)))
               
               (setf (gethash (list r c dr dc) seen) t)
               
               ;; Check if we reached the end
               (when (string= (aref (aref grid r) c) "E")
                 (format t "Found end with direction (~A,~A)~%" dr dc)
                 (format t "Path taken:~%")
                 (dolist (step (reverse (cons (list r c) current-path)))
                   (format t "  (~A,~A)~%" (first step) (second step)))
                 (return-from solve-one cost))
               
               (let ((moves (list 
                           (list (+ cost 1) (+ r dr) (+ c dc) dr dc 
                                 (cons (list r c) current-path))  ; straight
                           (list (+ cost 1000) r c dc (- dr)
                                 (cons (list r c) current-path))  ; clockwise
                           (list (+ cost 1000) r c (- dc) dr
                                 (cons (list r c) current-path)))))  ; counter
                 
                 (dolist (move moves)
                   (destructuring-bind (new-cost nr nc ndr ndc new-path) move
                     (when (and (< -1 nr rows)
                              (< -1 nc cols)
                              (not (string= (aref (aref grid nr) nc) "#"))
                              (not (gethash (list nr nc ndr ndc) seen)))
                       (heap-push pq new-cost 
                                (list new-cost nr nc ndr ndc new-path))))))))
    nil))


(solve-one)


(defun solve-two ()
  "Solve part two of day 16"
  (let* ((grid (read-input nil))
         (rows (length grid))
         (cols (length (aref grid 0)))
         (start (find-start grid))
         (sr (first start))
         (sc (second start))
         (pq (create-heapq))
         (lowest-cost (make-hash-table :test #'equal))
         (backtrack (make-hash-table :test #'equal))
         (best-cost most-positive-fixnum)
         (end-states (make-hash-table :test #'equal)))
    
    ;; Initialize starting state
    (heap-push pq 0 (list 0 sr sc 0 1))
    (setf (gethash (list sr sc 0 1) lowest-cost) 0)
    
    (loop while (not (heap-empty-p pq))
          do (let* ((current (heap-pop pq))
                    (cost (first current))
                    (r (second current))
                    (c (third current))
                    (dr (fourth current))
                    (dc (fifth current)))
               
               ;; Skip if we've found a better path to this state
               (when (> cost (gethash (list r c dr dc) lowest-cost most-positive-fixnum))
                 (continue))
               
               ;; Check if we reached the end
               (when (string= (aref (aref grid r) c) "E")
                 (when (> cost best-cost)
                   (return))
                 (setf best-cost cost)
                 (setf (gethash (list r c dr dc) end-states) t))
               
               ;; Generate next moves
               (let ((moves (list 
                           ;; Continue straight
                           (list (+ cost 1) (+ r dr) (+ c dc) dr dc)
                           ;; Turn clockwise
                           (list (+ cost 1000) r c dc (- dr))
                           ;; Turn counter-clockwise
                           (list (+ cost 1000) r c (- dc) dr))))
                 
                 (dolist (move moves)
                   (destructuring-bind (new-cost nr nc ndr ndc) move
                     ;; Skip walls and out of bounds
                     (when (and (< -1 nr rows)
                              (< -1 nc cols)
                              (not (string= (aref (aref grid nr) nc) "#")))
                       ;; Check if this is a better path
                       (let ((lowest (gethash (list nr nc ndr ndc) lowest-cost most-positive-fixnum)))
                         (when (<= new-cost lowest)
                           ;; If strictly better, clear previous backtrack entries
                           (when (< new-cost lowest)
                             (setf (gethash (list nr nc ndr ndc) backtrack) nil)
                             (setf (gethash (list nr nc ndr ndc) lowest-cost) new-cost))
                           ;; Add to backtrack map
                           (push (list r c dr dc) 
                                 (gethash (list nr nc ndr ndc) backtrack))
                           ;; Add to priority queue
                           (heap-push pq new-cost 
                                    (list new-cost nr nc ndr ndc))))))))))
    
    ;; Backtrack to find all tiles in optimal paths
    (let ((states (make-hash-table :test #'equal))
          (queue (make-array 0 :adjustable t :fill-pointer 0)))
      
      ;; Initialize with end states
      (loop for state being the hash-keys of end-states do
            (setf (gethash state states) t)
            (vector-push-extend state queue))
      
      ;; Process queue
      (loop while (> (length queue) 0)
            do (let ((current (vector-pop queue)))
                 (dolist (prev (gethash current backtrack))
                   (unless (gethash prev states)
                     (setf (gethash prev states) t)
                     (vector-push-extend prev queue)))))
      
      ;; Count unique (r,c) pairs
      (let ((unique-tiles (make-hash-table :test #'equal)))
        (loop for (r c . rest) being the hash-keys of states do
              (setf (gethash (list r c) unique-tiles) t))
        (hash-table-count unique-tiles)))))

(solve-two)
