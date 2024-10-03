(ql:quickload "cl-ppcre")

(defun string-contains-p (substring string)
  (not (null (search substring string))))

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/15/"))
         (grid (if is-test 
                   (make-array '(10 10) :initial-element 0) 
                   (make-array '(100 100) :initial-element 0))))
    (with-open-file (stream path :direction :input)
        (loop for row from 0 
            for line = (read-line stream nil)
            while line do 
            (loop for col from 0 below (length line) 
                 for char = (char line col) do 
                 (setf (aref grid row col) (-(char-code char) (char-code #\0))))))
    grid))

;; Define the heap structure and operations
(defstruct heap
  (data (make-array 0 :adjustable t :fill-pointer 0))
  (compare-fn #'<))

(defun heap-insert (heap element)
  "Insert ELEMENT into the HEAP, maintaining the heap property."
  (vector-push-extend element (heap-data heap))
  (heapify-up heap (1- (fill-pointer (heap-data heap)))))

(defun heap-extract-min (heap)
  "Extract and return the minimum element from HEAP, reheapifying as necessary."
  (let ((data (heap-data heap)))
    (if (zerop (fill-pointer data))
        (error "Heap underflow")
        (let ((min (aref data 0))
              (last (aref data (1- (fill-pointer data)))))
          (setf (aref data 0) last)
          (decf (fill-pointer data))
          (heapify-down heap 0)
          min))))

(defun heapify-up (heap idx)
  "Restore heap property by moving the element at IDX up."
  (let ((data (heap-data heap))
        (compare-fn (heap-compare-fn heap)))
    (loop
      for parent = (floor (1- idx) 2)
      while (and (> idx 0)
                 ;; Compare the cost of the elements, i.e., (first (aref data idx)) and (first (aref data parent))
                 (funcall compare-fn (first (aref data idx)) (first (aref data parent))))
      do (rotatef (aref data idx) (aref data parent))
         (setf idx parent))))

(defun heapify-down (heap idx)
  "Restore heap property by moving the element at IDX down."
  (let* ((data (heap-data heap))
         (count (fill-pointer data))
         (compare-fn (heap-compare-fn heap)))
    (loop
      for left-child = (+ 1 (* 2 idx))
      for right-child = (+ 2 (* 2 idx))
      while (< left-child count)
      do (let* ((smallest (if (and (< right-child count)
                                   ;; Compare the cost of the children
                                   (funcall compare-fn (first (aref data right-child))
                                            (first (aref data left-child))))
                              right-child
                              left-child)))
           ;; Compare the cost of the current element with the smallest child
           (if (funcall compare-fn (first (aref data smallest)) (first (aref data idx)))
               (progn
                 (rotatef (aref data idx) (aref data smallest))
                 (setf idx smallest))
               (return))))))

;; Dijkstra's algorithm with priority queue

(defun dijkstra (grid)
  "Run Dijkstra's algorithm to find the shortest path from the top-left to bottom-right."
  (let* ((max-row (array-dimension grid 0))
         (max-col (array-dimension grid 1))
         (distances (make-array (list max-row max-col) :initial-element most-positive-fixnum))
         (heap (make-heap))
         (visited (make-hash-table)))

    ;; Set the starting position's cost to 0 and push it to the heap
    (setf (aref distances 0 0) 0)
    (heap-insert heap (list 0 0 0))  ;; Format: (cost row col)

    (loop
      while (> (fill-pointer (heap-data heap)) 0)
      for current = (heap-extract-min heap)
      for current-cost = (first current)
      for row = (second current)
      for col = (third current)

      ;; If we've reached the bottom-right corner, return the cost
      when (and (= row (1- max-row)) (= col (1- max-col)))
      return current-cost

      do
      (unless (gethash (list row col) visited)
        (setf (gethash (list row col) visited) t)

        ;; For each neighbor, update its cost if we find a shorter path
        (dolist (neighbor (neighbors row col max-row max-col))
          (let* ((nrow (first neighbor))
                 (ncol (second neighbor))
                 (new-cost (+ current-cost (aref grid nrow ncol))))
            (when (< new-cost (aref distances nrow ncol))
              (setf (aref distances nrow ncol) new-cost)
              (heap-insert heap (list new-cost nrow ncol)))))))))

(defun solve-one ()
  "Solve day 15 part one."
  (let ((grid (read-input nil)))  ;; Change 'nil' to 't' to use the test input
    (let ((result (dijkstra grid)))
      (format t "The shortest path cost is: ~a~%" result))))

;; To run the solution
(solve-one)
