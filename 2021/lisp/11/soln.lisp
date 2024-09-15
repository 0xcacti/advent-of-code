(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/11/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((line-array (map 'vector #'digit-char-p line)))
              (vector-push-extend line-array inp))))
    inp))

(read-input t)

(defun increment-all (grid)
  "increment all elements of a grid"
  (loop for i from 0 below (length grid)
        do (loop for j from 0 below (length (aref grid i))
                 do (incf (aref (aref grid i) j))))
  grid)

(defun run-flashes (grid flash-count has-flashed row col)
  "Run the flashes"
  (when (and (>= row 0) (< row (length grid))
             (>= col 0) (< col (length (aref grid 0)))
             (not (aref has-flashed row col)))
    (when (> (aref (aref grid row) col) 9)
      (setf (aref has-flashed row col) t)
      (incf flash-count)
      (setf (aref (aref grid row) col) 0)
      (loop for dx from -1 to 1 do
        (loop for dy from -1 to 1 do
          (let ((new-row (+ row dx))
                (new-col (+ col dy)))
            (when (and (>= new-row 0) (< new-row (length grid))
                       (>= new-col 0) (< new-col (length (aref grid 0)))
                       (not (aref has-flashed new-row new-col)))
              (incf (aref (aref grid new-row) new-col))
              (setf (values grid flash-count)
                    (run-flashes grid flash-count has-flashed new-row new-col)))))))
    (values grid flash-count)))

(defun run-step (grid)
  "Run a step"
  (let* ((rows (length grid))
         (cols (length (aref grid 0)))
         (has-flashed (make-array (list rows cols) :initial-element nil))
         (flash-count 0))
    (setf grid (increment-all grid))
    (loop for row from 0 below rows do
      (loop for col from 0 below cols do
        (setf (values grid flash-count)
              (run-flashes grid flash-count has-flashed row col))))
    (values grid flash-count)))

(defun solve-one ()
  "Solve part one of day eleven"
  (let* ((input (read-input t))
         (total-flashes 0))
    (loop repeat 100 do
      (multiple-value-bind (new-grid step-flashes)
          (run-step input)
        (setf input new-grid)
        (incf total-flashes step-flashes)))
    (format t "Total flashes after 100 steps: ~a~%" total-flashes)))

(solve-one)
