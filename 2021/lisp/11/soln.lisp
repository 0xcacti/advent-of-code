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

(increment-all (read-input t))

(defun run-flashes (grid flash-count has-flashed row col)
  "Run the flashes"
  (if (> (aref (aref grid row) col) 9))


(defun run-step (grid)
  "run a step"
  (setf grid (increment-all grid))




(defun solve-one ()
  "Solve part one day eleven"
  (let ((input (read-input nil)) 
        (sum 0))
    (loop for i from 0 below (length input) 
          for line = (aref input i)
          for result = (meow line)
          do 
          (format t "Input: ~a~%" line)
          when result 
          do (incf sum (case result 
                         (#\) 3)
                         (#\] 57)
                         (#\} 1197)
                         (#\> 25137)
                         (otherwise 0))))
    (format t "Sum: ~a~%" sum)))


