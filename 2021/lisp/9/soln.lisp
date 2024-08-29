(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/9/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let* ((parts (cl-ppcre:split "" line))) 
              (setf parts (mapcar #'parse-integer parts))
              (vector-push-extend (coerce parts 'vector) inp))))
    (setf inp (coerce inp 'vector))
    inp))

(read-input t)

(defun is-low-point (inp y x)
    (let ((point (aref (aref inp y) x))
          (x-bound (length (aref inp 0)))
          (y-bound (length inp))
          (surrounding nil)
          (is-low nil))

        (when (>= (- x 1) 0) 
         (push (aref (aref inp y) (- x 1)) surrounding))

        (when (< (+ x 1) x-bound) 
         (push (aref (aref inp y) (+ x 1)) surrounding))

        (when (>= (- y 1) 0) 
         (push (aref (aref inp (- y 1)) x) surrounding))


        (when (< (+ y 1) y-bound)  
         (push (aref (aref inp (+ y 1)) x) surrounding))

      (when (every (lambda (x) (> x point)) surrounding)
          (setf is-low point))

      is-low))





(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test nil) (sum 0))
    (setf input (read-input is-test))
    (loop for y from 0 below (length input) do
          (loop for x from 0 below (length (aref input 0)) do
                (let ((lp (is-low-point input y x)))
                  (when lp 
                    (incf sum (+ lp 1))))))
    (format t "sum: ~a~%" sum)))


(solve-one)

