(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/9/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let* ((parts (cl-ppcre:split "" line))) 
              (vector-push-extend parts inp))))
    inp))

(read-input t)

(defun is-low-point (inp x y)
    (let ((point (aref inp x y))
          (x-bound (array-dimension inp 0))
          (y-bound (length inp))
          (surrounding nil))
      (cond 
        ((>= (- x 1) 0) (push (aref inp (- x 1) y) surrounding))
        ((< (- y 1) 0) (push (aref inp x (- y 1)) supprounding))
        ((< (+ x 1) x-bound)  (push (aref inp (+ x 1) y) surrounding))
        ((< (+ y 1) y-bound) ((push (aref inp x (+ y 1)) surrounding))))
      (every #'> point surrounding)))


(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test t))
    (setf input (read-input is-test))
    (format t "input: ~a~%" input)))))

(solve-one)

