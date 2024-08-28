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

(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test t))
    (setf input (read-input is-test))
    (format t "input: ~a~%" input)))))

(solve-one)

