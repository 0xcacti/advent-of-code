(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/19/")))
    (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
            while line do 
            (loop for col from 0 below (length line) 
                 for char = (char line col) do 
                 (setf (aref grid row col) (-(char-code char) (char-code #\0))))))
    grid))



