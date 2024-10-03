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

(defun solve-one () 
  "solve day 15 part one"
  (let ((grid (read-input t)))
    (format t "Grid: ~a~%" grid)))


(solve-one)
