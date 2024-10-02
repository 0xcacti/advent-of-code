(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/12/"))
         (caves (make-hash-table :test 'equal)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
              (let ((line-array (cl-ppcre:split "-" line)))
                (push (second line-array) (gethash (first line-array) caves '()))
                (push (first line-array) (gethash (second line-array) caves '())))))
    caves))

(read-input t)


(defun solve-one ())
