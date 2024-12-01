(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2024/lisp/02/"))
         (left (make-array 0 :adjustable t :fill-pointer 0))
         (right (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((parts (cl-ppcre:split "   " line)))
              (vector-push-extend (parse-integer (first parts)) left)
              (vector-push-extend (parse-integer (second parts)) right))))
    (values left right)))

