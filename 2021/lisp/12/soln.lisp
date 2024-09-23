(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/11/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0))
         (octopuses (make-hash-table :test 'equal)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((line-array (map 'vector 
                                    (lambda (c) (- (char-code c) (char-code #\0))) 
                                    line)))
              (vector-push-extend line-array inp))))
    (loop for r below (length inp) do 
          (loop for c below (length (aref inp 0)) do
                (setf (gethash (list r c) octopuses) (aref (aref inp r) c))))
    octopuses))

