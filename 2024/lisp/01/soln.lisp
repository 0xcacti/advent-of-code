(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2024/lisp/01/"))
         (left (make-array 0 :adjustable t :fill-pointer 0))
         (right (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((parts (cl-ppcre:split "   " line)))
              (vector-push-extend (parse-integer (first parts)) left)
              (vector-push-extend (parse-integer (second parts)) right))))
    (values left right)))



(defun part-one () 
  (multiple-value-bind (left right) (read-input nil) 
    (format t "left: ~a~%" left)
    (format t "right: ~a~%" right)
    (setf left (sort left #'<))
    (setf right (sort right #'<))
    (loop for i from 0 below (length left) 
          sum (abs (- (aref left i) (aref right i))))))

(part-one)
