(ql:quickload "cl-ppcre")

(defparameter *input* nil)

(defun read-input (is-test input)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/6/")))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((parts (mapcar #'parse-integer (cl-ppcre:split "," line))))
              (setf input (nconc input parts)))))) input)



(read-input t)
*input*

(defun map-day (input)
  (mapcar (lambda (x) (if (= x 0) (list 6 8) (- x 1))) input))




(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test t))
    (setf input (read-input is-test input))
    (format t "Input: ~a~%" input)
    (loop for i from 0 to 79 do 
          (setf input (map-day input)))
    (+ (mapcar #'length input))))


(solve-one)
