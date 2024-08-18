(ql:quickload "split-sequence")

(defparameter *inp* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/3/test-input.txt" :direction :input)
  (loop for line = (read-line stream nil)
        while line do 
        (vector-push-extend line *inp*)))

(defun solv-one () 
  "solve day 3 part 1"
  (let ((gamma "") (epsilon ""))
    (loop for i from 0 below (length *inp*) do 
          (let ((line (aref *inp* i)))
            (
    )))

