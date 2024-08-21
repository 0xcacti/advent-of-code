(ql:quickload "split-sequence")
(defparameter *inp-nums* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((line (read-line stream nil)))
    (when line
      (vector-push-extend (split-sequence:split-sequence #\, line) *inp*))))

(defparameter *inp-board-strings* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((line (read-line stream nil)))
    (when line
      (vector-push-extend (split-sequence:split-sequence #\, line) *inp*))))


*inp*

