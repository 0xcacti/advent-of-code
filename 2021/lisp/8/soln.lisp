(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/8/"))
         (signal-patterns nil)
         (output nil))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let* ((parts (cl-ppcre:split " \\| " line))) 
              (push (cl-ppcre:split " " (first parts)) signal-patterns)
              (push (cl-ppcre:split " " (second parts)) output))))
      (list (reverse signal-patterns) (reverse output))))



(defun solve-one () 
  "Solve part one day eight"
  (let ((is-test t) (input nil) (signal-patterns nil) (output nil))
    (setf input (read-input is-test))
    (format t "input: ~a~%" input)
    (setf signal-patterns (first (first input)))
    (setf output (first (second input)))
    (format t "signal-patterns: ~a~%" signal-patterns)
    (format t "output: ~a~%" output)))

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf

(solve-one)
