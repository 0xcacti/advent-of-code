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
  (let ((is-test nil) (input nil) (signal-patterns nil) (output nil) (c 0))
    (setf input (read-input is-test))
    (format t "input: ~a~%" input)
    (setf signal-patterns (first input))
    (setf output (second input))
    (format t "signal-patterns: ~a~%" signal-patterns)
    (format t "output: ~a~%" output)
    (loop for out in output do 
          (loop for o in out do 
        (format t "out: ~a~%" out)
        (let ((l (length o)))
          (when (or (= l 2) (= l 3) (= l 4) (= l 7))
            (incf c)))))
    (format t "c: ~a~%" c)))

;; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab 

(solve-one)
