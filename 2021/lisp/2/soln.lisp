(ql:quickload "split-sequence")

(defparameter *inp* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/2/input.txt" :direction :input)
  (loop for line = (read-line stream nil)
        while line do 
        (vector-push-extend (split-sequence:split-sequence #\Space line) *inp*)))

(defun solv-one () 
  "solve day 2 part 1"
  (let ((x 0) (y 0))
    (loop for i from 0 below (length *inp*) do 
          (let ((direction (first (aref *inp* i))) (amount (parse-integer (second (aref *inp* i)))))
            (cond 
              ((string= direction "forward") (setf x (+ x amount)))
              ((string= direction "down") (setf y (+ y amount)))
              ((string= direction "up") (setf y (- y amount))))
            )
   ) (* x y)))

(solv-one)

(defun solv-two () 
  "solve day 2 part 1"
  (let ((x 0) (y 0) (aim 0))
    (loop for i from 0 below (length *inp*) do 
          (let ((direction (first (aref *inp* i))) (amount (parse-integer (second (aref *inp* i)))))
            (cond 
              ((string= direction "forward") 
                    (progn (setf x (+ x amount))
                     (setf y (+ y (* amount aim)))))
              ((string= direction "down") (setf aim (+ aim amount)))
              ((string= direction "up") (setf aim (- aim amount))))
            )
   ) (* x y)))

(solv-two)

