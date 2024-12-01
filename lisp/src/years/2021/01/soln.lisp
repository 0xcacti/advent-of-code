(defparameter *inp* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/lisp/src/years/2021/01/input.txt" :direction :input)
  (loop for line = (read-line stream nil)
        while line do 
        (vector-push-extend (parse-integer line) *inp*)))

(defun increased (a b)
  (> a b))

(defun find-num-depth-changes () 
  "find the number of times the depths changes across inp"
  (let ((change-count 0))
    (loop for i from 1 below (length *inp*)
        do 
        (if (increased (aref *inp* i) (aref *inp* (- i 1)))
            (incf change-count)))
    change-count))

(find-num-depth-changes)

(defun find-num-depth-changes-window ()
  "find the number of times the depths changes across inp using a window"
  (let ((change-count 0)
        (i 0)
        (j 1))
    (loop while (< (+ j 2) (length *inp*))
        do 
        (if (increased (+ (aref *inp* j) (aref *inp* (+ j 1)) (aref *inp* (+ j 2)))
                       (+ (aref *inp* i) (aref *inp* (+ i 1)) (aref *inp* (+ i 2))))
            (incf change-count))
        (incf i)
        (incf j))
    change-count))


(find-num-depth-changes-window)
*inp*
