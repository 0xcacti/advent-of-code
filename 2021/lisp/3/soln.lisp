(ql:quickload "split-sequence")

(defparameter *inp* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/3/input.txt" :direction :input)
  (loop for line = (read-line stream nil)
        while line do 
        (vector-push-extend 
         (map 'list #'(lambda (char) (string char)) line)
         *inp*)))

*inp*

(defun solve-one () 
  "solve day 3 part 1"
  (let 
      ((gamma "") 
       (epsilon ""))
    (loop for i from 0 to (1- (length (aref *inp* 0))) do 
        (let ((o-count 0) (i-count 0))
        (loop for j from 0 to (1- (length *inp*)) do 
            (if (string= (nth i (aref *inp* j)) "0")
                (incf o-count)
                (incf i-count)))
          (if (> o-count i-count)
              (setf gamma (concatenate 'string gamma "0")
                    epsilon (concatenate 'string epsilon "1"))
              (setf gamma (concatenate 'string gamma "1")
                    epsilon (concatenate 'string epsilon "0")))))
    (let ((x (parse-integer gamma :radix 2)) (y (parse-integer epsilon :radix 2)))
      (return-from solve-one (* x y)))))

(solve-one)

(defun solv-two () 
  "solve day 3 part 2"
)
