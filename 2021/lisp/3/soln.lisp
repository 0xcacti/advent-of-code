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


*inp*
(defun more-ones (inp col)
  "Count the number of ones in a column and tell you if there are more ones than zeros."
  (let ((o-count 0) (i-count 0))
    (loop for j from 0 to (1- (length inp)) do
      (if (string= (nth col (nth j inp)) "0")
          (incf o-count)
        (incf i-count)))
    (cond ((> o-count i-count) "0")
          ((> i-count o-count) "1")
          (t "2"))))



(defun build-oxygen () 
  "build oxygen value for day 3 part 2"
  (let ((oxygen "") (cur-inp (map 'list #'identity *inp*)))
    (loop while (/= (length cur-inp) 1) do
        (format t "cur-inp: ~a~%" cur-inp)
        (loop for i from 0 to (1- (length (aref *inp* 0))) do 
            (let ((col-value (more-ones cur-inp i)))
              (cond ((string= col-value "0") (setf cur-inp (remove-if-not (lambda (x) (string= (nth i x) "0")) cur-inp)))
                    ((string= col-value "1") (setf cur-inp (remove-if-not (lambda (x) (string= (nth i x) "1")) cur-inp)))
                    ((string= col-value "2") (setf cur-inp (remove-if-not (lambda (x) (string= (nth i x) "1")) cur-inp)))))
              (when (= (length cur-inp) 1)
              (return))))
    (setf oxygen (apply #'concatenate 'string (first cur-inp)))
    oxygen))


(defun build-co2 ()
  "build co2 value for day 3 part 2"
  (let ((co2 "") (cur-inp (map 'list #'identity *inp*)))
    (loop while (and cur-inp (/= (length cur-inp) 1)) do
        (loop for i from 0 to (1- (length (aref *inp* 0))) do 
            (format t "cur-inp: ~a~%" cur-inp)
            (let ((col-value (more-ones cur-inp i)))
              (cond ((string= col-value "0") (setf cur-inp (remove-if-not (lambda (x) (string= (nth i x) "1")) cur-inp)))
                    ((string= col-value "1") (setf cur-inp (remove-if-not (lambda (x) (string= (nth i x) "0")) cur-inp)))
                    ((string= col-value "2") (setf cur-inp (remove-if-not (lambda (x) (string= (nth i x) "0")) cur-inp)))))
              (when (= (length cur-inp) 1)
              (return))))
    (setf co2 (apply #'concatenate 'string (first cur-inp)))
    co2))


(defun solve-two () 
  "solve day 3 part 2"
  (* (parse-integer (build-oxygen) :radix 2) (parse-integer (build-co2) :radix 2)))

