(ql:quickload "cl-ppcre")

(defun read-input (is-test input)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/6/")))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((parts (mapcar #'parse-integer (cl-ppcre:split "," line))))
              (setf input (nconc input parts)))))) input)

(defun map-day (input)
  (mapcar (lambda (x) 
            (cond 
             ((listp x) (map-day x))
             ((= x 0) (list 6 8))
             (t (- x 1)))) input))


(defun count-elements (lst)
  "recursively count the total number of elements in a list"
  (cond 
    ((null lst) 0)
    ((atom (car lst)) (+ 1 (count-elements (cdr lst))))
    (t (+ (count-elements (car lst)) (count-elements (cdr lst))))))


(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test nil))
    (setf input (read-input is-test input))
    (loop for i from 0 to 79 do 
          (format t "i: ~a~%" i)
          (setf input (map-day input)))
    (format t "sum: ~a~%" (count-elements input))))

(solve-one)

(defun populate-counter (input counter)
  (loop for i in input do 
        (incf (elt counter i))) counter)

(defun simulate-days (counter days)
  (dotimes (j days)
    (let ((new-fish (elt counter 0)))
      (loop for i from 0 below 8 do 
            (setf (elt counter i) (elt counter (+ i 1))))
      (incf (elt counter 6) new-fish)
      (setf (elt counter 8) new-fish)))
  counter)

(defun solve-two () 
  "Solve part two day six"
  (let ((input nil) (is-test nil) (counter (make-array '(9) :initial-element 0)))
    (setf input (read-input is-test input))
    (setf counter (populate-counter input counter))
    (setf counter (simulate-days counter 256))
    (format t "counter: ~a~%" (reduce #'+ counter))))

(solve-two)
