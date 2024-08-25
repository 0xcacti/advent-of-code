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
    (loop for i from 0 to 255 do 
          (format t "i: ~a~%" i)
          (setf input (map-day input)))
    (format t "sum: ~a~%" (count-elements input))))

(solve-one)
