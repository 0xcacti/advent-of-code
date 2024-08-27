(ql:quickload "cl-ppcre")
(ql:quickload "cl-ppcre")

(defun read-input (is-test input)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/7/")))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((parts (mapcar #'parse-integer (cl-ppcre:split "," line))))
              (setf input (nconc input parts)))))) input)

(defun find-fuel-cost (val input) 
  (reduce #'+ (mapcar (lambda (x) (abs (- val x))) input)))


(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test nil) (median 0) (center 0))
    (setf input (read-input is-test input))
    (setf center (truncate (/ (length input) 2)))
    (format t "center: ~a~%" center)
    (setf input (sort input #'<))
    (if (evenp (length input)) 
        (setf median (/ (+ (elt input center) (elt input (- center 1))) 2))
        (setf median (elt input (- center))))
    (format t "input: ~a~%" input)
    (format t "median: ~a~%" median)
    (format t "fuel cost: ~a~%" (find-fuel-cost median input))))


(solve-one)

(defun fuel-cost (distance)
  (/ (* distance (+ distance 1)) 2))

(defun total-fuel-cost (input position)
  (reduce #'+ (mapcar (lambda (p) (fuel-cost (abs (- p position)))) input)))


(defun solve-two ()
  "Solve part two day six"
  (let ((input nil) (is-test nil) (soln 0))
    (setf input (read-input is-test input))
    (setf input (sort input #'<))
    (setf mean (truncate (/ (reduce #'+ input) (length input))))
    (setf soln (total-fuel-cost input mean))

    (let ((l-mean (- mean 1)) (found-min nil))
      (loop while (not found-min) do
            (let ((new-cost (total-fuel-cost input l-mean)))
                  (if (< new-cost soln)
                      (progn
                       (setf soln new-cost)
                       (setf l-mean (- l-mean 1)))
                      (setf found-min t)))))

    (let ((r-mean (+ mean 1)) (found-min nil))
      (loop while (not found-min) do 
            (let ((new-cost (total-fuel-cost input r-mean)))
                  (if (< new-cost soln)
                      (progn
                       (setf soln new-cost)
                       (setf r-mean (+ r-mean 1)))
                      (setf found-min t)))))

    (format t "soln: ~a~%" soln)))


(solve-two)

(defun read-input (is-test input)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/7/")))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((parts (mapcar #'parse-integer (cl-ppcre:split "," line))))
              (setf input (nconc input parts)))))) input)

(defun find-fuel-cost (val input) 
  (reduce #'+ (mapcar (lambda (x) (abs (- val x))) input)))


(defun solve-one ()
  "Solve part one day six"
  (let ((input nil) (is-test nil) (median 0) (center 0))
    (setf input (read-input is-test input))
    (setf center (truncate (/ (length input) 2)))
    (format t "center: ~a~%" center)
    (setf input (sort input #'<))
    (if (evenp (length input)) 
        (setf median (/ (+ (elt input center) (elt input (- center 1))) 2))
        (setf median (elt input (- center))))
    (format t "input: ~a~%" input)
    (format t "median: ~a~%" median)
    (format t "fuel cost: ~a~%" (find-fuel-cost median input))))


(solve-one)

(defun fuel-cost (distance)
  (/ (* distance (+ distance 1)) 2))

(defun total-fuel-cost (input position)
  (reduce #'+ (mapcar (lambda (p) (fuel-cost (abs (- p position)))) input)))


(defun solve-two ()
  "Solve part two day six"
  (let ((input nil) (is-test nil) (soln 0))
    (setf input (read-input is-test input))
    (setf input (sort input #'<))
    (setf mean (truncate (/ (reduce #'+ input) (length input))))
    (setf soln (total-fuel-cost input mean))

    (let ((l-mean (- mean 1)) (found-min nil))
      (loop while (not found-min) do
            (let ((new-cost (total-fuel-cost input l-mean)))
                  (if (< new-cost soln)
                      (progn
                       (setf soln new-cost)
                       (setf l-mean (- l-mean 1)))
                      (setf found-min t)))))

    (let ((r-mean (+ mean 1)) (found-min nil))
      (loop while (not found-min) do 
            (let ((new-cost (total-fuel-cost input r-mean)))
                  (if (< new-cost soln)
                      (progn
                       (setf soln new-cost)
                       (setf r-mean (+ r-mean 1)))
                      (setf found-min t)))))

    (format t "soln: ~a~%" soln)))


(solve-two)
