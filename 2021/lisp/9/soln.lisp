(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/9/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let* ((parts (cl-ppcre:split "" line))) 
              (setf parts (mapcar #'parse-integer parts))
              (vector-push-extend (coerce parts 'vector) inp))))
    (setf inp (coerce inp 'vector))
    inp))

(read-input t)

(defun is-low-point (inp y x)
    (let ((point (aref (aref inp y) x))
          (x-bound (length (aref inp 0)))
          (y-bound (length inp))
          (surrounding nil)
          (is-low nil))

        (when (>= (- x 1) 0) 
         (push (aref (aref inp y) (- x 1)) surrounding))

        (when (< (+ x 1) x-bound) 
         (push (aref (aref inp y) (+ x 1)) surrounding))

        (when (>= (- y 1) 0) 
         (push (aref (aref inp (- y 1)) x) surrounding))


        (when (< (+ y 1) y-bound)  
         (push (aref (aref inp (+ y 1)) x) surrounding))

      (when (every (lambda (x) (> x point)) surrounding)
          (setf is-low point))

      is-low))





(defun solve-one ()
  "Solve part one day nine"
  (let ((input nil) (is-test nil) (sum 0))
    (setf input (read-input is-test))
    (loop for y from 0 below (length input) do
          (loop for x from 0 below (length (aref input 0)) do
                (let ((lp (is-low-point input y x)))
                  (when lp 
                    (incf sum (+ lp 1))))))
    (format t "sum: ~a~%" sum)))


(defun find-basin (inp pairs y x last-value &optional is-first) 

    (let ((x-bound (length (aref inp 0)))
          (y-bound (length inp)))


      (when (or (>= x x-bound) (>= y y-bound) (< x 0) (< y 0))
        (return-from find-basin pairs))

      (let ((point (aref (aref inp y) x)))

      (when (gethash (cons y x) pairs)
        (return-from find-basin pairs))

      (unless is-first 
          (when (or (<= point last-value) (= point 9))
            (return-from find-basin pairs)))



      (setf (gethash (cons y x) pairs) t)

      ;; this does the recursion 
      (find-basin inp pairs y (- x 1) point) 
      (find-basin inp pairs y (+ x 1) point) 
      (find-basin inp pairs (- y 1) x point)
      (find-basin inp pairs (+ y 1) x point)))
  pairs )

(defun check-basin ()
  (let ((input nil) (is-test t) (sum 0) (basin nil))
    (setf input (read-input is-test))
    (setf basin (find-basin input (make-hash-table :test 'equal) 4 6 5 t))
    (maphash (lambda (k v) (format t "~a~%" k)) basin)
    (format t "length: ~a~%" (hash-table-count basin))))

(check-basin)
    


(defun solve-two ()
  "Solve part one day nine"
  (let ((input nil) (is-test nil) (basins nil))
    (setf input (read-input is-test))
    (loop for y from 0 below (length input) do
          (loop for x from 0 below (length (aref input 0)) do
                (let ((lp (is-low-point input y x)))
                  (when lp 
                    (push (hash-table-count (find-basin input (make-hash-table :test 'equal) y x lp t)) basins)))))
    (setf basins (sort basins #'>))
    (format t "basins: ~a~%" basins)
    (let* ((largest-basins (subseq basins 0 (min 3 (length basins))))
      (product (reduce #'* largest-basins)))
      (format t "basins: ~a~%" largest-basins)
      (format t "product: ~a~%" product))))


(solve-two)


