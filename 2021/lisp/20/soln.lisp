(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/20/"))
         (code "")
         (image (if is-test (make-array '(5 5) :initial-element ".") (make-array '(100 100) :initial-element "."))))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line 
            for row from 0 do 
            (cond ((= row 0) (setf code line))
                  ((= row 1) (continue))
                  (t
                    (loop for char across line 
                        for col from 0 do 
                            (setf (aref image (- row 2) col) char)))))) (values code image)))


(defun print-image (image)
  (destructuring-bind (rows cols) (array-dimensions image)
    (loop for i from 0 below rows do
      (loop for j from 0 below cols do
        (format t "~a" (aref image i j)))
      (format t "~%"))))

(defun safe-aref (image row col) 
  (destructuring-bind (rows cols) (array-dimensions image)
    (if (and (>= row 0) (< row rows) 
             (>= col 0) (< col cols))
        (aref image row col)
      #\.)))

  

(defun mega-pixel-coords (row col) 
  (list
    (list (- row 1) (- col 1))
    (list (- row 1) col)
    (list (- row 1) (+ col 1))
    (list row (- col 1))
    (list row col)
    (list row (+ col 1))
    (list (+ row 1) (- col 1))
    (list (+ row 1) col)
    (list (+ row 1) (+ col 1))))

(mega-pixel-coords 5 10)

(defun build-binary-string (image row col) 
  (let ((surrounding (mega-pixel-coords row col))
        (binary-string ""))
    (loop for (r c) in surrounding do 
        (let ((point (safe-aref image r c)))
          (cond 
            ((char= point #\#) (setf binary-string (concatenate 'string binary-string "1")))
            ((char= point #\.) (setf binary-string (concatenate 'string binary-string "0"))))))
    binary-string))

(defun binary-string-to-decimal (binary-string) 
  (parse-integer binary-string :radix 2))

(defun solve-one () 
  "Solve day 20 part one" 
  (multiple-value-bind (code image) (read-input t)
    (format t "Image dimensions in solve-one: ~a~%" (array-dimensions image))
    (print-image image)
    (binary-string-to-decimal (build-binary-string image 2 2))))

(solve-one)
