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
                            (setf (aref image (- row 2) col) (string char))))))) (values code image)))


(defun print-image (image)
  (destructuring-bind (rows cols) (array-dimensions image)
    (loop for i from 0 below rows do
      (loop for j from 0 below cols do
        (format t "~a" (aref image i j)))
      (format t "~%"))))

(defun safe-aref (image row col default-pixel)
  (destructuring-bind (rows cols) (array-dimensions image)
    (if (and (>= row 0) (< row rows)
             (>= col 0) (< col cols))
        (aref image row col)
        default-pixel)))  ; Use provided default pixel

  

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
(mega-pixel-coords 0 0)

(defun build-binary-string (image row col default-pixel)
  (let ((surrounding (mega-pixel-coords row col))
        (binary-string ""))
    (loop for (r c) in surrounding do 
          (let ((point (safe-aref image r c default-pixel)))
            (cond 
              ((string= point "#") (setf binary-string (concatenate 'string binary-string "1")))
              ((string= point ".") (setf binary-string (concatenate 'string binary-string "0"))))))
    binary-string))


(defun binary-string-to-decimal (binary-string) 
  (parse-integer binary-string :radix 2))

(defun find-next-pixel (code index) 
  (subseq code index (+ index 1)))


(defun new-picture (code image default-pixel)
  (let* ((old-dimensions (array-dimensions image))
         (old-rows (first old-dimensions))
         (old-cols (second old-dimensions))
         (new-rows (+ old-rows 4))
         (new-cols (+ old-cols 4))
         (new-image (make-array (list new-rows new-cols) :initial-element "." :element-type 'string)))
    (loop for i from -2 below (+ old-rows 2) do 
          (loop for j from -2 below (+ old-cols 2) do 
                (setf (aref new-image (+ i 2) (+ j 2))
                      (find-next-pixel code 
                                       (binary-string-to-decimal
                                         (build-binary-string image i j default-pixel))))))
    new-image))



(defun count-mega-pixels (image)
  (let ((count 0))
    (destructuring-bind (rows cols) (array-dimensions image)
      (loop for i from 0 below rows do 
            (loop for j from 0 below cols do 
                  (if (string= (aref image i j) "#") (incf count)))))
    count))


(defun solve-one () 
  "Solve day 20 part one" 
  (multiple-value-bind (code image) (read-input nil)
    (let ((default-pixel "."))
      ;; First enhancement
      (setf image (new-picture code image default-pixel))
      ;; Update default pixel based on the code
      (setf default-pixel (if (string= (subseq code 0 1) "#")
                              (if (string= default-pixel ".") "#" ".")
                              default-pixel))
      ;; Second enhancement
      (setf image (new-picture code image default-pixel))
      ;; Update default pixel again if necessary
      (setf default-pixel (if (string= (subseq code 0 1) "#")
                              (if (string= default-pixel ".") "#" ".")
                              default-pixel))
      ;; Count and return the number of lit pixels
      (count-mega-pixels image))))


(solve-one)

(defun solve-two ()
  "Solve day 20 part two"
  (multiple-value-bind (code image) (read-input nil)
    (let ((default-pixel "."))
      (loop for i from 0 below 50 do 
            (setf image (new-picture code image default-pixel))
            (setf default-pixel (if (string= (subseq code 0 1) "#")
                                    (if (string= default-pixel ".") "#" ".")
                                    default-pixel))))
      (count-mega-pixels image)))

(solve-two)
