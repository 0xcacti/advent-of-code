(ql:quickload "cl-ppcre")

(defun string-contains-p (substring string)
  (not (null (search substring string))))

(defun last-n-chars (n string) 
  (subseq string (- (length string) n)))

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/13/"))
         (coords (make-hash-table :test 'equal))
         (folds '()))
    (format t "Input file: ~a~%" input-file)
    (format t "Path: ~a~%" path)
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (when (and line (not (string= line "")))
                (cond 
                  ((string-contains-p "," line)
                   (let ((line-array (cl-ppcre:split "," line)))
                      (setf 
                        (gethash 
                          (list 
                           (parse-integer (first line-array)) 
                           (parse-integer (second line-array)))
                          coords) t)))
                  ((string-contains-p "=" line) 
                   (let ((line-array (cl-ppcre:split "=" line)))
                     (push (list 
                             (first line-array) 
                             (parse-integer (second line-array))) folds))))))
    (values coords (reverse folds)))))

(read-input t)

(defun print-map(coords) 
    (maphash (lambda (k v) (format t "~a->~a~%" k v)) coords))

(defun get-max-x (coords)
  (let ((max-x 0))
    (maphash (lambda (k v) (setf max-x (max max-x (first k)))) coords)
    max-x))

(defun get-max-y (coords)
  (let ((max-y 0))
    (maphash (lambda (k v) (setf max-y (max max-y (second k)))) coords)
    max-y))

(defun perform-y-fold (coords fold-line max-x max-y)
  (let ((new-coords (make-hash-table :test 'equal)))
    (maphash (lambda (k v) 
               (let* ((x (first k))
                      (y (second k))
                      (new-y (if (< y fold-line)
                                 y
                                 (- fold-line (- y fold-line)))))
                 (setf (gethash (list x new-y) new-coords) t))) coords)
    new-coords))

(defun perform-x-fold (coords fold-line max-x max-y)
  (let ((new-coords (make-hash-table :test 'equal)))
    (maphash (lambda (k v) 
               (let* ((x (first k))
                      (y (second k))
                      (new-x (if (< x fold-line)
                                 x
                                 (- fold-line (- x fold-line)))))
                 (setf (gethash (list new-x y) new-coords) t))) coords)
    new-coords))



;; fold around 7 
;; (9 10) -> (9 4)
;; map y = 14 
;; 14 - 10 = 4

(defun solve-one ()
  (multiple-value-bind (coords folds) (read-input nil)
    (format t "Coords:~%")
    (print-map coords)
    (format t "Folds: ~a~%" folds)
    (let ((max-x (get-max-x coords))
          (max-y (get-max-y coords))
          (fold-line (second (first folds))))
      (format t "Max X: ~a~%" max-x)
      (format t "Max Y: ~a~%" max-y)
      (format t "Fold Line: ~a~%" fold-line)
    (setf coords (perform-x-fold coords fold-line max-x max-y))
    (format t "num coords:~a~%" (hash-table-count coords)))))

(solve-one)

(defun pretty-print(coords) 
    (let ((max-x (get-max-x coords))
          (max-y (get-max-y coords)))
    (loop for y from 0 to max-y do 
          (loop for x from 0 to max-x do 
                (if (gethash (list x y) coords)
                    (format t "#")
                    (format t ".")))
          (format t "~%"))))

(defun solve-two ()
  (multiple-value-bind (coords folds) (read-input nil)
    (format t "Coords:~%")
    (print-map coords)
    (format t "Folds: ~a~%" folds)
    (loop for fold in folds do 
        (let ((max-x (get-max-x coords))
              (max-y (get-max-y coords))
              (fold-direction (last-n-chars 1 (first fold)))
              (fold-line (second fold)))
        (if (string= fold-direction "x")
            (setf coords (perform-x-fold coords fold-line max-x max-y))
            (setf coords (perform-y-fold coords fold-line max-x max-y)))))

    (pretty-print coords)))
(solve-two)
