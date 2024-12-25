(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/21
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/21)

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((lines '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line do
              (push line lines)))
      (reverse lines))))

(read-input t)

(defun compute-seqs (keypad)
  (let ((pos (make-hash-table :test #'equal))
        (seqs (make-hash-table :test #'equal)))
    ;; First build position map
    (loop for r from 0 below (array-dimension keypad 0) do
          (loop for c from 0 below (array-dimension keypad 1) do
                (when (aref keypad r c)
                  (setf (gethash (aref keypad r c) pos) (cons r c)))))
    
    ;; Get sorted keys
    (let ((sorted-keys (sort (alexandria:hash-table-keys pos) #'string<)))
      ;; Compute sequences using sorted keys
      (dolist (x sorted-keys)
        (dolist (y sorted-keys)
          (if (equal x y)
              (setf (gethash (cons x y) seqs) (list "A"))
              (let ((possibilities nil)
                    (q (list (cons (gethash x pos) "")))
                    (optimal most-positive-fixnum))
                (loop while q do
                      (let* ((curr (pop q))
                            (r (car (car curr)))
                            (c (cdr (car curr)))
                            (moves (cdr curr)))
                        (when (>= (length moves) optimal)
                          (return))
                        (dolist (move '((-1 0 "^") (1 0 "v") (0 -1 "<") (0 1 ">")))
                          (let* ((nr (+ r (first move)))
                                (nc (+ c (second move)))
                                (nm (third move)))
                            (when (and (>= nr 0) (>= nc 0)
                                     (< nr (array-dimension keypad 0))
                                     (< nc (array-dimension keypad 1))
                                     (aref keypad nr nc))
                              (if (equal (aref keypad nr nc) y)
                                  (cond ((< (1+ (length moves)) optimal)
                                         (setf optimal (1+ (length moves)))
                                         (setf possibilities (list (concatenate 'string moves nm "A"))))
                                        ((= (1+ (length moves)) optimal)
                                         (push (concatenate 'string moves nm "A") possibilities)))
                                  (setf q (append q (list (cons (cons nr nc)
                                                              (concatenate 'string moves nm)))))))))))
                (setf (gethash (cons x y) seqs) (nreverse possibilities))))))
    seqs)))

(defun cartesian-product (lists)
  (if (null lists)
      (list nil)
      (loop for head in (car lists)
            nconc (loop for tail in (cartesian-product (cdr lists))
                       collect (cons head tail)))))


(defun solve (string seqs)
  (format t "~%Solving for string: ~a~%" string)
  (let* ((input (concatenate 'string "A" string))
         (pairs (loop for i below (length string)
                     collect (cons (char input i) 
                                 (char string i)))))
    (format t "Pairs: ~a~%" pairs)
    (let ((opts (loop for pair in pairs
                     for key = (cons (string (car pair)) 
                                   (string (cdr pair)))
                     do (format t "Looking up ~a: ~a~%" key (gethash key seqs))
                     collect (gethash key seqs))))
      (format t "Opts: ~a~%" opts)
      (when (every #'identity opts)
        (let ((product (cartesian-product opts)))
          (format t "Product: ~a~%" product)
          (when product
            (mapcar (lambda (combo)
                     (apply #'concatenate 'string combo))
                   product)))))))

(defun print-seqs (seqs)
  (format t "All keys:~%")
  (loop for k being the hash-keys of seqs do
        (format t "~a~%" k))
  (format t "~%All key-value pairs:~%")
  (maphash (lambda (k v)
             (format t "~a -> ~a~%" k v))
           seqs))

(defun solve-one ()
  "Solve part one day 21"
  (let* ((lines (read-input nil))
         (num-keypad #2A(("7" "8" "9")
                        ("4" "5" "6")
                        ("1" "2" "3")
                        (nil "0" "A")))
         (dir-keypad #2A((nil "^" "A")
                        ("<" "v" ">")))
         (num-seqs (compute-seqs num-keypad))
         (dir-seqs (compute-seqs dir-keypad))
         (dir-lengths (let ((lengths (make-hash-table :test #'equal)))
                       (maphash (lambda (k v)
                                (setf (gethash k lengths) (length (first v))))
                              dir-seqs)
                       lengths))
         (total 0))

    (labels ((compute-length (seq &optional (depth 2))
           (format t "Computing length for ~a at depth ~a~%" seq depth)
           (if (= depth 1)
               (loop for x across (concatenate 'string "A" seq)
                     for y across seq
                     for key = (cons (string x) (string y))
                     do (format t "Length lookup ~a: ~a~%" key (gethash key dir-lengths))
                     sum (or (gethash key dir-lengths) 0))
               (loop for x across (concatenate 'string "A" seq)
                     for y across seq
                     for key = (cons (string x) (string y))
                     sum (let ((seqs (gethash key dir-seqs)))
                           (format t "Seq lookup ~a: ~a~%" key seqs)
                           (if (null seqs)
                               0
                               (reduce #'min 
                                     (mapcar (lambda (subseq)
                                             (compute-length subseq (1- depth)))
                                           seqs))))))))

    
      (format t "do we get here")
      (dolist (line lines)
        (let* ((inputs (solve line num-seqs))
               (length (apply #'min (mapcar #'compute-length inputs))))
          (incf total (* length (parse-integer (subseq line 0 (1- (length line))))))))
      total)))

(solve-one)

(defun solve-two ()
  "Solve part two day 21"
  (let* ((lines (read-input nil))
         (num-keypad #2A(("7" "8" "9")
                        ("4" "5" "6")
                        ("1" "2" "3")
                        (nil "0" "A")))
         (dir-keypad #2A((nil "^" "A")
                        ("<" "v" ">")))
         (num-seqs (compute-seqs num-keypad))
         (dir-seqs (compute-seqs dir-keypad))
         (dir-lengths (let ((lengths (make-hash-table :test #'equal)))
                       (maphash (lambda (k v)
                                (setf (gethash k lengths) (length (first v))))
                              dir-seqs)
                       lengths))
         (memo (make-hash-table :test 'equal))
         (total 0))
    (labels ((compute-length (seq &optional (depth 25))
               (let ((key (cons seq depth)))
                 (or (gethash key memo)
                     (setf (gethash key memo)
                           (if (= depth 1)
                               (loop for x across (concatenate 'string "A" seq)
                                   for y across seq
                                   for pair = (cons (string x) (string y))
                                   sum (or (gethash pair dir-lengths) 0))
                               (loop for x across (concatenate 'string "A" seq)
                                   for y across seq
                                   for pair = (cons (string x) (string y))
                                   sum (let ((seqs (gethash pair dir-seqs)))
                                        (if (null seqs)
                                            0
                                            (reduce #'min 
                                                   (mapcar (lambda (subseq)
                                                           (compute-length subseq (1- depth)))
                                                         seqs))))))))))
             (process-lines ()
               (loop for line in lines
                     for inputs = (solve line num-seqs)
                     for min-length = (apply #'min (mapcar #'compute-length inputs))
                     do (incf total (* min-length 
                                     (parse-integer (subseq line 0 (1- (length line)))))))))
      (process-lines)
      total)))

(solve-two)
