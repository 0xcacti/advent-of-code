(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/19
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/19)

(defun read-input (is-test)
  "Read patterns and designs from the input file."
  (with-input (path :test is-test)
    (let ((patterns nil)
          (designs '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              for i from 0
              while line do
              (cond
                ((zerop i)
                 (setf patterns (cl-ppcre:split ", " line)))
                ((> i 1)
                 (push line designs)))))
      (values patterns (reverse designs)))))

(defun can-obtain (design patterns max-len memo)
  "Determine if the given design can be obtained using the patterns."
  (or (gethash design memo) ; Check if already computed
      (progn
        (setf (gethash design memo) ; Assume it's false until proven true
              (or
                (string= design "") ; Empty design is trivially obtainable
                (loop for i from 1 to (min (length design) max-len)
                      thereis (and
                                (member (subseq design 0 i) patterns :test #'string=)
                                (can-obtain (subseq design i) patterns max-len memo))))))
        (gethash design memo)))

(defun solve-one ()
  "Solve part one of day 19."
  (multiple-value-bind (patterns designs) (read-input nil)
    (let* ((max-len (apply #'max (mapcar #'length patterns)))
           (memo (make-hash-table :test 'equal)))
      (format t "Patterns: ~a~%" patterns)
      (format t "Designs: ~a~%" designs)
      (let ((count (count-if (lambda (design)
                               (can-obtain design patterns max-len memo))
                             designs)))
        (format t "Number of possible designs: ~a~%" count)))))

(solve-one)

(defun count-possibilities (design patterns max-len memo)
  "Count the number of ways the given design can be constructed using the patterns."
  (or (gethash design memo) ; Check if already computed
      (let ((result
             (if (string= design "")
                 1 ; Base case: empty design has one way to be constructed (do nothing)
                 (loop for i from 1 to (min (length design) max-len)
                       sum (if (member (subseq design 0 i) patterns :test #'string=)
                               (count-possibilities (subseq design i) patterns max-len memo)
                               0)))))
        (setf (gethash design memo) result) ; Cache the result
        result)))

(defun solve-two ()
  "Solve part two of day 19."
  (multiple-value-bind (patterns designs) (read-input nil)
    (let* ((max-len (apply #'max (mapcar #'length patterns)))
           (memo (make-hash-table :test 'equal)))
      (format t "Patterns: ~a~%" patterns)
      (format t "Designs: ~a~%" designs)
      (let ((total (loop for design in designs
                         sum (count-possibilities design patterns max-len memo))))
        (format t "Total number of possibilities: ~a~%" total)))))

(solve-two)

