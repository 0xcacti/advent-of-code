(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/02
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/02)

(defun read-input (is-test)
  (with-input (path :test is-test)  ; this replaces your path construction
    (let ((reports (make-array 0 :adjustable t :fill-pointer 0)))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (let ((parts (cl-ppcre:split " " line)))
                (let ((report '()))
                  (loop for part in parts do 
                        (push (parse-integer part) report))
                  (vector-push-extend (reverse report) reports))))
        reports))))


(defun is-increasing (report) 
  (loop for i from 1 below (length report) 
        always (> (nth i report) (nth (1- i) report))))

(defun is-decreasing (report) 
  (loop for i from 1 below (length report) 
        always (< (nth i report) (nth (1- i) report))))

(defun maintains-difference (report)
  (loop for i from 1 below (length report) 
        always (<=(abs (- (nth i report) (nth (1- i) report))) 3)))

(defun is-valid (report) 
  (and (or (is-decreasing report) (is-increasing report)) 
       (maintains-difference report)))


(defun solve-one () 
  "Solve day two, part one" 
  (let ((reports (read-input nil))
        (valid 0))
    (loop for i from 0 below (length reports) do 
          (let ((report (aref reports i)))
            (if (is-valid report) (incf valid))))
    valid))

(solve-one) 

(defun remove-nth (n list)
  (append (subseq list 0 n) (subseq list (1+ n))))

(defun could-be-valid (report) 
  (or (is-valid report)
      (loop for i from 0 below (length report)
            thereis (let ((modifidied-report (remove-nth i report)))
                      (is-valid modifidied-report)))))

(defun solve-two ()
  "Solve day two, part two"
  (let ((reports (read-input t))
        (valid 0))
    (loop for i from 0 below (length reports) do 
          (let ((report (aref reports i)))
            (if (could-be-valid report) (incf valid))))
    valid))

(solve-two)
