(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/25
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/25)

(defun lines-to-array (lines)
    (let* ((height (length lines))
         (width (length (first lines)))
         (array (make-array (list height width))))
    (loop for line in lines
          for row from 0 do
          (loop for char across line
                for col from 0 do
                (setf (aref array row col) char)))
    array))



(defun read-input (is-test)
  (with-input (path :test is-test)
    (with-open-file (stream path :direction :input)
      (let ((current-section '())
            (all-sections '()))
        (loop for line = (read-line stream nil nil)
              while line do
              (cond
                ;; Empty line - process current section if we have one
                ((string= line "")
                 (when current-section
                   (push (lines-to-array (reverse current-section)) all-sections)
                   (setf current-section nil)))
                ;; Non-empty line - add to current section
                (t (push line current-section))))
        ;; Don't forget to process the last section
        (when current-section
          (push (lines-to-array (reverse current-section)) all-sections))
        (reverse all-sections)))))

(read-input t)

(defun print-grid (grid)
  (loop for i from 0 below (array-dimension grid 0) do
        (loop for j from 0 below (array-dimension grid 1) do
              (format t "~a" (aref grid i j)))
        (format t "~%")))

(defun sort-keys-and-locks (schematics) 
  (let ((keys '())
        (locks '()))
    (loop for scheme in schematics do 
        (if (char= (aref scheme 0 0) #\#)
            (push scheme locks)
            (push scheme keys)))
    (values keys locks)))

(defun get-key-heights (keys)
  (let ((key-heights '()))
    (loop for key in keys do 
      ;; loop across columns
      (let ((heights '()))
      (loop for col from 0 below (array-dimension key 1) do 
          (let ((height 0))
          (loop for row from 0 below (array-dimension key 0) do 
              (if (char= (aref key row col) #\#) (incf height)))
          (push (1- height) heights)))
        (push (reverse heights) key-heights)))
    (reverse key-heights)))

(defun solve-one () 
  "Solve part one day 25" 
  (let ((all-schematics (read-input nil))
        (ans 0))
    (multiple-value-bind (keys locks) (sort-keys-and-locks all-schematics)
      (let ((key-heights (get-key-heights keys))
            (lock-heights (get-key-heights locks)))
        (loop for key in keys 
              for height in key-heights do 
          (format t "~a~%" key) 
          (format t "~a~%" height))

        (loop for lock in locks 
              for height in lock-heights do 
          (format t "~a~%" lock) 
          (format t "~a~%" height))

        (loop for key in key-heights do 
          (loop for lock in lock-heights do 
                ;; (format t "~a~%~a~%" key lock)
                (if (every #'(lambda (x y) (< (+ x y) 6)) key lock)
                    (incf ans)))))) ans))


(solve-one)
;; Lock 1,2,0,5,3 and key 3,0,2,0,1: all columns fit!

