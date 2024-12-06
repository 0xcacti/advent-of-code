(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/06
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/06)

(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((mappy '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (push (loop for c across line collect c) mappy)))
      (reverse mappy))))

(read-input t)

(defun find-start (mappy)
  (loop for row in mappy
        for r from 0
        thereis (loop for symbol in row
                      for c from 0
                      when (char= symbol #\^)
                      return (list r c))))

(defun in-bounds (mappy x y)
  (and (>= x 0) 
       (< x (length (nth 0 mappy)))  ; Changed from (first mappy)
       (>= y 0) 
       (< y (length mappy))))


(defun solve-one (&optional (max-iters 10))
 "Solve day 6, part 1"
 (let* ((mappy (read-input nil))
        (pos (find-start mappy))
        (iters 0))
   (loop for row in mappy
         do (format t "~{~a~}~%" row))
   (loop while (and (in-bounds mappy (first pos) (second pos))
                    (< iters max-iters))
         count t
         do 
         (let ((current-char (nth (second pos) (nth (first pos) mappy))))
           (cond 
             ((char= current-char #\v)
              (if (not (in-bounds mappy (1+ (first pos)) (second pos)))
                  (return)  ; Stop if out of bounds
                  (if (char= (nth (second pos) (nth (1+ (first pos)) mappy)) #\#)
                      (setf (nth (second pos) (nth (first pos) mappy)) #\<)
                      (progn
                        (setf (nth (second pos) (nth (first pos) mappy)) #\x)
                        (setf (nth (second pos) (nth (1+ (first pos)) mappy)) #\v)
                        (setf pos (list (1+ (first pos)) (second pos)))))))
             ((char= current-char #\>)
              (if (not (in-bounds mappy (first pos) (1+ (second pos))))
                  (return)  ; Stop if out of bounds
                  (if (char= (nth (1+ (second pos)) (nth (first pos) mappy)) #\#)
                      (setf (nth (second pos) (nth (first pos) mappy)) #\v)
                      (progn
                        (setf (nth (second pos) (nth (first pos) mappy)) #\x)
                        (setf (nth (1+ (second pos)) (nth (first pos) mappy)) #\>)
                        (setf pos (list (first pos) (1+ (second pos))))))))
             ((char= current-char #\<)
              (if (not (in-bounds mappy (first pos) (1- (second pos))))
                  (return)  ; Stop if out of bounds
                  (if (char= (nth (1- (second pos)) (nth (first pos) mappy)) #\#)
                      (setf (nth (second pos) (nth (first pos) mappy)) #\^)
                      (progn
                        (setf (nth (second pos) (nth (first pos) mappy)) #\x)
                        (setf (nth (1- (second pos)) (nth (first pos) mappy)) #\<)
                        (setf pos (list (first pos) (1- (second pos))))))))
             ((char= current-char #\^)
              (if (not (in-bounds mappy (1- (first pos)) (second pos)))
                  (return)  ; Stop if out of bounds
                  (if (char= (nth (second pos) (nth (1- (first pos)) mappy)) #\#)
                      (setf (nth (second pos) (nth (first pos) mappy)) #\>)
                      (progn
                        (setf (nth (second pos) (nth (first pos) mappy)) #\x)
                        (setf (nth (second pos) (nth (1- (first pos)) mappy)) #\^)
                        (setf pos (list (1- (first pos)) (second pos))))))))
           (incf iters))) 
      (1+ (loop for row in mappy
         sum (count #\x row)))))

(solve-one 55)
