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

(defun write-map-state (mappy iteration output-file)
  (with-open-file (out output-file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~%Iteration ~A:~%" iteration)
    (loop for row in mappy
          do (format out "~{~a~}~%" row))
    (format out "~%")))

(defun in-bounds (mappy x y)
  (and (>= x 0) 
       (< x (length (nth 0 mappy)))  ; Changed from (first mappy)
       (>= y 0) 
       (< y (length mappy))))


(defun run-guard (mappy pos) 
   (loop while (in-bounds mappy (first pos) (second pos))
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
                        (setf pos (list (1- (first pos)) (second pos))))))))))
       mappy)

(defun solve-one ()
 "Solve day 6, part 1"
 (let* ((mappy (read-input nil))
        (pos (find-start mappy))
        (iters 0)
        (output-file "map-states.txt")
        (mappy (run-guard mappy pos)))
      (1+ (loop for row in mappy
         sum (count #\x row)))))

(solve-one)

(defun copy-map (mappy)
  (copy-tree mappy))

(defun run-guard-two (mappy pos obstacle-pos) 
  (let 
    ((seen (make-hash-table :test 'equal)))
    (setf (nth (second obstacle-pos) (nth (first obstacle-pos) mappy)) #\#)
   (loop while (in-bounds mappy (first pos) (second pos))
         count t
         do 
         (let* ((current-char (nth (second pos) (nth (first pos) mappy)))
               (state-key (list (first pos) (second pos) current-char)))
         (when (gethash state-key seen)
              (return-from run-guard-two 1))
         (setf (gethash state-key seen) t)
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
                        (setf pos (list (1- (first pos)) (second pos)))))))))))
       0)


(defun solve-two ()

  "Solve part 2 by finding all positions that create loops"
  (let* ((original-map (read-input nil))
         (start-pos (find-start original-map))
         (valid-positions 0))
    (loop for row from 0 below (length original-map)
          do 
          (format t "Row ~A of ~A ~%" row (length original-map))
          (loop for col from 0 below (length (first original-map))
                   do (let ((test-map (copy-map original-map)))
                        (incf valid-positions (run-guard-two test-map start-pos (list row col))))))
    (1- valid-positions)))

(solve-two)
