(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/14
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/14)

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((robots '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do
              (let* ((parts (cl-ppcre:split " " line))
                     (robot '()))
                (loop for part in parts do 
                  (setf part (subseq part 2))
                  (let ((sub-parts (cl-ppcre:split "," part)))
                    (loop for sub-part in sub-parts do
                      (let ((sub-part (parse-integer sub-part)))
                        (push sub-part robot)))))
                (push (reverse robot) robots))))
      (reverse robots))))

(read-input t)

(defun solve-one ()
  "Solve part one day 14"
  (let* ((is-test nil)
         (robots (read-input is-test))
         (width (if is-test 11 101))
         (height (if is-test 7 103))
         (result '()))
    (loop for (px py vx vy) in robots do
          (push (list (mod (+ px (* vx 100)) width)
                     (mod (+ py (* vy 100)) height)) result))
    (setf result (reverse result))
    (let ((tl 0)
          (bl 0)
          (tr 0)
          (br 0)
          (vm (floor (- height 1) 2))
          (hm (floor (- width 1) 2)))
      ;; Don't process points on the middle lines
      (loop for (px py) in result
            when (and (not (= px hm))
                     (not (= py vm)))
            do 
            (if (< px hm)
                (if (< py vm)
                    (incf tl)
                    (incf bl))
                (if (< py vm)
                    (incf tr)
                    (incf br))))
      (* tl bl tr br))))

(solve-one)


(defun solve-two ()
  "Solve part two day 14"
  (let* ((robots (read-input nil))  
         (width 101)
         (height 103)
         (min-sf most-positive-fixnum)
         best-iteration)
    
    (loop for second from 0 below (* width height)
          do
          (let ((result '()))
            (loop for (px py vx vy) in robots do
                  (push (list (mod (+ px (* vx second)) width)
                            (mod (+ py (* vy second)) height))
                        result))
            (setf result (reverse result))
            
            (let ((tl 0)
                  (bl 0)
                  (tr 0)
                  (br 0)
                  (vm (floor (- height 1) 2))
                  (hm (floor (- width 1) 2)))
              
              (loop for (px py) in result
                    when (and (not (= px hm))
                            (not (= py vm)))
                    do 
                    (if (< px hm)
                        (if (< py vm)
                            (incf tl)
                            (incf bl))
                        (if (< py vm)
                            (incf tr)
                            (incf br))))
              
              (let ((sf (* tl bl tr br)))
                (when (< sf min-sf)
                  (setf min-sf sf)
                  (setf best-iteration second))))))
    
    best-iteration))

(solve-two)
