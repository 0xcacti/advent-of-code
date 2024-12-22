(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/13
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/13)

(defstruct machine
  a-btn-x
  a-btn-y
  b-btn-x
  b-btn-y
  prize-x
  prize-y)

(defun parse-button-line (line)
  "Parse a button line like 'Button A: X+94, Y+34' into (x y) values"
  (let* ((parts (cl-ppcre:split "[,:]" line))
         (x-part (cl-ppcre:scan-to-strings "\\d+" (second parts)))
         (y-part (cl-ppcre:scan-to-strings "\\d+" (third parts))))
    (list (parse-integer x-part) 
          (parse-integer y-part))))

(defun parse-prize-line (line)
  "Parse a prize line like 'Prize: X=8400, Y=5400' into (x y) values"
  (let* ((parts (cl-ppcre:split "[,:]" line))
         (x-part (cl-ppcre:scan-to-strings "\\d+" (second parts)))
         (y-part (cl-ppcre:scan-to-strings "\\d+" (third parts))))
    (list (parse-integer x-part)
          (parse-integer y-part))))

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((machines '()))
      (with-open-file (stream path :direction :input)
        (loop
          for line1 = (read-line stream nil)
          for line2 = (read-line stream nil)
          for line3 = (read-line stream nil)
          while line1 do
          (let* ((a-btn (parse-button-line line1))
                 (b-btn (parse-button-line line2))
                 (prize (parse-prize-line line3))
                 (machine (make-machine 
                          :a-btn-x (first a-btn)
                          :a-btn-y (second a-btn)
                          :b-btn-x (first b-btn)
                          :b-btn-y (second b-btn)
                          :prize-x (first prize)
                          :prize-y (second prize))))
            (push machine machines)
            ;; Skip blank line if it exists
            (read-line stream nil)))
        (nreverse machines)))))

(defun find-shortest () )

(defun solve-one ()
  "Solve part one day 13"
  (let ((machines (read-input nil))
        (total-tokens 0))
    (loop for machine in machines do 
      (let ((min-tokens most-positive-double-float)
            (solution-found nil))
        (loop for a from 0 to 100 do 
          (loop for b from 0 to 100 do 
            (let* ((x-pos (+ (* a (machine-a-btn-x machine)) 
                            (* b (machine-b-btn-x machine))))
                   (y-pos (+ (* a (machine-a-btn-y machine)) 
                            (* b (machine-b-btn-y machine)))))
              ;; Only process if both X and Y match
              (when (and (= x-pos (machine-prize-x machine))
                        (= y-pos (machine-prize-y machine)))
                (let ((tokens (+ (* a 3) b)))
                  (format t "Found solution for machine:~%X=~A Y=~A~%A=~A B=~A Tokens=~A~%" 
                          x-pos y-pos a b tokens)
                  (setf min-tokens (min min-tokens tokens))
                  (setf solution-found t))))))
        (when solution-found
          (format t "Adding tokens: ~A~%" min-tokens)
          (incf total-tokens min-tokens))))
    total-tokens))

(solve-one)


(defun solve-two ()
  "Solve part two day 13"
  (let ((machines (read-input nil))
        (increment-amt 10000000000000)
        (total-tokens 0))
    
    (loop for machine in machines do
      (incf (machine-prize-x machine) increment-amt)
      (incf (machine-prize-y machine) increment-amt)
      
      (let* ((ax (machine-a-btn-x machine))
             (ay (machine-a-btn-y machine))
             (bx (machine-b-btn-x machine))
             (by (machine-b-btn-y machine))
             (px (machine-prize-x machine))
             (py (machine-prize-y machine))
             (ca (/ (- (* px by) (* py bx))
                   (- (* ax by) (* ay bx))))
             (cb (/ (- px (* ax ca)) bx)))
        
        (when (and (zerop (mod ca 1))
                  (zerop (mod cb 1))
                  (plusp ca)  
                  (plusp cb))
          (let ((tokens (+ (* (floor ca) 3) 
                          (floor cb))))
            (format t "Found solution for machine: A=~A B=~A Tokens=~A~%" 
                    (floor ca) (floor cb) tokens)
            (incf total-tokens tokens)))))
    
    total-tokens))
    

(solve-two)
