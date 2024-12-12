(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/07
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/07)

(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((solns '())
          (eqns '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (let ((parts (cl-ppcre:split ": " line)))
                (push (parse-integer (car parts)) solns)
                (push (mapcar #'parse-integer (cl-ppcre:split " " (cadr parts))) eqns))))
        (values (reverse solns) (reverse eqns)))))

(read-input t)

(defun can-yield-two (eqn soln) 
  (if (or (= soln (apply #'* eqn))
          (= soln (apply #'+ eqn)))
      t
      nil))

(defun can-yield (eqn soln) 
  (if (= (length eqn) 2)
      (can-yield-two eqn soln)
      (or 
        (can-yield (subseq eqn 0 (1- (length eqn))) (- soln (nth (1- (length eqn)) eqn))) 
        (can-yield (subseq eqn 0 (1- (length eqn))) (/ soln (nth (1- (length eqn)) eqn))) )))

(can-yield '(10 19) 190)
(can-yield '(81 40 27) 3267)
(can-yield '(11 6 16 20) 292)
      


(defun solve-one () 
  "Solve part one day seven" 
  (multiple-value-bind (solns eqns) (read-input nil)
    (let ((ans 0))
      (loop for soln in solns
            for eqn in eqns 
            when (can-yield eqn soln) do 
            (incf ans soln))
      ans)))

(solve-one)

(defun || (a b)
   (values (parse-integer (concatenate 'string 
                                      (princ-to-string a) 
                                      (princ-to-string b)))))

(defun can-yield-two-with-concat (eqn soln) 
  (if (or (= soln (apply #'* eqn))
          (= soln (apply #'+ eqn))
          (= soln (apply #'|| eqn)))
      t
      nil))

(can-yield-two-with-concat '(15 6) 156)


(defun de-concat (a b)
  (let* ((stringified-a (princ-to-string a))
         (stringified-b (princ-to-string b))
         (len-b (length stringified-b)))
    (values (parse-integer (subseq stringified-a 0 
                                 (- (length stringified-a) 
                                    (length stringified-b)))))))

(de-concat 156 6)  


(defun can-yield-with-concat (eqn soln) 
  (if (= (length eqn) 2)
      (can-yield-two-with-concat eqn soln)
      (or 
       (can-yield-with-concat (subseq eqn 0 (1- (length eqn))) 
                             (- soln (nth (1- (length eqn)) eqn)))
       ;; Only do division if it results in an integer
       (and (= 0 (mod soln (nth (1- (length eqn)) eqn)))
            (can-yield-with-concat (subseq eqn 0 (1- (length eqn))) 
                                 (/ soln (nth (1- (length eqn)) eqn))))
       (can-yield-with-concat (subseq eqn 0 (1- (length eqn))) 
                             (de-concat soln (nth (1- (length eqn)) eqn))))))

(can-yield-with-concat '(15 6) 156)
(can-yield-with-concat '(17 8 14) 192)

(defun solve-two () 
  "Solve part two day seven"
  (multiple-value-bind (solns eqns) (read-input t)
    (let ((ans 0))
      (loop for soln in solns
            for eqn in eqns 
            when (can-yield-with-concat eqn soln) do 
            (incf ans soln))
      ans)))

(solve-two)
