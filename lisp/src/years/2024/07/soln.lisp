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
      

;; 7290: 6 8 6 15
(can-yield '(6 8 6 15) 7290)


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

(defun evaluate-expression (nums ops)
  (let ((result (car nums)))
    (loop for i from 0 below (length ops)
          for op = (nth i ops)
          for num = (nth (1+ i) nums)
          do (setf result 
                   (cond ((eq op '+) (+ result num))
                         ((eq op '*) (* result num))
                         ((eq op '||) (|| result num)))))
    result))

(defun generate-operator-combinations (len)
  (if (= len 1)
      (list (list '+) (list '*) (list '||))
      (let ((result nil))
        (dolist (op '(+ * ||))
          (dolist (sub (generate-operator-combinations (1- len)))
            (push (cons op sub) result)))
        result)))

(defun can-yield-with-concat (eqn soln)
  (let ((ops-needed (1- (length eqn))))
    (some (lambda (ops)
            (= soln (evaluate-expression eqn ops)))
          (generate-operator-combinations ops-needed))))

(defun solve-two ()
  "Solve part two day seven"
  (multiple-value-bind (solns eqns) (read-input nil)
    (loop for soln in solns
          for eqn in eqns
          when (can-yield-with-concat eqn soln)
          sum soln)))

(solve-two)
