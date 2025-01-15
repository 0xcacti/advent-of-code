(in-package :advent-of-code/utils)

(defstruct set ())

(defun element-of-set (x set) 
  (cond ((not set) nil)
        ((eq x (car set)) t) 
        (t (element-of-set x (cdr set)))))

(defun append-to-set (x set)
  (if (element-of-set x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (not set1) (not set2)) '())
        ((element-of-set (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (t (intersection-set (cdr set1) set2))))


(defun union-set (set1 set2)
;; consing up a list seems to be very important


