(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/04
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/04)

(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((word-search '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
                (let ((chars '()))
                  (loop for c across line do 
                    (push c chars))
                  (push (reverse chars) word-search))))
        (reverse word-search))))

(read-input t)

(defun in-bounds (x x-min x-max y y-min y-max)
  (and (<= x-min x) (< x x-max) (<= y-min y) (< y y-max)))

(defun directions () 
     '((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1)))


(defun bfs-one (word-search word r c) 
  (let ((total-matches 0))
  (loop for r from 0 below (length word-search) do
    (loop for c from 0 below (length (first word-search)) do
      (when (char= (nth c (nth r word-search)) #\X)
        (loop for dir in (directions) do 
          (let ((found-word t))
          (loop for i from 0 below (length word) do 
            (unless 
                (in-bounds (+ r (* i (first dir)))
                           0 
                           (length word-search) 
                           (+ c (* i (second dir)))
                           0 
                           (length (first word-search)))
              (setf found-word nil)
              (return nil))
            (unless (char= (nth (+ c (* i (second dir))) 
                             (nth (+ r (* i (first dir))) word-search))
                           (nth i word))
              (setf found-word nil)
              (return nil)))
            (when found-word (incf total-matches)))))))
    total-matches))

(defun solve-one ()
  "Solve day four, part one" 
  (let ((word-search (read-input nil)))
    (bfs-one word-search '(#\X #\M #\A #\S) 0 0)))

(solve-one)

(defun directions-two ()
  '('((-1 -1) (1 1)) '((-1 1) (1 -1))))

(defun bfs-two (word-search word r c) 
  (let ((total-matches 0))
  (loop for r from 0 below (length word-search) do
    (loop for c from 0 below (length (first word-search)) do
      (when (char= (nth c (nth r word-search)) #\A)
        (let ((found-word t))
        (loop for diag in (directions-two) do 
            (loop for dir in diag do 
            (unless 
                (in-bounds (+ r (first dir))
                           0 
                           (length word-search) 
                           (+ c (second dir))
                           0 
                           (length (first word-search)))
              (setf found-word nil))
            ;; if ((char= pos m) and (char= pos s)) or ((char= pos s) and (char= pos m))
            (unless (or 
                      (and 
                       (char= (nth (+ c (second dir) (nth (+ r (first dir) #\M)
                       (char= (nth (+ r (first dir) #\M)
                             (nth (+ r (* i (first dir))) word-search))
                           (nth i word))
              (setf found-word nil)
              (return nil)))
            (when found-word (incf total-matches)))))))
    total-matches))

(defun solve-two ()
  "Solve day four, part two"
  )

(solve-two)
