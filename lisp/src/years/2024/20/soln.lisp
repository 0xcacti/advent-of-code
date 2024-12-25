(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/20
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/20)

(defun read-input (is-test)
  "Read the input and parse it into a 2D array."
  (with-input (path :test is-test)
    (let ((rows '())
          (cols 0))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line do
              (progn
                (setf cols (length line))
                (push (coerce line 'vector) rows))))
      (make-array (list (length rows) cols)
                  :initial-contents (reverse rows)))))

(read-input t)

(defun solve-one ()
  "Solve part one day 20"
  (let* ((grid (read-input nil))
         (rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (r nil)
         (c nil)
         (dists (make-array (list rows cols) :initial-element -1))
         (ans 0))
    ;; Find start position
    (loop for i from 0 below rows
          do (loop for j from 0 below cols
                   when (char= (aref grid i j) #\S)
                   do (setf r i
                          c j))
          until r)
    
    ;; BFS part - exactly like Python
    (setf (aref dists r c) 0)
    (loop while (not (char= (aref grid r c) #\E))
          do (loop for (nr . nc) in (list (cons (1+ r) c) 
                                         (cons (1- r) c)
                                         (cons r (1+ c))
                                         (cons r (1- c)))
                   when (and (>= nr 0) (>= nc 0)
                            (< nr rows) (< nc cols)
                            (not (char= (aref grid nr nc) #\#))
                            (= (aref dists nr nc) -1))
                   do (setf (aref dists nr nc) (1+ (aref dists r c)))
                      (setf r nr
                           c nc)
                   (return)))
    
    ;; Count part - exactly like Python
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (not (char= (aref grid r c) #\#))
                   do (loop for (nr . nc) in (list (cons (+ r 2) c)
                                                  (cons (1+ r) (1+ c))
                                                  (cons r (+ c 2))
                                                  (cons (1- r) (1+ c)))
                           when (and (>= nr 0) (>= nc 0)
                                   (< nr rows) (< nc cols)
                                   (not (char= (aref grid nr nc) #\#))
                                   (/= (aref dists r c) -1)
                                   (/= (aref dists nr nc) -1)
                                   (>= (abs (- (aref dists r c)
                                             (aref dists nr nc))) 102))
                           do (incf ans))))
    ans))

(solve-one)

(defun solve-two ()
  (let* ((grid (read-input nil))
         (rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         (start-r nil)
         (start-c nil)
         (dists (make-array (list rows cols) :initial-element -1))
         (ans 0))
    ;; Find start
    (loop for i from 0 below rows
          do (loop for j from 0 below cols
                   when (char= (aref grid i j) #\S)
                   do (setf start-r i
                          start-c j))
          until start-r)
    
    ;; BFS to fill distances
    (let ((queue (list (cons start-r start-c))))
      (setf (aref dists start-r start-c) 0)
      (loop while queue do
            (let* ((curr (pop queue))
                   (r (car curr))
                   (c (cdr curr)))
              (loop for (nr . nc) in (list (cons (1+ r) c) 
                                         (cons (1- r) c)
                                         (cons r (1+ c))
                                         (cons r (1- c)))
                    when (and (>= nr 0) (>= nc 0)
                             (< nr rows) (< nc cols)
                             (not (char= (aref grid nr nc) #\#))
                             (= (aref dists nr nc) -1))
                    do (setf (aref dists nr nc) (1+ (aref dists r c)))
                       (push (cons nr nc) queue)))))
    
    ;; Check each possible cheat
    (loop for r from 0 below rows do
          (loop for c from 0 below cols
                when (and (not (char= (aref grid r c) #\#))
                         (/= (aref dists r c) -1))
                do (loop for radius from 2 to 20 do 
                        (loop for dr from 0 to radius do
                              (let* ((dc (- radius dr))
                                     (coords (remove-duplicates 
                                             (list (cons (+ r dr) (+ c dc))
                                                   (cons (+ r dr) (- c dc))
                                                   (cons (- r dr) (+ c dc))
                                                   (cons (- r dr) (- c dc)))
                                             :test #'equal)))
                                (loop for (nr . nc) in coords 
                                      when (and (>= nr 0) (>= nc 0)
                                              (< nr rows) (< nc cols)
                                              (not (char= (aref grid nr nc) #\#))
                                              (/= (aref dists nr nc) -1)
                                              (>= (- (aref dists r c) 
                                                    (aref dists nr nc))
                                                 (+ 100 radius)))
                                      do (incf ans)))))))
    ans))

(solve-two)
