(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/18
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/18)

(defun read-input (is-test &optional (coord-count nil))
  (with-input (path :test is-test)
    (with-open-file (stream path :direction :input)
      (let ((coords (loop for line = (read-line stream nil nil)
                         while line
                         collect (let* ((split-pos (position #\, line)))
                                  (list (parse-integer (subseq line 0 split-pos))
                                        (parse-integer (subseq line (1+ split-pos))))))))
        (if coord-count
            (subseq coords 0 coord-count)
            coords)))))

(defun solve-one (is-test)
  "Solve part one day 18"
  (let* ((s (if is-test 6 70))  
         (n (if is-test 12 1024))
         (grid (make-array (list (1+ s) (1+ s)) :initial-element 0))
         (coords (read-input is-test n))  
         (q (make-queue))
         (seen (make-hash-table :test #'equal)))
    
    (loop for (r c) in (subseq coords 0 n)
          do (setf (aref grid c r) 1))
    
    (enqueue q (list 0 0 0))
    (setf (gethash (list 0 0) seen) t)
    
    (loop while (not (queue-empty-p q))
          do (let* ((current (dequeue q))
                   (r (first current))
                   (c (second current))
                   (d (third current)))
               
               (dolist (next (list (list (1+ r) c)
                                 (list r (1+ c))
                                 (list (1- r) c)
                                 (list r (1- c))))
                 (let ((nr (first next))
                       (nc (second next)))
                   (when (and (>= nr 0) (>= nc 0) (<= nr s) (<= nc s)  
                            (zerop (aref grid nr nc))                  
                            (not (gethash (list nr nc) seen)))         
                     (when (and (= nr s) (= nc s))                     
                       (return-from solve-one (1+ d)))
                     (setf (gethash (list nr nc) seen) t)
                     (enqueue q (list nr nc (1+ d))))))))
    nil))


(solve-one t)
(solve-one nil)

(defun connected (coords s n)
  (let ((grid (make-array (list (1+ s) (1+ s)) :initial-element 0))
        (q (make-queue))
        (seen (make-hash-table :test #'equal)))
    (loop for (r c) in (subseq coords 0 n)
          do (setf (aref grid c r) 1))

    (enqueue q (list 0 0))
    (setf (gethash (list 0 0) seen) t)

    (loop while (not (queue-empty-p q))
          do (let* ((current (dequeue q))
                   (r (first current))
                   (c (second current)))

               (dolist (next (list (list (1+ r) c)
                                 (list r (1+ c))
                                 (list (1- r) c)
                                 (list r (1- c))))
                 (let ((nr (first next))
                       (nc (second next)))
                   (when (and (>= nr 0) (>= nc 0) (<= nr s) (<= nc s) 
                            (zerop (aref grid nr nc))                
                            (not (gethash (list nr nc) seen)))      
                     (when (and (= nr s) (= nc s))                 
                       (return-from connected t))
                     (setf (gethash (list nr nc) seen) t)
                     (enqueue q (list nr nc)))))))
    nil))

(defun solve-two (is-test)
  "Solve part two of day 18"
  (let* ((s (if is-test 6 70))
         (coords (read-input is-test)) 
         (lo 0)
         (hi (1- (length coords))))
    (loop while (< lo hi)
          do (let ((mi (floor (+ lo hi) 2)))
               (if (connected coords s (1+ mi))
                   (setf lo (1+ mi))
                   (setf hi mi))))
    (let ((result (nth lo coords)))
      (format t "~{~a~^,~}~%" result))))

(solve-two nil)
