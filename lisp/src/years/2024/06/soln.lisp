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


(defun solve-one ()
 "Solve day 6, part 1"
 (let* ((mappy (read-input nil))
        (pos (find-start mappy))
        (iters 0)
        (output-file "map-states.txt"))
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
      (1+ (loop for row in mappy
         sum (count #\x row)))))

(solve-one)

(defun copy-map (mappy)
  "Create a deep copy of the map"
  (mapcar #'copy-list mappy))

(defun detect-loop (positions)
  "Look for repeating sequences of positions"
  (let ((pos-list (reverse positions)))
    (loop for i from 0 below (- (length pos-list) 4)
          do (loop for pattern-len from 2 to (min 20 (floor (length pos-list) 2))
                  when (and (<= (+ i pattern-len) (length pos-list))
                           (>= (length pos-list) (* 2 pattern-len))
                           (equal (subseq pos-list i (+ i pattern-len))
                                 (subseq pos-list (+ i pattern-len) (+ i (* 2 pattern-len)))))
                  return t)
          finally (return nil))))



(defun detect-pattern-match (mappy prev-map)
  "Check if the current map state matches a previous state exactly"
  (loop for row1 in mappy
        for row2 in prev-map
        always (equal row1 row2)))

(defun print-state (mappy)
  (loop for row in mappy do
        (format t "~{~a~}~%" row))
  (format t "~%"))


(defun count-xs-around (mappy r c)
  "Count how many x's are in adjacent positions"
  (let ((count 0))
    (when (and (in-bounds mappy (1+ r) c)
               (char= (nth c (nth (1+ r) mappy)) #\x))
      (incf count))
    (when (and (in-bounds mappy (1- r) c)
               (char= (nth c (nth (1- r) mappy)) #\x))
      (incf count))
    (when (and (in-bounds mappy r (1+ c))
               (char= (nth (1+ c) (nth r mappy)) #\x))
      (incf count))
    (when (and (in-bounds mappy r (1- c))
               (char= (nth (1- c) (nth r mappy)) #\x))
      (incf count))
    count))

(defun simulate-guard (mappy pos max-steps)
  "Simulate guard movement using part 1 logic"
  (let ((steps 0)
        (current-pos pos)
        (revisits 0))
    (loop while (and (in-bounds mappy (first current-pos) (second current-pos))
                     (< steps max-steps))
          do 
          (let* ((r (first current-pos))
                 (c (second current-pos))
                 (current-char (nth c (nth r mappy))))
            
            ;; Check for multiple adjacent x's - indicating we're revisiting an area
            (when (and (> steps 8)
                      (> (count-xs-around mappy r c) 1))
              (incf revisits))
            
            ;; If we've revisited areas multiple times, it's a real loop
            (when (> revisits 4)
              (return-from simulate-guard t))
            
            (cond 
              ((char= current-char #\v)
               (if (not (in-bounds mappy (1+ r) c))
                   (return-from simulate-guard nil)  ; Exit map
                   (if (char= (nth c (nth (1+ r) mappy)) #\#)
                       (setf (nth c (nth r mappy)) #\<)
                       (progn
                         (setf (nth c (nth r mappy)) #\x)
                         (setf (nth c (nth (1+ r) mappy)) #\v)
                         (setf current-pos (list (1+ r) c))))))
              ((char= current-char #\>)
               (if (not (in-bounds mappy r (1+ c)))
                   (return-from simulate-guard nil)  ; Exit map
                   (if (char= (nth (1+ c) (nth r mappy)) #\#)
                       (setf (nth c (nth r mappy)) #\v)
                       (progn
                         (setf (nth c (nth r mappy)) #\x)
                         (setf (nth (1+ c) (nth r mappy)) #\>)
                         (setf current-pos (list r (1+ c)))))))
              ((char= current-char #\<)
               (if (not (in-bounds mappy r (1- c)))
                   (return-from simulate-guard nil)  ; Exit map
                   (if (char= (nth (1- c) (nth r mappy)) #\#)
                       (setf (nth c (nth r mappy)) #\^)
                       (progn
                         (setf (nth c (nth r mappy)) #\x)
                         (setf (nth (1- c) (nth r mappy)) #\<)
                         (setf current-pos (list r (1- c)))))))
              ((char= current-char #\^)
               (if (not (in-bounds mappy (1- r) c))
                   (return-from simulate-guard nil)  ; Exit map
                   (if (char= (nth c (nth (1- r) mappy)) #\#)
                       (setf (nth c (nth r mappy)) #\>)
                       (progn
                         (setf (nth c (nth r mappy)) #\x)
                         (setf (nth c (nth (1- r) mappy)) #\^)
                         (setf current-pos (list (1- r) c)))))))
            (incf steps))
          finally (return nil))))

(defun solve-two ()
  "Solve part 2 by finding all positions that create loops"
  (let* ((original-map (read-input t))
         (start-pos (find-start original-map))
         (valid-positions 0))
    (loop for row from 0 below (length original-map)
          do (loop for col from 0 below (length (first original-map))
                   do (let ((test-map (copy-map original-map)))
                        (when (and (char= (nth col (nth row test-map)) #\.)
                                 (not (equal (list row col) start-pos)))
                          (setf (nth col (nth row test-map)) #\#)
                          (when (simulate-guard test-map start-pos 50)
                            (incf valid-positions))))))
    valid-positions))

(solve-two)
