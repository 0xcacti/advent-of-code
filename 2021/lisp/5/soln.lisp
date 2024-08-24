(ql:quickload "cl-ppcre")

(defparameter *test-board* (make-array '(10 10) :initial-element 0 :adjustable t))
(defparameter *board* (make-array '(1000 1000) :initial-element 0 :adjustable t))
(defparameter *pairs* '())


(with-open-file (stream "~/code/challenges/aoc/2021/lisp/5/input.txt" :direction :input)
  (let ((data-str (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line do
                          (princ line out)
                          (terpri out)))))
    (let ((parts (cl-ppcre:split "\\n+" data-str)))
      (loop for part in parts do 
            (let* ((coordinates (cl-ppcre:split " -> " part))
                  (pair1 (mapcar #'parse-integer (cl-ppcre:split "," (first coordinates))))
                  (pair2 (mapcar #'parse-integer (cl-ppcre:split "," (second coordinates)))))
                  (push (list pair1 pair2) *pairs*))))))

*board*
      
(defun populate-board ()
  (loop for pair in *pairs* do 
        (let* ((pair1 (first pair))
               (pair2 (second pair))
               (x1 (first pair1))
               (y1 (second pair1))
               (x2 (first pair2))
               (y2 (second pair2)))
          (cond
            ;; Handle horizontal line
            ((= y1 y2)
             (loop for x from (min x1 x2) to (max x1 x2) do
                   (incf (aref *board* y1 x) 1)))
            
            ;; Handle vertical line
            ((= x1 x2)
             (loop for y from (min y1 y2) to (max y1 y2) do
                   (incf (aref *board* y x1) 1)))))))
            
            ;; Handle diagonal lines (45-degree only)
            ;; ((= (abs (- x1 x2)) (abs (- y1 y2)))
            ;;  (let ((dx (if (> x2 x1) 1 -1))
            ;;        (dy (if (> y2 y1) 1 -1)))
            ;;    (loop for x = x1 then (+ x dx)
            ;;          for y = y1 then (+ y dy)
            ;;          repeat (1+ (abs (- x1 x2))) do
            ;;          (setf (aref *board* y x) 1)))))))

*pairs*
*test-board*
*board*

(defun count-twos () 
  (let ((count 0))
      (loop for row from 0 to 999 do
            (loop for col from 0 to 999 do 
                  (if (> (aref *board* row col) 1)
                      (incf count)))) count))

(defun solve-one () 
  "solve part one day 5"
  (populate-board)
  (count-twos))

(solve-one)


(defun populate-board-with-diag ()
  (loop for pair in *pairs* do 
        (let* ((pair1 (first pair))
               (pair2 (second pair))
               (x1 (first pair1))
               (y1 (second pair1))
               (x2 (first pair2))
               (y2 (second pair2)))
          (cond
            ;; Handle horizontal line
            ((= y1 y2)
             (loop for x from (min x1 x2) to (max x1 x2) do
                   (incf (aref *board* y1 x) 1)))
            
            ;; Handle vertical line
            ((= x1 x2)
             (loop for y from (min y1 y2) to (max y1 y2) do
                   (incf (aref *board* y x1) 1)))
            
            ;; Handle diagonal lines (45-degree only)
            ((= (abs (- x1 x2)) (abs (- y1 y2))) ;; checks if the line is diagonal at 45
              (let ((dx (if (> x2 x1) 1 -1))  ;; set the x travel direction
                    (dy (if (> y2 y1) 1 -1))) ;; set the y travel direction
                (loop for x = x1 then (+ x dx)  ;; loop through the x values
                      for y = y1 then (+ y dy)    ;; loop through the y values
                      repeat (1+ (abs (- x1 x2))) do  ;; loop through the absolute difference between the x values
                      (incf (aref *board* y x) 1)))))))) ;; set the value of the board to 1 if it is on the line




(defun solve-two () 
  "sole part two day 5"
  (populate-board-with-diag)
  (count-twos))


(solve-two)
