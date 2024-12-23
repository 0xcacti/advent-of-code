(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/15
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/15)

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let* ((content "")
           (parts '())
           (top "")
           (bottom "")
           (moves ""))
      ;; Read the file content
      (with-open-file (stream path :direction :input)
        (setf content (make-string (file-length stream)))
        (read-sequence content stream))
      
      ;; Split into top and bottom parts
      (setf parts (cl-ppcre:split "\\n\\n" content))
      (setf top (first parts))
      (setf bottom (second parts))
      
      ;; Get dimensions first
      (let* ((lines (cl-ppcre:split "\\n" top))
             (rows (length lines))
             (cols (length (first lines)))
             (grid (make-array (list rows cols) :initial-element ".")))  ; 2D array with known dimensions
        
        ;; Fill the grid
        (loop for line in lines
              for r from 0 do
                (loop for char across line
                      for c from 0 do
                        (setf (aref grid r c) (string char))))
        
        ;; Process moves
        (setf moves (apply #'concatenate 'string (cl-ppcre:split "\\n" bottom)))
        (values grid moves)))))

(read-input t)

(defun find-position (grid)
  (loop for r below (array-dimension grid 0) do
        (loop for c below (array-dimension grid 1)
              when (string= (aref grid r c) "@")
                return (return-from find-position (values r c)))))

(defun solve-one ()
  "Solve part one day 15" 
  (multiple-value-bind (grid moves) (read-input t)
    (format t "Grid: ~a~%" grid)
    (multiple-value-bind (r c) (find-position grid)  
      (loop for move in (cl-ppcre:split "" moves) do
        (let* ((dr (case (char move 0)
                    (#\^ -1)
                    (#\v 1)
                    (otherwise 0)))
               (dc (case (char move 0)
                    (#\< -1)
                    (#\> 1)
                    (otherwise 0)))
               (targets (list (cons r c)))
               (cr r)
               (cc c)
               (go t)
               (rows (array-dimension grid 0))
               (cols (array-dimension grid 1)))
          
          (tagbody
           loop
             (incf cr dr)
             (incf cc dc)
             ;; Check bounds before accessing array
             (when (or (< cr 0) (>= cr rows)
                      (< cc 0) (>= cc cols))
               (setf go nil)
               (go end))
             (let ((char (aref grid cr cc)))
               (cond ((string= char "#")
                      (setf go nil)
                      (go end))
                     ((string= char "O")
                      (push (cons cr cc) (cdr (last targets))))
                     ((string= char ".")
                      (go end))))
             (go loop)
           end)
          
          (unless go (continue))
          
          (setf (aref grid r c) ".")
          (setf (aref grid (+ r dr) (+ c dc)) "@")
          
          (loop for (br . bc) in (cdr targets) do
            (setf (aref grid (+ br dr) (+ bc dc)) "O"))
          
          (incf r dr)
          (incf c dc))))
    
    (let ((sum 0)
          (rows (array-dimension grid 0))
          (cols (array-dimension grid 1)))
      (loop for r below rows do
        (loop for c below cols do
          (when (string= (aref grid r c) "O")
            (incf sum (+ (* 100 r) c)))))
      sum)))

(solve-one)
