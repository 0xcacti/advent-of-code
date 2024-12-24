(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/15
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/15)

(defun read-input (is-test)
  "Read and parse the input file"
  (with-input (path :test is-test)
    (let* ((content "")
           (parts '())
           (top "")
           (bottom ""))
      ;; Read the file content
      (with-open-file (stream path :direction :input)
        (setf content (make-string (file-length stream)))
        (read-sequence content stream))
      
      ;; Split into top and bottom parts
      (setf parts (cl-ppcre:split "\\n\\n" content))
      (setf top (first parts))
      (setf bottom (second parts))
      
      ;; Get dimensions and create grid
      (let* ((lines (cl-ppcre:split "\\n" top))
             (rows (length lines))
             (cols (length (first lines)))
             (grid (make-array (list rows cols) :initial-element #\.)))
        
        ;; Fill the grid
        (loop for line in lines
              for r from 0 do
                (loop for char across line
                      for c from 0 do
                        (setf (aref grid r c) char)))
        
        ;; Process moves - concatenate all lines, remove newlines, and convert to string
        (values grid (cl-ppcre:regex-replace-all "\\n" bottom ""))))))

(defun find-position (grid)
  (loop for i below (array-dimension grid 0)
        thereis (loop for j below (array-dimension grid 1)
                     when (char= (aref grid i j) #\@)
                       return (list i j))))

(defun solve-one ()
  "Solve part one of day 15"
  (multiple-value-bind (grid moves) (read-input nil)
    (let* ((pos (find-position grid))
           (r (first pos))
           (c (second pos))
           (rows (array-dimension grid 0))
           (cols (array-dimension grid 1)))
      (loop for move across moves do 
            (let ((dr (cond ((char= move #\^) -1)
                           ((char= move #\v) 1)
                           (t 0)))
                  (dc (cond ((char= move #\<) -1)
                           ((char= move #\>) 1)
                           (t 0))))
              (let ((targets (list (list r c)))
                    (cr r)
                    (cc c)
                    (going t))
                (loop while t do 
                      (incf cr dr)
                      (incf cc dc)
                      (let ((char (aref grid cr cc)))
                        (cond 
                          ((char= char #\#) 
                           (setf going nil) 
                           (return))
                          ((char= char #\O)
                           (push (list cr cc) targets))
                          ((char= char #\.) 
                           (return)))))
                (when going
                  (setf (aref grid r c) #\.)
                  (setf (aref grid (+ r dr) (+ c dc)) #\@)
                  (loop for (br bc) in (butlast targets) do 
                        (setf (aref grid (+ br dr) (+ bc dc)) #\O))
                  (setf r (+ r dr))
                  (setf c (+ c dc))))))
      ;; Calculate score like the Python version
      (let ((score 0))
        (loop for row from 0 below rows do
              (loop for col from 0 below cols do
                    (when (char= (aref grid row col) #\O)
                      (incf score (+ (* 100 row) col)))))
        (format t "Score: ~a~%" score)))))

(solve-one)

(defun expand-grid (grid)
  (let* ((rows (array-dimension grid 0))
         (cols (array-dimension grid 1))
         ;; Create the larger grid, filling with dots
         (new-grid (make-array (list rows (* cols 2)) :initial-element #\.)))
    
    ;; Fill in the expanded grid - going horizontally
    (loop for r below rows do
          (loop for c below cols do
                (let ((char (aref grid r c)))
                  (case char
                    (#\# (setf (aref new-grid r (* c 2)) #\#)
                         (setf (aref new-grid r (1+ (* c 2))) #\#))
                    (#\O (setf (aref new-grid r (* c 2)) #\[)
                         (setf (aref new-grid r (1+ (* c 2))) #\]))
                    (#\@ (setf (aref new-grid r (* c 2)) #\@)
                         (setf (aref new-grid r (1+ (* c 2))) #\.))
                    (#\. (setf (aref new-grid r (* c 2)) #\.)
                         (setf (aref new-grid r (1+ (* c 2))) #\.))))))
    new-grid))

(defun print-grid (grid)
  (loop for i below (array-dimension grid 0) do
        (loop for j below (array-dimension grid 1) do
              (format t "~c" (aref grid i j)))
        (format t "~%")))

(defun solve-two ()
  "Solve part two of day 15"
  (multiple-value-bind (grid moves) (read-input t)
    (setf grid (expand-grid grid))
    (let* ((pos (find-position grid))
           (r (first pos))
           (c (second pos))
           (rows (array-dimension grid 0))
           (cols (array-dimension grid 1))
           (score 0)
           (i 0))  ; Move score declaration up
      (print-grid grid)
      (format t "rows: ~a, cols: ~a~%" rows cols)
      (format t "Initial position: ~a, ~a~%" r c)

      (loop for move across moves do 
            (if (= i 1000)
                (return-from solve-two))
            (let* ((dr (cond ((char= move #\^) -1)
                            ((char= move #\v) 1)
                            (t 0)))
                   (dc (cond ((char= move #\<) -1)
                            ((char= move #\>) 1)
                            (t 0)))
                   (targets (list (list r c)))
                   (going t))

              (loop for target in targets
                    for (cr cc) = target
                    for nr = (+ cr dr)
                    for nc = (+ cc dc)
                    do (unless (member (list nr nc) targets :test #'equal)
                         (let ((char (aref grid nr nc)))
                           (cond ((char= char #\#)
                                  (setf going nil)
                                  (return))
                                 ((char= char #\[)
                                  (setf targets 
                                        (append targets 
                                                (list (list nr nc)
                                                      (list nr (1+ nc))))))
                                 ((char= char #\])
                                  (setf targets 
                                        (append targets 
                                                (list (list nr nc)
                                                      (list nr (1- nc))))))))))

              (when going
                (let ((copy (make-array (array-dimensions grid))))
                  ;; Copy grid
                  (loop for i below rows do
                        (loop for j below cols do
                              (setf (aref copy i j) (aref grid i j))))
                  ;; Move exactly like Python
                  (setf (aref grid r c) #\.)
                  (setf (aref grid (+ r dr) (+ c dc)) #\@)
                  (loop for target in (rest targets)
                        for (br bc) = target
                        do (setf (aref grid br bc) #\.))
                  (loop for target in (rest targets)
                        for (br bc) = target
                        do (setf (aref grid (+ br dr) (+ bc dc))
                                (aref copy br bc)))
                  (setf r (+ r dr))
                  (setf c (+ c dc)))))
            (incf i)
            (print-grid grid)
            )
      ;; Only calculate score once at the very end
      (loop for row from 0 below rows do
            (loop for col from 0 below cols do
                  (when (char= (aref grid row col) #\[)
                    (incf score (+ (* 100 row) col)))))
      (print-grid grid)
      (format t "Score: ~a~%" score))))

(solve-two)
