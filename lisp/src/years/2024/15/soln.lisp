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
  "Find the position of the robot (@) in the grid"
  (loop for r below (array-dimension grid 0) do
        (loop for c below (array-dimension grid 1)
              when (char= (aref grid r c) #\@)
                return (values r c))))

(defun can-move-p (grid r c dr dc moves)
  "Check if a move is valid for all pieces in the chain"
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1)))
    (loop for (curr-r . curr-c) in moves
          for next-r = (+ curr-r dr)
          for next-c = (+ curr-c dc)
          never (or (< next-r 0) (>= next-r rows)
                   (< next-c 0) (>= next-c cols)
                   (char= (aref grid next-r next-c) #\#)
                   (and (char= (aref grid next-r next-c) #\O)
                        (not (member (cons next-r next-c) moves :test #'equal)))))))

(defun move-pieces (grid moves dr dc)
  "Move all pieces in the chain by the given delta"
  (let ((pieces (reverse moves)))  ; Reverse to move from last to first
    ;; Clear old positions
    (loop for (r . c) in pieces do
          (setf (aref grid r c) #\.))
    
    ;; Set new positions
    (loop for (r . c) in pieces
          for new-r = (+ r dr)
          for new-c = (+ c dc)
          for piece = (if (equal (first pieces) (cons r c)) #\@ #\O)
          do (setf (aref grid new-r new-c) piece))))

(defun get-direction (move-char)
  "Convert move character to direction deltas"
  (case move-char
    (#\^ (list -1 0))
    (#\v (list 1 0))
    (#\< (list 0 -1))
    (#\> (list 0 1))
    (otherwise (list 0 0))))

(defun solve-one ()
  "Solve part one of day 15"
  (multiple-value-bind (grid moves) (read-input nil)
    ;; Process each move
    (loop for move across moves
          for (r c) = (multiple-value-list (find-position grid))
          for (dr dc) = (get-direction move)
          do
             (let ((move-chain (list (cons r c))))
               ;; Build chain of pieces to move
               (loop for curr-pos = (car (last move-chain))
                     for next-r = (+ (car curr-pos) dr)
                     for next-c = (+ (cdr curr-pos) dc)
                     while (and (>= next-r 0) (< next-r (array-dimension grid 0))
                               (>= next-c 0) (< next-c (array-dimension grid 1))
                               (char= (aref grid next-r next-c) #\O))
                     do (push (cons next-r next-c) (cdr (last move-chain))))
               
               ;; Check if move is valid and execute
               (when (can-move-p grid r c dr dc move-chain)
                 (move-pieces grid move-chain dr dc))))
    
    ;; Calculate final GPS coordinates with explicit 0 for empty rows
    (loop for r below (array-dimension grid 0)
          sum (or (loop for c below (array-dimension grid 1)
                       when (char= (aref grid r c) #\O)
                       sum (+ (* 100 r) c))
                  0))))  ; Return 0 if no O's found in row

(solve-one)
