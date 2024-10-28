;; =============================
;; Load Necessary Libraries
;; =============================

(ql:quickload :pileup)
(use-package :pileup)

;; =============================
;; Define the A* Package
;; =============================

(defpackage :astar
  (:use :cl)
  (:import-from :alexandria :rcurry)
  (:import-from :pileup :make-heap :heap-insert :heap-pop)
  (:export :a* :unit-cost))

(in-package :astar)

;; =============================
;; A* Algorithm Implementation
;; =============================

(defun build-path (state previous)
  "Reconstruct the path from the start state to the given state using the previous hash table."
  (let ((path '()))
    (loop while state
          do (push state path)
             (setf state (gethash state previous)))
    path))

(defun a* (&key start neighbors goalp (heuristic (constantly 0)) test)
  "A* search algorithm.
:start - initial state
:neighbors - function to get neighbors
:goalp - predicate to determine goal state
:heuristic - heuristic function
:test - hash table test
Returns a multiple value of (path cost)."
  (let ((cost-to  (make-hash-table :test test))
        (previous (make-hash-table :test test))
        (frontier (make-heap #'< :key #'cdr)))
    (setf (gethash start cost-to) 0)
    (heap-insert frontier (cons start 0))
    (loop for (state . _) = (heap-pop frontier)
          while state
          for cost-so-far = (gethash state cost-to)
          do (when (funcall goalp state)
               (return (values (build-path state previous) cost-so-far)))
          (loop for (neighbor . cost) in (funcall neighbors state)
                for new-cost = (+ cost-so-far cost)
                for prev-cost = (gethash neighbor cost-to)
                when (or (null prev-cost) (< new-cost prev-cost))
                  do (progn
                       (setf (gethash neighbor cost-to) new-cost)
                       (setf (gethash neighbor previous) state)
                       (heap-insert frontier
                                    (cons neighbor
                                          (+ new-cost (funcall heuristic neighbor))))))
          finally (return (values nil most-positive-fixnum)))))

(defun unit-cost (neighbors)
  "Wrap neighbors with unit cost."
  (lambda (state)
    (mapcar (rcurry #'cons 1) (funcall neighbors state))))

;; =============================
;; Switch Back to the User Package
;; =============================

(in-package :cl-user)

;; =============================
;; Helper and Utility Functions
;; =============================

(defun side-room (amphipod)
  "Returns the room number corresponding to the amphipod."
  (case amphipod
    (#\A 1)
    (#\B 2)
    (#\C 3)
    (#\D 4)))

(defun energy (amphipod)
  "Returns the energy cost per step for the given amphipod."
  (case amphipod
    (#\A 1)
    (#\B 10)
    (#\C 100)
    (#\D 1000)))

(defun manhattan-distance (pos1 pos2)
  "Calculates the Manhattan distance between two positions."
  (abs (- pos1 pos2)))

(defun hall-empty-p (burrow start end)
  "Checks if the hallway between start and end positions is empty."
  (let ((hall (aref burrow 0)))
    (if (< start end)
        (loop for x from (1+ start) below end
              always (char= (aref hall x) #\.))
        (loop for x from (1+ end) below start
              always (char= (aref hall x) #\.)))))

(defun read-input (is-test)
  "Reads the input file and returns the initial burrow state as a vector.
:is-test - If true, reads 'test-input.txt', else 'input.txt'."
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/23/")))
    (with-open-file (input path :direction :input)
      (read-line input) ; Skip first line
      (let* ((hall-line (read-line input))
             (room-line1 (read-line input))
             (room-line2 (read-line input)))
        (apply #'vector
               (cons
                ;; Hallway state
                (subseq hall-line 1 12)
                ;; Room states - store as strings with top amphipod first
                (loop for col in '(3 5 7 9)
                      collect (string (char room-line1 col)
                                      (char room-line2 col)))))))))



(defun goalp (burrow)
  "Determines if the burrow state is the goal state."
  (and
   ;; Hallway must be empty
   (every (lambda (c) (char= c #\.)) (aref burrow 0))
   ;; Each room contains only the correct amphipods
   (every (lambda (room amphipod)
            (every (lambda (c) (char= c amphipod)) room))
          (vector (aref burrow 1)
                  (aref burrow 2)
                  (aref burrow 3)
                  (aref burrow 4))
          '(#\A #\B #\C #\D))))

;; =============================
;; Heuristic Function
;; =============================

(defun heuristic (state)
  "Heuristic function for A* based on the minimal energy required to move amphipods to their rooms."
  (let ((total-cost 0))
    ;; Calculate cost for amphipods in rooms
    (loop for room from 1 to 4
          for room-contents = (aref state room)
          for room-x = (* 2 room)
          do (loop for j below (length room-contents)
                    for amphipod = (aref room-contents j)
                    unless (char= amphipod #\.)
                      do (let* ((target-room (side-room amphipod))
                                (target-x (* 2 target-room))
                                (dist (+ (manhattan-distance room-x target-x)
                                         (* 2 (1+ j)))) ; Assuming each vertical step costs 2
                                )
                           (incf total-cost (* (energy amphipod) dist)))))
    ;; Calculate cost for amphipods in hallway
    (loop for pos below (length (aref state 0))
          for amphipod = (aref (aref state 0) pos)
          unless (char= amphipod #\.)
            do (let* ((target-room (side-room amphipod))
                      (target-x (* 2 target-room))
                      (dist (+ (manhattan-distance pos target-x) 1))) ; 1 step to enter room
                 (incf total-cost (* (energy amphipod) dist))))
    total-cost))

;; =============================
;; Move Generation Functions
;; =============================

(defun move (burrow from-type from-pos to-type to-pos)
  "Generates a new burrow state by moving an amphipod and calculates the move cost.
from-type, to-type - 0 for hallway, 1-4 for rooms
from-pos, to-pos - positions within the hallway or room."
  (let* ((amphipod (aref (aref burrow from-type) from-pos))
         (new-burrow (copy-seq burrow))
         (distance
          (cond
            ;; Moving from hallway to room
            ((and (= from-type 0) (not (= to-type 0)))
             (+ (abs (- from-pos (* 2 to-type))) ; Horizontal distance
                (+ to-pos 1)))                    ; Vertical distance
            ;; Moving from room to hallway
            ((and (= to-type 0) (not (= from-type 0)))
             (+ (abs (- (* 2 from-type) to-pos)) ; Horizontal distance
                (+ from-pos 1)))                  ; Vertical distance
            (t (error "Invalid move types")))))
    ;; Update burrow state
    ;; Update from-type (replace amphipod with '.')
    (setf (aref new-burrow from-type)
          (concatenate 'string 
                       (subseq (aref new-burrow from-type) 0 from-pos)
                       "."
                       (subseq (aref new-burrow from-type) (1+ from-pos))))
    ;; Update to-type (replace '.' with amphipod)
    (setf (aref new-burrow to-type)
          (concatenate 'string 
                       (subseq (aref new-burrow to-type) 0 to-pos)
                       (string amphipod)
                       (subseq (aref new-burrow to-type) (1+ to-pos))))
    ;; Return new state and cost
    (cons new-burrow (* (energy amphipod) distance))))

(defun moves (burrow)
  "Generates all possible moves from the current burrow state."
  (let ((hall-spots '(0 1 3 5 7 9 10))
        (result '()))
    ;; Room to hallway moves
    (loop for room from 1 to 4
          for room-contents = (aref burrow room)
          for top-amphipod-pos = (position #\. room-contents :test #'char/=)
          when top-amphipod-pos
            for amphipod = (aref room-contents top-amphipod-pos)
            when (or (/= room (side-room amphipod))
                     (some (lambda (c)
                             (and (char/= c #\.)
                                  (/= (side-room c) room)))
                           (subseq room-contents (1+ top-amphipod-pos))))
              do (dolist (hall-pos hall-spots)
                   (when (hall-empty-p burrow hall-pos room)
                     (push (move burrow room top-amphipod-pos 0 hall-pos) result))))
    ;; Hallway to room moves
    (loop for hall-pos in hall-spots
          for amphipod = (aref (aref burrow 0) hall-pos)
          unless (char= amphipod #\.)
            when (let* ((target-room (side-room amphipod))
                       (room-contents (aref burrow target-room))
                       (is-clear (every (lambda (c)
                                          (or (char= c #\.)
                                              (char= c amphipod)))
                                        room-contents))
                       (path-clear (hall-empty-p burrow hall-pos (* 2 target-room)))
                       (target-pos (position #\. room-contents)))
                   (and is-clear path-clear target-pos))
              do (let ((target-room (side-room amphipod))
                       (target-pos (position #\. (aref burrow (side-room amphipod)))))
                   (when target-pos
                     (push (move burrow 0 hall-pos target-room target-pos) result))))
    result))

;; =============================
;; A* Arrange Function
;; =============================

(defun arrange (burrow)
  "Finds the optimal arrangement using the A* algorithm."
  (astar:a* :start burrow 
            :neighbors #'moves 
            :goalp #'goalp
            :heuristic #'heuristic
            :test #'equal))

;; =============================
;; Expand Function for Part Two
;; =============================

(defun expand (burrow)
  "Expands the burrow for part two by inserting additional amphipods."
  (vector
   (aref burrow 0)
   "DD"
   "CB"
   "BA"
   "AC"
   (aref burrow 1)
   (aref burrow 2)
   (aref burrow 3)
   (aref burrow 4)))

;; =============================
;; Debugging Helper Functions
;; =============================

(defun print-burrow (burrow)
  "Prints the current state of the burrow."
  (format t "Hallway: ~a~%" (aref burrow 0))
  (loop for i from 1 to 4
        do (format t "Room ~a: ~a~%" i (aref burrow i)))
  (format t "~%"))

(defun debug-solve-one ()
  "Debug function to solve part one with detailed output."
  (format t "Starting solve-one~%")
  (let ((burrow (read-input nil)))
    (if burrow
        (progn
          (format t "Initial burrow state:~%")
          (print-burrow burrow)
          (format t "Checking if initial state is goal: ~a~%" (goalp burrow))
          (format t "Calculating initial moves...~%")
          (let ((initial-moves (moves burrow)))
            (format t "Number of initial moves: ~a~%" (length initial-moves))
            (format t "First few moves:~%")
            (loop for move in (subseq initial-moves 0 (min 3 (length initial-moves)))
                  do (progn
                       (print-burrow (car move))
                       (format t "Cost: ~a~%~%" (cdr move)))))
          (format t "Starting A* search...~%")
          (let ((path cost) (path cost))
            (multiple-value-bind (path cost) (arrange burrow)
              (format t "A* search result: Cost = ~a~%" cost)
              cost)))
        (format t "Failed to read input burrow~%"))))


;; =============================
;; Main Solution Functions
;; =============================

(defun solve-one ()
  "Solves part one of the problem."
  (let ((burrow (read-input nil)))
    (nth-value 1 (arrange burrow))))

(defun solve-two ()
  "Solves part two of the problem."
  (let ((burrow (read-input nil))
        (expanded '()))
    (setf expanded (expand burrow))
    (nth-value 1 (arrange expanded))))

;; =============================
;; Execution
;; =============================

(format t "Part 1: ~a~%" (solve-one))
(format t "Part 2: ~a~%" (solve-two))

;; Optional: Run debug solve
(debug-solve-one)

