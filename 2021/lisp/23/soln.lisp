(defvar *room-a* '((3 2) (3 3) (3 4) (3 5)))
(defvar *room-b* '((5 2) (5 3) (5 4) (5 5)))
(defvar *room-c* '((7 2) (7 3) (7 4) (7 5)))
(defvar *room-d* '((9 2) (9 3) (9 4) (9 5)))

(defvar *rooms* (list *room-a* *room-b* *room-c* *room-d*))
(defvar *hallway* '((1 1) (2 1) (4 1) (6 1) (8 1) (10 1) (11 1)))

(defvar *amphipod-consumption* (list (cons #\A 1) (cons #\B 10) (cons #\C 100) (cons #\D 1000)))
(defvar *amphipod-room* (list (cons #\A *room-a*) (cons #\B *room-b*) (cons #\C *room-c*) (cons #\D *room-d*)))

(defun loc (xy)
  (+ (* (second xy) 14) (first xy)))

(defun parse-input (lines)
  "Convert input lines into a single string representation of the burrow state."
  (let ((result (make-string (* 14 (length lines)) :initial-element #\.)))
    (loop for line in lines
          for y from 0
          do (loop for char across line
                   for x from 0
                   do (setf (char result (+ (* y 14) x)) char)))
    result))

(defstruct move-step
  burrow
  energy)

(defun folded-p (burrow)
  (= (/ (length burrow) 14) 5))

(defun empty-p (burrow pos)
  (char= (aref burrow (loc pos)) #\.))

(defun valid-amphipod-p (char)
  (member char '(#\A #\B #\C #\D)))

(defun get-room-slots (burrow room)
  "Retrieve all room slots if in unfolded (part B) layout, otherwise only top 2."
  (if (folded-p burrow)
      (subseq room 0 2)
      room))

(defun move-to-hallway (burrow a)
  "Generate all possible moves from a room position `a` to valid hallway positions."
  (let ((results '())
        (reachable '())
        (amphipod (aref burrow (loc a))))
    (when (valid-amphipod-p amphipod)
      (let* ((room (cdr (assoc amphipod *amphipod-room*)))
             (room-x (first (first room)))
             (room-slots (get-room-slots burrow room))
             (movable (or (not (equal (first a) room-x))
                          (some (lambda (r)
                                  (let ((char (aref burrow (loc r))))
                                    (and (valid-amphipod-p char)
                                         (char/= char amphipod))))
                                room-slots))))
        (when movable
          ;; Generate valid hallway moves
          (dolist (h *hallway* reachable)
            (if (< (first h) (first a))
                (if (empty-p burrow h)
                    (push h reachable)
                    (setf reachable nil))
                (when (empty-p burrow h)
                  (push h reachable))))
          ;; Calculate energy for each move to hallway
          (dolist (n reachable)
            (let* ((next (concatenate 'string 
                                      (subseq burrow 0 (loc n))
                                      (string amphipod)
                                      (subseq burrow (1+ (loc n)) (loc a))
                                      "."
                                      (subseq burrow (1+ (loc a)))))
                   (energy (* (+ (abs (- (first a) (first n)))
                                 (abs (- (second a) (second n))))
                              (cdr (assoc amphipod *amphipod-consumption*)))))
              (push (make-move-step :burrow next :energy energy) results))))))
    results))

(defun move-to-room (burrow a)
  "Moves amphipod to its designated room if reachable and room is valid."
  (let ((results '())
        (reachable t)
        (amphipod (aref burrow (loc a))))
    (when (valid-amphipod-p amphipod)
      (let* ((room (cdr (assoc amphipod *amphipod-room*)))
             (room-x (first (first room))))
        (if (< (first a) room-x)
            (dolist (h *hallway*)
              (when (and (> (first h) (first a))
                         (< (first h) room-x))
                (unless (empty-p burrow h)
                  (setf reachable nil))))
            (dolist (h *hallway*)
              (when (and (< (first h) (first a))
                         (>= (first h) room-x))
                (unless (empty-p burrow h)
                  (setf reachable nil)))))
        (when reachable
          (let ((slots (get-room-slots burrow room)))
            (dolist (r slots)
              (let ((char (aref burrow (loc r))))
                (when (and (valid-amphipod-p char)
                           (char/= char amphipod))
                  (setf reachable nil))))))
        (when reachable
          (let ((slots (get-room-slots burrow room)))
            (dolist (r (reverse slots))
              (when (empty-p burrow r)
                (let* ((next (concatenate 'string
                                          (subseq burrow 0 (loc a))
                                          "."
                                          (subseq burrow (1+ (loc a)) (loc r))
                                          (string amphipod)
                                          (subseq burrow (1+ (loc r)))))
                       (energy (* (+ (abs (- (first a) (first r)))
                                     (abs (- (second a) (second r))))
                                  (cdr (assoc amphipod *amphipod-consumption*)))))
                  (push (make-move-step :burrow next :energy energy) results)
                  (return))))))))
    results))


(defun get-room-slots (burrow room)
  "Adjusts the room slots based on folded or unfolded layout."
  (if (folded-p burrow)
      (subseq room 0 2)  ; If folded, only consider top 2 slots.
      room))             ; For unfolded layout, use all 4 slots.

(defun move (burrow)
  "Generates all valid next states by moving amphipods from rooms to hallway or hallway to rooms."
  (let ((nexts '()))
    ;; Move amphipods from rooms to hallway
    (dolist (room *rooms*)
      (let ((slots (get-room-slots burrow room)))
        (dolist (n slots)
          (unless (empty-p burrow n)
            (setf nexts (append nexts (move-to-hallway burrow n)))
            (return)))))
    ;; Move amphipods from hallway to rooms
    (dolist (h *hallway*)
      (unless (empty-p burrow h)
        (setf nexts (append nexts (move-to-room burrow h)))))
    nexts))


(defun organize (input target)
  (let* ((start (parse-input input))
         (*memo* (make-hash-table :test 'equal)))
    (setf (gethash start *memo*) 0)
    (loop with queue = (list start)
          while queue
          do (let ((burrow (pop queue)))
               (dolist (move-step (move burrow))
                 (let ((next-burrow (move-step-burrow move-step))
                       (energy (+ (gethash burrow *memo*)
                                (move-step-energy move-step))))
                   (if (or (not (gethash next-burrow *memo*))
                           (< energy (gethash next-burrow *memo*)))
                       (progn
                         (setf (gethash next-burrow *memo*) energy)
                         (push next-burrow queue)))))))
    (gethash (parse-input target) *memo*)))

(defun solve-part-a (input)
  (organize input
           '("#############"
             "#...........#"
             "###A#B#C#D###"
             "  #A#B#C#D#  "
             "  #########")))

(defun solve-part-b (input)
  (let ((unfolded-input (list (first input)
                             (second input) 
                             (third input)
                             "  #D#C#B#A#  "
                             "  #D#B#A#C#  "
                             (fourth input)
                             "  #########")))
    (organize unfolded-input
             '("#############"
               "#...........#"
               "###A#B#C#D###"
               "  #A#B#C#D#  "
               "  #A#B#C#D#  "
               "  #A#B#C#D#  "
               "  #########"))))

(format t "~A~%" 
        (solve-part-a '("#############"
                       "#...........#"
                       "###B#D#C#A###"
                       "  #C#D#B#A#  "
                       "  #########")))

(format t "Part 2 TEST: ~A~%" 
        (solve-part-b '("#############"
                       "#...........#"
                       "###B#C#B#D###"
                       "  #A#D#C#A#  "
                       "  #########")))


(format t "Part 2: ~A~%" 
        (solve-part-b '("#############"
                       "#...........#"
                       "###B#D#C#A###"
                       "  #C#D#B#A#  "
                       "  #########")))
