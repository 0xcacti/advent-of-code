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

(defun should-move-from-room-p (burrow amphipod room-slots)
  "Determine if an amphipod should move from its current room."
  (some (lambda (r)
          (let ((char (aref burrow (loc r))))
            (and (valid-amphipod-p char)
                 (char/= char amphipod))))
        room-slots))

(defun room-ready-p (burrow amphipod room-slots)
  "Check if a room is ready to receive its designated amphipod."
  (every (lambda (r)
           (let ((char (aref burrow (loc r))))
             (or (char= char #\.)
                 (char= char amphipod))))
         room-slots))

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
                         (should-move-from-room-p burrow amphipod room-slots))))
        (when movable
          (dolist (h *hallway*)
            (let ((path-clear t))
              (if (< (first h) (first a))
                  (loop for x from (1+ (first h)) to (1- (first a))
                        do (unless (empty-p burrow (list x 1))
                             (setf path-clear nil)))
                  (loop for x from (1+ (first a)) to (1- (first h))
                        do (unless (empty-p burrow (list x 1))
                             (setf path-clear nil))))
              (when (and path-clear (empty-p burrow h))
                (push h reachable))))
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
        (amphipod (aref burrow (loc a))))
    (when (valid-amphipod-p amphipod)
      (let* ((room (cdr (assoc amphipod *amphipod-room*)))
             (room-x (first (first room)))
             (room-slots (get-room-slots burrow room))
             (path-clear t))
        
        ;; Check if path to room is clear
        (if (< (first a) room-x)
            (loop for x from (1+ (first a)) to (1- room-x)
                  do (unless (empty-p burrow (list x 1))
                       (setf path-clear nil)))
            (loop for x from (1+ room-x) to (1- (first a))
                  do (unless (empty-p burrow (list x 1))
                       (setf path-clear nil))))
        
        (when (and path-clear (room-ready-p burrow amphipod room-slots))
          ;; Find deepest available slot
          (let ((target-slot (find-if #'(lambda (r) (empty-p burrow r))
                                     (reverse room-slots))))
            (when target-slot
              (let* ((next (concatenate 'string
                                      (subseq burrow 0 (loc a))
                                      "."
                                      (subseq burrow (1+ (loc a)) (loc target-slot))
                                      (string amphipod)
                                      (subseq burrow (1+ (loc target-slot)))))
                     (energy (* (+ (abs (- (first a) (first target-slot)))
                                 (abs (- (second a) (second target-slot))))
                              (cdr (assoc amphipod *amphipod-consumption*)))))
                (push (make-move-step :burrow next :energy energy) results)))))))
    results))

(defun move (burrow)
  "Generates all valid next states by moving amphipods from rooms to hallway or hallway to rooms."
  (let ((nexts '()))
    ;; Try room to hallway moves first
    (dolist (room *rooms*)
      (let ((slots (get-room-slots burrow room)))
        (dolist (slot slots)
          (unless (empty-p burrow slot)
            (setf nexts (append nexts (move-to-hallway burrow slot)))
            (return)))))
    
    ;; Then try hallway to room moves
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
                   (when (or (not (gethash next-burrow *memo*))
                            (< energy (gethash next-burrow *memo*)))
                     (setf (gethash next-burrow *memo*) energy)
                     (push next-burrow queue))))))
    (gethash (parse-input target) *memo*)))

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


(defun solve-part-a (input)
  (organize input
           '("#############"
             "#...........#"
             "###A#B#C#D###"
             "  #A#B#C#D#  "
             "  #########")))


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

