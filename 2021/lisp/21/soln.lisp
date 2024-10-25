(defun read-input (is-test) 
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/21/"))
         (player-one-pos 0)
         (player-two-pos 0))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line 
            for row from 0 do 
            (cond ((= row 0) (setf player-one-pos (parse-integer (subseq line (- (length line) 1)))))
                  ((= row 1) (setf player-two-pos (parse-integer (subseq line (- (length line) 1))))))))
      (values player-one-pos player-two-pos)))

(read-input t)

(defun die-mod (start addend)
    (1+ (mod (+ start addend -1) 10)))

(die-mod 7 5)


(defun play-game (die p-one p-two)
  "Plays the game using the given die closure and player positions, and returns the final scores."
  (let ((p-one-points 0)
        (p-two-points 0)
        (game-over nil))
    (loop while (not game-over)
          do
          (let ((p-one-next-3 (funcall die)))
            (setf p-one (die-mod p-one p-one-next-3))
            (incf p-one-points p-one))
            (if (>= p-one-points 1000)
                (return-from play-game (values p-one-points p-two-points)))

          (let ((p-two-next-3 (funcall die)))
            (setf p-two (die-mod p-two p-two-next-3))
            (incf p-two-points p-two)
            (if (>= p-two-points 1000)
                (return-from play-game (values p-two-points p-one-points)))))))

(defun solve-one ()
  "Solve day 21 part one"
  (multiple-value-bind (player-one-pos player-two-pos) (read-input nil)
    (format t "Player 1 starting position: ~a~%" player-one-pos)
    (format t "Player 2 starting position: ~a~%" player-two-pos)
    (let* ((current-dice-val 1)
          (roll-count 0)
          (die (lambda () 
            (let ((rolls (list (1+ (mod (1- current-dice-val) 100))
                               (1+ (mod current-dice-val 100))
                               (1+ (mod (1+ current-dice-val) 100)))))
              (setf current-dice-val (+ current-dice-val 3))
              (incf roll-count 3 )
              (reduce #'+ rolls)))))
      (multiple-value-bind (winner-final loser-final)
          (play-game die player-one-pos player-two-pos)
        (format t "Winner: ~a~%" winner-final)
        (format t "Loser: ~a~%" loser-final)
        (format t "Roll count: ~a~%" roll-count)
      (* roll-count loser-final)))))

(solve-one)

(defparameter *memoization-table* (make-hash-table :test 'equal))

(defun dirac-die-outcomes ()
  "Returns a list of (roll . universes) pairs representing all possible outcomes."
  '((3 . 1) (4 . 3) (5 . 6) (6 . 7) (7 . 6) (8 . 3) (9 . 1)))

(defun die-mod (position roll)
  "Computes the new position after the roll."
  (let ((new-pos (mod (+ position roll -1) 10)))
    (+ new-pos 1)))  ; Adjust for 1-based indexing

(defun play-quantum-game (p-one-pos p-one-score p-two-pos p-two-score turn)
  "Recursively explores all quantum game outcomes, returning wins for each player."
  (let ((memo-key (list p-one-pos p-one-score p-two-pos p-two-score turn)))
    (or (gethash memo-key *memoization-table*)
        (let ((result
               (cond
                 ((>= p-one-score 21) (list 1 0))  ; Player 1 wins
                 ((>= p-two-score 21) (list 0 1))  ; Player 2 wins
                 (t (let ((p-one-wins 0)
                          (p-two-wins 0))
                      (dolist (outcome (dirac-die-outcomes))
                        (let* ((roll (car outcome))
                               (universes (cdr outcome))
                               (new-pos (if (eql turn 'p1)
                                            (die-mod p-one-pos roll)
                                            (die-mod p-two-pos roll)))
                               (new-score (if (eql turn 'p1)
                                              (+ p-one-score new-pos)
                                              (+ p-two-score new-pos))))
                          (format t "Turn: ~a, Roll: ~a, Universes: ~a~%" turn roll universes)
                          (format t "New Position: ~a, New Score: ~a~%" new-pos new-score)
                          (if (>= new-score 21)
                              (if (eql turn 'p1)
                                  (incf p-one-wins universes)
                                  (incf p-two-wins universes))
                            (destructuring-bind (sub-p-one-wins sub-p-two-wins)
                                (play-quantum-game
                                 (if (eql turn 'p1) new-pos p-one-pos)
                                 (if (eql turn 'p1) new-score p-one-score)
                                 (if (eql turn 'p2) new-pos p-two-pos)
                                 (if (eql turn 'p2) new-score p-two-score)
                                 (if (eql turn 'p1) 'p2 'p1))
                              (incf p-one-wins (* universes sub-p-one-wins))
                              (incf p-two-wins (* universes sub-p-two-wins))))))
                      (list p-one-wins p-two-wins))))))
          (setf (gethash memo-key *memoization-table*) result)
          result))))

(defun solve-two ()
  "Solve day 21 part two"
  (clrhash *memoization-table*)
  (multiple-value-bind (p-one-pos p-two-pos) (read-input nil)
    (format t "Player 1 starting position: ~a~%" p-one-pos)
    (format t "Player 2 starting position: ~a~%" p-two-pos)
    (destructuring-bind (p-one-wins p-two-wins)
        (play-quantum-game p-one-pos 0 p-two-pos 0 'p1)
      (format t "Player 1 wins: ~a~%" p-one-wins)
      (format t "Player 2 wins: ~a~%" p-two-wins)
      (format t "Max wins: ~a~%" (max p-one-wins p-two-wins)))))

(solve-two)
