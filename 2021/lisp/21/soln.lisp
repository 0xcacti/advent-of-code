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


(defun solve-two ()
  "Solve day 21 part two"
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
