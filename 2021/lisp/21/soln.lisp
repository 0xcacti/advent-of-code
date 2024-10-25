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



(defun solve-one ()
  "Solve day 21 part one"
  (multiple-value-bind (player-one-pos player-two-pos) (read-input t)
    (format t "Player 1 starting position: ~a~%" player-one-pos)
    (format t "Player 2 starting position: ~a~%" player-two-pos)
    (let ((current-dice-val 1))
        (setf player-one
    ))

(solve-one)
