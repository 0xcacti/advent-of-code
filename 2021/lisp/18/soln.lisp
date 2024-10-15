;; Rules
;; If any pair is nested inside four pairs, the leftmost such pair explodes.
;; If any regular number is 10 or greater, the leftmost such regular number splits.
;;  During reduction, at most one action applies, after which the process returns 
;;  to the top of the list of actions. For example, 
;;  if split produces a pair that meets the explode criteria, 
;;  that pair explodes before other splits occur.
(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/18/"))
         (lines '()))
    (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
            while line 
            collect line))))


(defun find-simple-pairs (snail-num)
  "Find simple pairs in a snailfish number string."
  (let* ((pair-regex "\\[(\\d+),(\\d+)\\]")
         (pair-matches (cl-ppcre:all-matches-as-strings pair-regex snail-num))
         (pair-positions (cl-ppcre:all-matches pair-regex snail-num))
         (actual-pairs '()))
    
    (loop for pair in pair-matches
          for position in pair-positions
          do (cl-ppcre:register-groups-bind (left-num right-num)
                 ("(\\d+),(\\d+)" pair)
               (push (list (parse-integer left-num)
                           (parse-integer right-num))
                     actual-pairs)))
    
    (values (reverse pair-positions) (reverse actual-pairs))))

(defun try-explode (snail-num) 
  (multiple-value-bind (simpleNums pairs) (find-simple-pairs snail-num)
    )

(defun reduce (snail-num) 
  (let (( yes true ))
    (loop while yes do 
          (multiple-value-bind (snail-num yes) (try-explode snail-num))
          (if yes (continue))
          (multiple-value-bind (snail-num yes) (try-split snail-num))))
  snail-num)

(defun solve-one () 
  "Solve day 18 part one"
  (let* ((input (read-input t))
         (l1 (first input)))
    (loop for line in (rest input) do )))



(solve-one)
