(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/10/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (vector-push-extend line inp)))
    inp))

(read-input t)



(defun is-valid (input)
  "check if a set of parens is matched and valid." 
  (let ((open-stack nil) (close-stack nil))
    (loop for char across input do 
          (cond 
            ((member char "({[<") (push char open-stack))
            ((member char ")}]>") 
             (progn 
              (let ((top (car open-stack)))
                (cond 
                 ((string= top char)
                    

  )



(is-valid "{([(<{}[<>[]}>{[]{[(<()>")



(defun solve-one ()
  "Solve part one day ten"
  (let ((input nil) (is-test t) (sum 0))
    (setf input (read-input is-test))
    (format t "Input: ~a~%" input)))

(solve-one)

[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
(((({<>}<{<{<>}{[]{[]{}
{<[[]]>}<{[{[{[]{()[[[]
<{([{{}}[<[[[<>{}]]]>[]]


{([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
[[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
[{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
[<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
<{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.
