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

(defstruct queue
  (elements '()))

(defun create-queue ()
  "create an empty queue"
  (make-queue :elements '()))

(defun enqueue (queue element)
  "add an element to the end of the queue" 
  (setf (queue-elements queue) 
        (append (queue-elements queue) (list element))))

(defun dequeue (queue)
  "remove an element from the front of the queue" 
  (let ((first-element (first (queue-elements queue))))
    (setf (queue-elements queue) (rest (queue-elements queue)))
    first-element))


(defun matching-brace (open)
  "Return the expected closing brace for a given opening brace."
  (cond
    ((char= open #\( ) #\))
    ((char= open #\[ ) #\])
    ((char= open #\{ ) #\})
    ((char= open #\< ) #\>)
    (t nil)))


(defun meow (input)
  "Check if a set of braces is matched and valid."
  (let ((open-stack nil))
    (loop for char across input do 
          (cond 
            ((member char '(#\( #\{ #\[ #\<)) (push char open-stack))
            ((member char '(#\) #\} #\] #\>))
             (let ((expected (matching-brace (pop open-stack))))
               (unless (char= char expected)
                 (return char))))))
    nil))



(meow "{([(<{}[<>[]}>{[]{[(<()>")



(defun solve-one ()
  "Solve part one day ten"
  (let ((input nil) (is-test t) (sum 0))
    (setf input (read-input is-test))
    (format t "Input: ~a~%" input)))

