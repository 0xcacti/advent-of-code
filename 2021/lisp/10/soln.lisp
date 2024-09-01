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
  "Check if a set of parens is matched and valid." 
  (let ((stack nil))
    (loop for char across input do
          (cond 
            ((member char '(#\( #\{ #\[ #\<))
             (push char stack))
            ((member char '(#\) #\} #\] #\>))
             (if (or (null stack)
                     (not (char= (matching-brace (car stack)) char)))
                 (return-from meow char)
                 (pop stack)))))
    nil))


;; (defun solve-one ()
;;   "Solve part one day ten"
;;   (let ((input (read-input t)) 
;;         (sum 0))
;;     (loop for i from 0 below (length input) 
;;           for line = (aref input i)
;;           for result = (meow line)
;;           do 
;;           (format t "Input: ~a~%" line)
;;           when result 
;;           (incf sum (case result 
;;                       (#\) 3)
;;                       (#\] 57)
;;                       (#\} 1197)
;;                       (#\> 25137)
;;                       (otherwise 0)))
;;   (format t "Sum: ~a~%" sum)))

(defun solve-one ()
  "Solve part one day ten"
  (let ((input (read-input nil)) 
        (sum 0))
    (loop for i from 0 below (length input) 
          for line = (aref input i)
          for result = (meow line)
          do 
          (format t "Input: ~a~%" line)
          when result 
          do (incf sum (case result 
                         (#\) 3)
                         (#\] 57)
                         (#\} 1197)
                         (#\> 25137)
                         (otherwise 0))))
    (format t "Sum: ~a~%" sum)))


(defun matching-brace (char)
  "Returns the matching closing brace for a given opening brace"
  (case char
    (#\( #\))
    (#\[ #\])
    (#\{ #\})
    (#\< #\>)
    (otherwise nil)))


(defun find-matches (input)
  "Build a string to complete the match and return unmatched opening brackets" 
  (let ((stack nil))
    (loop for char across input do
          (cond 
            ((member char '(#\( #\{ #\[ #\<))
             (push char stack))
            ((member char '(#\) #\} #\] #\>))
             (if (or (null stack)
                     (not (char= (matching-brace (car stack)) char)))
                 (return-from find-matches nil)
                 (pop stack)))))
    stack))

(defun line-sum (line)
  "find sum for a line"
  (let ((unmatches (find-matches line))
        (sum 0))
    (format t "Unmatches: ~a~%" unmatches)
    (when unmatches
      (loop for char across (coerce unmatches 'string) do 
            (setf sum (* sum 5))
            (incf sum (case char 
                        (#\( 1)
                        (#\[ 2)
                        (#\{ 3)
                        (#\< 4)
                        (otherwise 0)))))
    sum))


(line-sum "[({(<(())[]>[[{[]{<()<>>")


(defun solve-two ()
  "Solve part two day ten"
  (let ((input (read-input nil)) (scores (make-array 0 :adjustable t :fill-pointer 0)))
    (setf input (remove-if-not #'(lambda (line) (null (meow line))) input))
    (format t "Input: ~a~%" input)
    (loop for i from 0 below (length input)
          for line = (aref input i)
          for result = (line-sum line)
          do 
          (vector-push-extend result scores))
    (setf scores (sort scores #'<))
    (format t "Winner is: ~a~%" (aref scores (+ (truncate (/ (length scores) 2) 1))))))

