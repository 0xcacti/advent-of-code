(in-package :advent-of-code/utils)

(defstruct queue
  (elements nil :type  list)
  (head 0 :type fixnum)
  (tail 0 :type fixnum))

(defun enqueue (queue element)
  (setf (queue-elements queue) (nconc (queue-elements queue) (list element)))
  (incf (queue-tail queue))
  queue)

(defun dequeue (queue) 
  (when (> (queue-tail queue) (queue-head queue))
    (let ((element (pop (queue-elements queue))))
      (incf (queue-head queue))
      element)))

(defun queue-empty-p (queue)
  (= (queue-head queue) (queue-tail queue)))
