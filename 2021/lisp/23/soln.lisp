(defparameter *room-positions* '(2 4 6 8))  
(defparameter *energy-costs* '(1 10 100 1000))  
(defparameter *test-start-state* '((1 2 1 3) 
                         (0 3 2 0)))
(defparameter *start-state* '((1 3 2 0)
                              (2 3 1 0)))

(defstruct state 
  hallway 
  rooms 
  energy)

(defun make-initial-state (input)

