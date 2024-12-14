(in-package :advent-of-code/utils)

(defun manhattan-distance (x1 y1 x2 y2) 
  "Calculate the Manhattan distance between two points"
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defun neighbors-4 (r c)
  "Return the 4 neighbors of a point"
  (list (cons (1- r) c)    ; up
        (cons (1+ r) c)    ; down
        (cons r (1- c))    ; left
        (cons r (1+ c))))  ; right

(defun neighbors-8 (x y) 
  "Return the 8 neighbors of a point"
  (loop :for dx :in '(-1 0 1)
        :append (loop :for dy :in '(-1 0 1)
                     :unless (and (zerop dx) (zerop dy))
                     :collect (cons (+ x dx) (+ y dy)))))
