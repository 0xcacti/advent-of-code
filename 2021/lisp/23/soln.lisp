(ql:quickload "a-star")

(defun read-input (is-test)
  (let ((input-file (if is-test "test-input.txt" "input.txt")))
    (with-open-file (input input-file)
      (read-line input) ; Skip first line
      (apply #'vector
             (subseq (read-line input) 1 12)
             (loop with r1 = (read-line input) 
                   and r2 = (read-line input)
                   for c in '(3 5 7 9)
                   collect (format nil "~c~c" (char r1 c) (char r2 c)))))))

;; Helper functions
(defun goalp (burrow)
  (every (lambda (c s) (every (lambda (x) (char= c x)) s)) 
         ".ABCD" burrow))

(defun side-room (amphipod) 
  (- (char-code amphipod) 64))

(defun energy (amphipod) 
  (expt 10 (1- (side-room amphipod))))

(defun move (burrow i j x y)
  (let* ((amphipod (char (aref burrow i) j))
         (dist (if (= i 0)
                   (+ y 1 (abs (- (* 2 x) j)))
                   (+ j 1 (abs (- (* 2 i) y)))))
         (new (map 'vector #'copy-seq burrow)))
    (rotatef (char (aref new i) j) (char (aref new x) y))
    (cons new (* (energy amphipod) dist))))

(defun hall-empty-p (burrow pos room)
  (loop with hall = (aref burrow 0)
        for x from (min pos (* 2 room)) to (max pos (* 2 room))
        always (char= (char hall x) #\.)))

(defun moves (burrow)
  (let ((hall-spots '(0 1 3 5 7 9 10)))
    (uiop:while-collecting (add)
      ;; Try moving from rooms to hallway
      (dolist (r '(1 2 3 4))
        (uiop:if-let ((j (position #\. (aref burrow r) :test #'char/=)))
          (dolist (h hall-spots)
            (when (hall-empty-p burrow h r)
              (add (move burrow r j 0 h))))))
      ;; Try moving from hallway to rooms
      (dolist (h hall-spots)
        (let ((a (char (aref burrow 0) h)))
          (unless (char= a #\.)
            (let* ((r (side-room a)) (room (aref burrow r)))
              (when (and
                     (hall-empty-p burrow (if (< h (* 2 r)) (1+ h) (1- h)) r)
                     (every (lambda (c) (member c (list #\. a))) room))
                (let ((j (position #\. (aref burrow r) :test #'char/=)))
                  (setf j (1- (or j (length (aref burrow r)))))
                  (add (move burrow 0 h r j)))))))))))

(defun arrange (burrow)
  (astar:a* :start burrow :neighbors #'moves :goalp #'goalp :test #'equalp))

(defun expand (burrow)
  (coerce 
    (loop for s across burrow 
          and x in '("" "DD" "CB" "BA" "AC")
          collect (concatenate 'string (subseq s 0 1) x (subseq s 1)))
    'vector))

(defun solve-one ()
  (let ((burrow (read-input nil)))
    (nth-value 1 (arrange burrow))))

(defun solve-two ()
  (let* ((burrow (read-input nil))
         (expanded (expand burrow)))
    (nth-value 1 (arrange expanded))))

;; Main execution
(format t "Part 1: ~a~%" (solve-one))
(format t "Part 2: ~a~%" (solve-two))
