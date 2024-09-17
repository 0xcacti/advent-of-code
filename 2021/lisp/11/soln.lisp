
(ql:quickload "cl-ppcre")

(defun make-hset (&key (test #'eql))
  (make-hash-table :test test))

(defun hset-add (item set)
  (setf (gethash item set) t))

(defun hset-remove (item set)
  (remhash item set))

(defun hset-contains-p (item set)
  (gethash item set))

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/11/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0))
         (octopuses (make-hash-table :test 'equal)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((line-array (map 'vector #'digit-char-p line)))
              (vector-push-extend line-array inp))))
  (loop for r below (length inp) do 
        (loop for c below (length (aref inp 0)) do
          (setf (gethash (list r c) octopuses) (aref (aref inp r) c))))

  octopuses))

(read-input t)

(defun print-octopuses (octopuses) 
  (loop for r below 5 do
        (loop for c below 5 do
              (format t "~a " (gethash (list r c) octopuses)))
        (format t "~%")))

(print-octopuses (read-input t))

(defun flash-dance (octopuses &aux 
                              (octopuses) (copy-hash-table octopuses)
                              (part1 0))
  (loop for step from 1 for flashes = (dance octopuses)
        sum flashes into flash-count
        when (= step 100) do (setf part1 flash-count)
        when (= flashes 100) return (values part1 step)))

(flash-dance (read-input t))

(defun dance (curr &aux remaining (flashed (make-hset nil :test 'equal)))
  (flet ((flashesp (energy) (> energy 9))
         (flash (p)
           (hset-add p flashed)
           (setf remaining (append (neighbors curr p) remaining))))
    (loop for p being the hash-keys of curr
          when (flashesp (incf (gethash p curr))) do (flash p))
    (loop while remaining
          for n = (pop remaining)
          unless (hset-contains-p n flashed)
          when (flashesp (incf (gethash n curr))) do (flash n))
    (loop for p being the hash-keys of curr using (hash-value e)
          count (when (flashesp e) (setf (gethash p curr) 0)))))


(defun adjacents (point &key (include-diagonal nil))
  "Return the list of adjacent points to the given point (row, col).
   If :include-diagonal is true, include diagonal neighbors."
  (let* ((row (first point))
         (col (second point))
         (deltas (if include-diagonal
                     '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))
                     '((0 -1) (0 1) (-1 0) (1 0)))))
    (loop for (dr dc) in deltas
          collect (list (+ row dr) (+ col dc)))))

(defun neighbors (octopuses p)
  (loop for n in (adjacents p :include-diagonal t)
        when (gethash n octopuses) collect n))





;; (defun increment-all (grid)
;;   "increment all elements of a grid"
;;   (loop for i from 0 below (length grid)
;;         do (loop for j from 0 below (length (aref grid i))
;;                  do (incf (aref (aref grid i) j))))
;;   grid)
;; 
;; (increment-all (read-input t))
;; 
;; (defun run-flashes (grid flash-count has-flashed row col)
;;   "Run the flashes"
;;   (when (and (>= row 0) (< row (length grid))
;;              (>= col 0) (< col (length (aref grid 0)))
;;              (not (aref has-flashed row col)))
;;     (when (> (aref (aref grid row) col) 9)
;;       (setf (aref has-flashed row col) t)
;;       (incf flash-count)
;;       (setf (aref (aref grid row) col) 0)
;;       (format t "Flashing cell at ~a, ~a~%" row col)
;;       (loop for dx from -1 to 1 do
;;         (loop for dy from -1 to 1 do
;;           (let ((new-row (+ row dx))
;;                 (new-col (+ col dy)))
;;             (when (and (>= new-row 0) (< new-row (length grid))
;;                        (>= new-col 0) (< new-col (length (aref grid 0)))
;;                        (not (aref has-flashed new-row new-col)))
;;               (incf (aref (aref grid new-row) new-col))
;;               (multiple-value-setq (grid flash-count)
;;                 (run-flashes grid flash-count has-flashed new-row new-col)))))))
;;     (values grid flash-count)))
;; 
;; 
;; (run-flashes (increment-all (read-input t)) 0 (make-array (list 10 10) :initial-element nil) 0 0)
;; 
;; (defun run-step (grid)
;;   "Run a step"
;;   (let* ((rows (length grid))
;;          (cols (length (aref grid 0)))
;;          (has-flashed (make-array (list rows cols) :initial-element nil))
;;          (flash-count 0))
;;     (setf grid (increment-all grid))
;;     (loop for row from 0 below rows do
;;       (loop for col from 0 below cols do
;;         (multiple-value-setq (grid flash-count)
;;           (run-flashes grid flash-count has-flashed row col))))
;;     (values grid flash-count)))
;; 
;; (run-step (read-input t))
;; 
;; (defun print-grid (grid)
;;   (loop for row across grid do
;;     (loop for cell across row do
;;       (format t "~2d " cell))
;;     (format t "~%")))
;; 
;; (let ((grid (increment-all (read-input t))))
;;   (print-grid grid)
;;   (format t "Grid dimensions: ~dx~d~%" (length grid) (length (aref grid 0))))
;; 
;; (defparameter *test-grid* (read-input t))
;; (defparameter *flashes* 0)
;; (multiple-value-bind (new-grid flashes)
;;     (run-step *test-grid*)
;;   (setf *test-grid* new-grid)
;;   (incf *flashes* flashes))
;; 
;; 
;; (multiple-value-bind (new-grid flashes)
;;     (run-step *test-grid*)
;;   (setf *test-grid* new-grid)
;;   (incf *flashes* flashes))
;; *test-grid*
;; *flashes*

