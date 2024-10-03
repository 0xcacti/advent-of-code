(ql:quickload "cl-ppcre")

(defun string-contains-p (substring string)
  (not (null (search substring string))))

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/14/"))
         (instructions (make-hash-table :test 'equal))
         (start ""))
    (format t "Input file: ~a~%" input-file)
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (when (and line (not (string= line "")))
                (cond 
                  ((string-contains-p "-" line)
                   (let ((line-array (cl-ppcre:split " -> " line)))
                      (setf 
                        (gethash (first line-array) instructions) (second line-array))))
                  (t (setf start line))))))
    (values instructions start)))


(defun print-map(coords) 
    (maphash (lambda (k v) (format t "~a->~a~%" k v)) coords))


(defun get-most-common (str) 
  (let ((counts (make-hash-table :test 'equal)))
    (loop for i from 0 below (length str) do
          (let* ((section (subseq str i (+ i 1)))
                 (count (gethash section counts 0)))
            (setf (gethash section counts) (+ count 1))))
    (let ((max-count 0)
          (max-section ""))
      (maphash (lambda (k v) 
                 (if (> v max-count) 
                     (setf max-count v max-section k))) counts)
      max-count)))

(defun get-least-common (str)
  (let ((counts (make-hash-table :test 'equal)))
    (loop for i from 0 below (length str) do
          (let* ((section (subseq str i (+ i 1)))
                 (count (gethash section counts 0)))
            (setf (gethash section counts) (+ count 1))))
    (let ((min-count 90000000000000000000)
          (min-section ""))
      (maphash (lambda (k v) 
                 (if (< v min-count) 
                     (setf min-count v min-section k))) counts)
      min-count)))


(defun solve-one () 
  "Solve day 14 part one" 
  (multiple-value-bind (instructions start) (read-input nil)
    (let ((end "")
          (ends '()))
    (format t "Instructions: ~%")
    (print-map instructions)
    (format t "Start: ~a~%" start)
    (loop for i from 0 below 10 do 
        (loop for j from 0 to (length start) 
              when (<= (+ j 2) (length start)) do
              (let* ((section (subseq start j (+ j 2)))
                     (instruction (gethash section instructions))
                     (start-ins (subseq section 0 1))
                     (end-ins (subseq section 1))
                     (to-append (concatenate 'string start-ins instruction)))
                (setf end (concatenate 'string end to-append))))
        (setf end (concatenate 'string end (subseq start (- (length start) 1))))
        (setf start end)
        (push end ends)
        (setf end ""))

      (- (get-most-common (first ends)) (get-least-common (first ends))))))


(solve-one)

(defun update-pair-counts (pair-counts char-counts instructions)
  "Update pair counts and character counts for one iteration based on instructions."
  (let ((new-pair-counts (make-hash-table :test 'equal)))
    (maphash (lambda (pair count)
               (let ((instruction (gethash pair instructions)))
                 (when instruction
                   (let* ((left (subseq pair 0 1))
                          (right (subseq pair 1 2))
                          (new-pair1 (concatenate 'string left instruction))
                          (new-pair2 (concatenate 'string instruction right)))
                     (incf (gethash new-pair1 new-pair-counts 0) count)
                     (incf (gethash new-pair2 new-pair-counts 0) count)
                     (incf (gethash instruction char-counts 0) count)))))
             pair-counts)
    new-pair-counts))

(defun solve-two () 
  "Solve day 14 part two" 
  (multiple-value-bind (instructions start) (read-input nil)
    (let ((pair-counts (make-hash-table :test 'equal))
          (char-counts (make-hash-table :test 'equal)))
      ;; Count all characters in the initial polymer
      (loop for char across start do
        (incf (gethash (string char) char-counts 0)))
      
      ;; Count initial pairs
      (loop for i from 0 below (1- (length start)) do
        (let ((pair (subseq start i (+ i 2))))
          (incf (gethash pair pair-counts 0))))

      ;; Perform 40 iterations
      (loop repeat 40 do 
        (setf pair-counts (update-pair-counts pair-counts char-counts instructions)))

      ;; Calculate the result
      (let ((counts (loop for count being the hash-values of char-counts
                          collect count)))
        (- (apply #'max counts) (apply #'min counts))))))

(solve-two)
