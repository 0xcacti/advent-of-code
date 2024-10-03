(ql:quickload "cl-ppcre")

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

(defun solve-one () 
  "Solve day 14 part one" 
  (multiple-value-bind (instructions start) (read-input t)
    (let ((end "")
          (ends '()))
    (format t "Instructions: ~%")
    (print-map instructions)
    (format t "Start: ~a~%" start)
    (loop for i from 0 below 9 do 
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
      (length (first ends)))))

"a" 
"ab" 
"abc"

(solve-one)
