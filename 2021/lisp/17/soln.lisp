(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/17/"))
         (code "")
         (parts '())
         (xmin 0)
         (xmax 0)
         (ymin 0)
         (ymax 0))
    (with-open-file (stream path :direction :input)
        (loop for row from 0 
            for line = (read-line stream nil)
            while line do 
            (setf code (concatenate 'string code line))))
    (setf code (second (cl-ppcre:split ": " code)))
    (setf parts (cl-ppcre:split ", " code))
    (setf parts (map 'list (lambda (x) (subseq x 2)) parts))
    (setf xmin (parse-integer (first (cl-ppcre:split "\\.\\." (first parts)))))
    (setf xmax (parse-integer (second (cl-ppcre:split "\\.\\." (first parts)))))
    (setf ymin (parse-integer (first (cl-ppcre:split "\\.\\." (second parts)))))
    (setf ymax (parse-integer (second (cl-ppcre:split "\\.\\." (second parts)))))
    (format t "target area ~%x: (~a, ~a) y: (~a, ~a)~%" xmin xmax ymin ymax)
    (values (list xmin xmax) (list ymin ymax))))

(read-input t)

;; (defun read-input (is-test)
;;   (let* ((input-file (if is-test "test-input.txt" "input.txt"))
;;          (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/14/"))
;;          (instructions (make-hash-table :test 'equal))
;;          (start ""))
;;     (format t "Input file: ~a~%" input-file)
;;     (with-open-file (stream path :direction :input)
;;       (loop for line = (read-line stream nil)
;;             while line do 
;;             (when (and line (not (string= line "")))
;;                 (cond 
;;                   ((string-contains-p "-" line)
;;                    (let ((line-array (cl-ppcre:split " -> " line)))
;;                       (setf 
;;                         (gethash (first line-array) instructions) (second line-array))))
;;                   (t (setf start line))))))
;;     (values instructions start)))


(read-input t)


(defun solve-one () 
  (let* ((hex-code (read-input nil))
         (binary (hex-to-binary hex-code)))
    (multiple-value-bind (packet new-pos version-sum)
        (parse-packet binary 0)
      (format t "Version Sum: ~a~%" version-sum)
      version-sum)))

(solve-one)
