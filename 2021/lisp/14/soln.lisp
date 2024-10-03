(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/13/"))
         (coords (make-hash-table :test 'equal))
         (folds '()))
    (format t "Input file: ~a~%" input-file)
    (format t "Path: ~a~%" path)
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (when (and line (not (string= line "")))
                (cond 
                  ((string-contains-p "," line)
                   (let ((line-array (cl-ppcre:split "," line)))
                      (setf 
                        (gethash 
                          (list 
                           (parse-integer (first line-array)) 
                           (parse-integer (second line-array)))
                          coords) t)))
                  ((string-contains-p "=" line) 
                   (let ((line-array (cl-ppcre:split "=" line)))
                     (push (list 
                             (first line-array) 
                             (parse-integer (second line-array))) folds))))))
    (values coords (reverse folds)))))
