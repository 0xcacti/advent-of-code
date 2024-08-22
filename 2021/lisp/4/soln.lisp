(ql:quickload "split-sequence")
(defparameter *inp-nums* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((line (read-line stream nil)))
    (when line
      (vector-push-extend (split-sequence:split-sequence #\, line) *inp*))))

*inp-nums*
(defparameter *inp-boards* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((data-str ""))
  (loop for line = (read-line stream nil)
        while line do 
        (setf data-str (concatenate 'string data-str line)))
    (let ((parts (split-sequence:split-sequence #(#\Newline #\Newline) data-str)))
      (loop for part in parts do
        (format t "part: ~a~%" part)))))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((data-str (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line do
                          (princ line out)
                          (terpri out)))))
    (let ((parts (split-sequence:split-sequence #\Newline data-str :remove-empty-subseqs t)))
      (loop for part in (split-sequence:split-sequence #\Newline (first parts) :remove-empty-subseqs t) do
        (format t "part: ~a~%" part)))))



*inp*

