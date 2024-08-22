(ql:quickload "split-sequence")
(ql:quickload "cl-ppcre")

(defparameter *inp-nums* (make-array 0 :fill-pointer 0 :adjustable t))
(defparameter *inp-boards* (make-array 0 :fill-pointer 0 :adjustable t))

*inp-nums*
*inp-boards*

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((data-str (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line do
                          (princ line out)
                          (terpri out)))))
    ;; Use regular expression to split on double newlines
    (let ((parts (cl-ppcre:split "\\n\\n+" data-str))
          (i 0))
      (loop for part in parts do
            (if (zerop i)
                ;; Handle the first part (numbers separated by commas)
                (progn
                  (let ((nums (mapcar #'parse-integer (split-sequence:split-sequence #\, part))))
                  (setf *inp-nums* (apply #'vector nums))))
                
                ;; Handle the subsequent parts (5x5 boards)
                (let ((board (make-array '(5 5))))
                  (loop for line in (split-sequence:split-sequence #\Newline part)
                        for row from 0 do
                        (setf line (string-trim '(#\Space #\Tab) line)) ;; Trim leading/trailing spaces
                        (let ((numbers (mapcar #'parse-integer
                                               (remove "" 
                                                       (cl-ppcre:split "\\s+" line)))))  ;; Split on one or more spaces or tabs
                          (loop for num in numbers
                                for col from 0 do
                                (setf (aref board row col) num))))
                  (vector-push-extend board *inp-boards*)))
            (incf i)))))

