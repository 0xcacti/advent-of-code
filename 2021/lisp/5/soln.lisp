(ql:quickload "cl-ppcre")

(defparameter *board* (make-array '(9 9) :initial-element 0 :adjustable t))
(defparameter *pairs* (make-array 0 :initial-element 0 :adjustable t :fill-pointer 0))


(with-open-file (stream "~/code/challenges/aoc/2021/lisp/5/test-input.txt" :direction :input)
  (let ((data-str (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line do
                          (princ line out)
                          (terpri out)))))
    (let ((parts (cl-ppcre:split "\\n+" data-str)))
      (loop for part in parts do 
            (format t "part: ~a~%" part)
            (let* ((pairs (cl-ppcre:split " -> " part))
                  (pair1 (mapcar #'parse-integer (cl-ppcre:split "," (first pairs))))
                  (pair2 (mapcar #'parse-integer (cl-ppcre:split "," (second pairs)))))
                  (vector-push-extend (list pair1 pair2) *pairs*))))))
      
