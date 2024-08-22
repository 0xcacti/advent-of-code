(ql:quickload "split-sequence")
(defparameter *inp-nums* (make-array 0 :fill-pointer 0 :adjustable t))

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((line (read-line stream nil)))
    (when line
      (vector-push-extend (split-sequence:split-sequence #\, line) *inp*))))

*inp-nums*
(defparameter *inp-boards* (make-array 0 :fill-pointer 0 :adjustable t))



(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((data-str (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line do
                          (princ line out)
                          (terpri out)))))
    (let ((parts (split-sequence:split-sequence #(#\Newline #\Newline) data-str)) (i 0))
      (loop for part in parts do
        (if (zerop i)
            (vector-push-extend (split-sequence:split-sequence #\, part) *inp-boards*)
            (let ((board (make-array 0 :fill-pointer 0 :adjustable t))) 
               (loop for line in (split-sequence:split-sequence #\Newline part) do 
                  (vector-push-extend (split-sequence:split-sequence #\, line) board))
               (vector-push-extend board *inp-boards*)))
        (incf i)))))


*inp*
*inp-boards*


(format t "womeow~%")

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/test-input.txt" :direction :input)
  (let ((data-str (with-output-to-string (out)
                    (loop for line = (read-line stream nil)
                          while line do
                          (princ line out)
                          (terpri out)))))
    
    ;; Trim any extraneous whitespace characters from the data string
    (setf data-str (string-trim '(#\Space #\Tab #\Newline) data-str))
    (format t "Data read: ~a~%" data-str)  ;; Debug: Print the entire data string

    ;; Attempt to split on both double newlines and spaces
    (let ((parts (split-sequence:split-sequence '(#\Newline #\Newline) data-str :remove-empty-subseqs t))
          (i 0))
      (format t "Number of parts: ~d~%" (length parts))  ;; Debug: Print the number of parts

      (loop for part in parts do
            (format t "Processing part ~d: ~a~%" i part)  ;; Debug: Print each part being processed

            (if (zerop i)
                ;; Handle the first part (numbers separated by commas)
                (progn
                  (format t "First part (numbers): ~a~%" part)  ;; Debug: Print first part
                  (vector-push-extend (split-sequence:split-sequence #\, part) *inp-nums*))
                
                ;; Handle the subsequent parts (5x5 boards)
                (let ((board (make-array '(5 5))))
                  (format t "meow~%")  ;; Debug: Indicate that we're entering board creation
                  (loop for line in (split-sequence:split-sequence #\Newline part) do
                        (setf line (string-trim '(#\Space #\Tab) line)) ;; Trim leading/trailing spaces
                        (let ((numbers (mapcar #'parse-integer
                                               (remove "" 
                                                       (split-sequence:split-sequence '(#\Space) line)))))
                          (format t "Row numbers: ~a~%" numbers)  ;; Debug: Print the numbers in each row
                          
                          (loop for num in numbers
                                for col from 0 do
                                (setf (aref board i col) num))))
                  (format t "Board created: ~a~%" board)  ;; Debug: Print the 5x5 board
                  (vector-push-extend board *inp-boards*)))
            (incf i)))))

