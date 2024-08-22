(ql:quickload "split-sequence")
(ql:quickload "cl-ppcre")

(defparameter *inp-nums* (make-array 0 :fill-pointer 0 :adjustable t))
(defparameter *inp-boards* (make-array 0 :fill-pointer 0 :adjustable t))
(defparameter *check-boards* (make-array 0 :fill-pointer 0 :adjustable t))

*inp-nums*
*inp-boards*
*check-boards*

(with-open-file (stream "~/code/challenges/aoc/2021/lisp/4/input.txt" :direction :input)
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
                  (vector-push-extend board *inp-boards*)
                  (vector-push-extend (make-array '(5 5) :initial-element 0) *check-boards*)))
            (incf i)))))



;; loop through each number marking them off on each board


*inp-nums*
*inp-boards*
*check-boards*


(defun mark-board (num board-index)
  (let ((board (aref *inp-boards* board-index)))
  (loop for row from 0 to 4 do 
        (loop for col from 0 to 4 do 
              (if (= num (aref board row col))
                  (setf (aref (aref *check-boards* board-index) row col) 1))))))

(defun mark-all-boards (num)
  (loop for i from 0 to (- (length *inp-boards*) 1) do
        (mark-board num i)))


(defun check-column (board-index)
  (let ((board (aref *check-boards* board-index)))
    (loop for col from 0 to 4 do 
          (let ((nums (loop for row from 0 to 4 collect (aref board row col))))
            (if (every (lambda (x) (= x 1)) nums)
                (return-from check-column t))))))


;; (defun check-diagonal-down (board-index)
;;   (let ((board (aref *check-boards* board-index)))
;;     (let ((nums (loop for i from 0 to 4 collect (aref board i i))))
;;       (if (every (lambda (x) (= x 1)) nums)
;;           (return-from check-diagonal-down t)))))
;; 
;; (defun check-diagonal-up (board-index)
;;   (let ((board (aref *check-boards* board-index)))
;;     (let ((nums (loop for i from 0 to 4 collect (aref board (- 4 i) i))))
;;       (if (every (lambda (x) (= x 1)) nums)
;;           (return-from check-diagonal-up t)))))


(defun check-horizontal (board-index)
  (let ((board (aref *check-boards* board-index)))
    (loop for row from 0 to 4 do 
          (let ((nums (loop for col from 0 to 4 collect (aref board row col))))
            (if (every (lambda (x) (= x 1)) nums)
                (return-from check-horizontal t))))))

(defun check-for-win (board-index)
  (or (check-column board-index)
      (check-horizontal board-index)))


(defun find-first-solve () 
  (loop for i from 0 to (- (length *inp-nums*) 1) do
        (let ((num (aref *inp-nums* i)))
          (mark-all-boards num)
          (loop for j from 0 to (- (length *inp-boards*) 1) do 
                (when (check-for-win j)
                  (return-from find-first-solve (list j num)))))))
*check-boards*


(defun score (board-index last-num)
  (let ((board (aref *inp-boards* board-index))
       (check-board (aref *check-boards* board-index))
       (score-sum 0))
    (loop for row from 0 to 4 do 
          (loop for col from 0 to 4 do 
                (if (= (aref check-board row col) 0)
                    (incf score-sum (aref board row col)))))
    (* score-sum last-num)))


(defun solve-one () 
  "solve part 1 day 4"
  (let ((first-solve-data (find-first-solve)))
    (format t "First solve data: ~a~%" first-solve-data)
    (score (first first-solve-data) (second first-solve-data))))

(solve-one)


(defun non-win-indexes ()
  (let ((non-win-boards (make-array 0 :fill-pointer 0 :adjustable t)))
    (loop for i from 0 to (- (length *inp-boards*) 1)
          when (not (check-for-win i))
          do (vector-push-extend i non-win-boards))
    non-win-boards))



(defun find-last-solve ()
  (loop for i from 0 to (- (length *inp-nums*) 1) do
        (let ((num (aref *inp-nums* i)))
          (mark-all-boards num)
          (let ((remaining-non-wins (non-win-indexes)))
            (format t "Step: ~d, Num: ~d, Remaining Non-Wins: ~a~%" i num remaining-non-wins)
            (when (= 1 (length remaining-non-wins))
              ;; We continue until the last board wins, so after this, we need to mark until this last board wins.
              (let ((last-non-winner (aref remaining-non-wins 0)))
                (when (null last-non-winner)
                  (error "Error: Last non-winner is NIL. Remaining non-wins: ~a" remaining-non-wins))
                (loop for j from i to (- (length *inp-nums*) 1) do
                      (let ((next-num (aref *inp-nums* j)))
                        (mark-board next-num last-non-winner)
                        (when (check-for-win last-non-winner)
                          (return-from find-last-solve (list last-non-winner next-num)))))))))))


(defun solve-two ()
  "solve part 2 day 4"
  (let ((last-winner (find-last-solve)))
    (format t "Last solve data: ~a~%" last-winner)
    (score (first last-winner) (second last-winner))))

*check-boards*

(solve-two)
