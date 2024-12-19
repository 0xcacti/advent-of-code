(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/11
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/11)

(defun read-input (is-test)
    (with-input (path :test is-test)
      (let ((stones '()))
      (with-open-file (stream path :direction :input)
            (loop for line = (read-line stream nil)
              while line do
              (let ((parts (cl-ppcre:split " " line)))
                (loop for part in parts do 
                  (push (parse-integer part) stones)))))
        (reverse stones))))

(read-input t)

(defun run-blinks (stones)
  (let ((i 0))
  (loop while (< i (length stones)) 
        for stone = (nth i stones)
        do
    (cond 
      ((= stone 0)
       (setf (nth i stones) 1)
       (incf i))
      ((evenp (length (write-to-string stone)))
       (let* ((num-str (write-to-string stone))
              (half-len (/ (length num-str) 2))
              (first-half (parse-integer (subseq num-str 0 half-len)))
              (second-half (parse-integer (subseq num-str half-len)))
              (before (subseq stones 0 (1+ i)))
              (after (subseq stones (1+ i))))
         (setf stones (append before (list second-half) after))
         (setf (nth i stones) first-half))
         (incf i 2))
      (t 
       (setf (nth i stones) (* stone 2024))
       (incf i)))))
    stones)

(defun solve-one ()
  "Solve part one day 11" 
  (let ((stones (read-input nil)))
    (loop for i from 0 below 25 do
    (setf stones (run-blinks stones)))
    (format t "stone count: ~a~%" (length stones))))

(solve-one)


(defparameter *cache* (make-hash-table :test 'equal))

(defun turbo-blinks (stone steps)
  (let ((key (cons stone steps)))  ; Create a key from both inputs
    (or (gethash key *cache*)      ; Check if result is cached
        (setf (gethash key *cache*) ; If not, compute and cache result
              (cond 
                ((= steps 0) 1)
                ((= stone 0)
                 (turbo-blinks 1 (1- steps)))
                ((evenp (length (write-to-string stone)))
                 (let* ((num-str (write-to-string stone))
                        (half-len (/ (length num-str) 2))
                        (first-half (parse-integer (subseq num-str 0 half-len)))
                        (second-half (parse-integer (subseq num-str half-len))))
                   (+ (turbo-blinks first-half (1- steps))
                      (turbo-blinks second-half (1- steps)))))
                (t 
                 (turbo-blinks (* stone 2024) (1- steps))))))))

(defun solve-two ()
  "Solve part two day 11"
  (let ((stones (read-input nil))
        (ans 0))

    (setf ans (loop for stone in stones sum 
        (turbo-blinks stone 75)))
    ans))

(solve-two)
