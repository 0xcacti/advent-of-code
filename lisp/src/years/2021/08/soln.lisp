(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/8/"))
         (signal-patterns nil)
         (output nil))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let* ((parts (cl-ppcre:split " \\| " line))) 
              (push (cl-ppcre:split " " (first parts)) signal-patterns)
              (push (cl-ppcre:split " " (second parts)) output))))
      (list (reverse signal-patterns) (reverse output))))



(defun solve-one () 
  "Solve part one day eight"
  (let ((is-test nil) (input nil) (signal-patterns nil) (output nil) (c 0))
    (setf input (read-input is-test))
    (format t "input: ~a~%" input)
    (setf signal-patterns (first input))
    (setf output (second input))
    (format t "signal-patterns: ~a~%" signal-patterns)
    (format t "output: ~a~%" output)
    (loop for out in output do 
          (loop for o in out do 
        (format t "out: ~a~%" out)
        (let ((l (length o)))
          (when (or (= l 2) (= l 3) (= l 4) (= l 7))
            (incf c)))))
    (format t "c: ~a~%" c)))

;; 0: 6
;; 6: 6
;; 9: 6

;; 2: 5
;; 3: 5
;; 5: 5

;; 1: 2 
;; 4: 4
;; 7: 3
;; 8: 7

;; a: (len(3) - len(2)) 
;; b: (len(4) - len(2)) 
;; c: in(len2)) only in 2 of len(6) 
;; d: not C, in(len(4)), only in 2 of len(6)
;; e: missing from len(6) and not(C) and not(d) 
;; f: only missing from 1 digit
;; g: what's left


(defun normalize (s) 
  "sorts characters in a string" 
    (coerce (sort (coerce s 'list) #'char<) 'string))

(defun deduce (digits)
  "takes a list of strings"
  (let ((len2 "") (len3 "") (len4 "") (len7 "") (len5 nil) (len6 nil) (digit-values (make-hash-table :test 'equal)) (digit6 ""))
    (loop for d in digits do 
      (cond 
        ((= (length d) 2) (setf len2 (normalize d)))
        ((= (length d) 3) (setf len3 (normalize d)))
        ((= (length d) 4) (setf len4 (normalize d)))
        ((= (length d) 7) (setf len7 (normalize d)))
        ((= (length d) 5) (push (normalize d) len5))
        (t (push (normalize d) len6))))

    ;; find 6: missing a letter from 1 
    ;; for each value in len6 
    (loop for l in len6 do 
        (when (not (every (lambda (c) (find c l)) len2))
          (setf (gethash l digit-values) 6)
          (setf digit6 l)
          (setf len6 (remove l len6))))

    (loop for l in len6 do 
          (when (not (every (lambda (c) (find c l)) len4))
            (setf (gethash l digit-values) 0)
            (setf len6 (remove l len6))))

    (setf (gethash (car len6) digit-values) 9)

    (loop for l in len5 do 
          (when (every (lambda (c) (find c l)) len2)
            (setf (gethash l digit-values) 3)
            (setf len5 (remove l len5))))



    (loop for l in len5 do 
          (when (every (lambda (c) (find c digit6)) l)
            (setf (gethash l digit-values) 5)
            (setf len5 (remove l len5))))

    (setf (gethash (car len5) digit-values) 2)


    (setf (gethash len2 digit-values) 1)
    (setf (gethash len4 digit-values) 4)
    (setf (gethash len3 digit-values) 7)
    (setf (gethash len7 digit-values) 8)

    digit-values))


(defun solve-two () 
  "Solve part two day eight"
  (let ((is-test nil) (input nil) (signal-patterns nil) (output nil) (sum 0))
    (setf input (read-input is-test))
    (setf signal-patterns (first input))
    (setf output (second input))
    (format t "signal-patterns: ~a~%" signal-patterns)
    (format t "output: ~a~%" output)
    (format t "length: ~a~%" (length signal-patterns))
    (loop for i from 0 to (- (length signal-patterns) 1) do 
        (format t "i: ~a~%" i)
        (let ((deduced-mapping (deduce (nth i signal-patterns))) (num-str ""))
          (loop for out in (nth i output) do 
            (let ((normalized-out (normalize out)))
              (setf num-str (concatenate 'string num-str (format nil "~a" (gethash normalized-out deduced-mapping))))))
          (incf sum (parse-integer num-str))))
    (format t "sum: ~a~%" sum)))




(solve-two)
