(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/11/"))
         (inp (make-array 0 :adjustable t :fill-pointer 0))
         (octopuses (make-hash-table :test 'equal)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (let ((line-array (map 'vector 
                                    (lambda (c) (- (char-code c) (char-code #\0))) 
                                    line)))
              (vector-push-extend line-array inp))))
    (loop for r below (length inp) do 
          (loop for c below (length (aref inp 0)) do
                (setf (gethash (list r c) octopuses) (aref (aref inp r) c))))
    octopuses))


(read-input t)

(defun print-octopuses (octopuses) 
  (loop for r below 10 do
        (loop for c below 10 do
              (format t "~a " (gethash (list r c) octopuses)))
        (format t "~%")))



(defun adjacent-points (point) 
  (let ((r (first point))
        (c (second point)))
    (list (list (- r 1) c)     ;; Up
          (list (+ r 1) c)     ;; Down
          (list r (- c 1))     ;; Left
          (list r (+ c 1))     ;; Right
          (list (- r 1) (- c 1)) ;; Top-left
          (list (- r 1) (+ c 1)) ;; Top-right
          (list (+ r 1) (- c 1)) ;; Bottom-left
          (list (+ r 1) (+ c 1)) ;; Bottom-right
          )))



(defun solve-one () 
  (let ((octopuses (read-input nil))
        (num-flashes 0))
    (loop for i below 100 do 
        (let ((todo '()))
      (loop for r below 10 do 
            (loop for c below 10 do 
                  (let ((current (gethash (list r c) octopuses)))
                    (setf (gethash (list r c) octopuses) (+ 1 current)))
                  (when ( > (gethash (list r c) octopuses) 9)
                    (push (list r c) todo))
                  ))
      (loop while todo do 
            (let ((point (pop todo)))
              (when (not (zerop (gethash point octopuses)))
                (setf (gethash point octopuses) 0)
                (incf num-flashes 1)
            (loop for adj in (adjacent-points point) 
                when (and (gethash adj octopuses)
                          (not (zerop (gethash adj octopuses))))
                    do 
                    (setf (gethash adj octopuses) (+ 1 (gethash adj octopuses)))
                    (when ( > (gethash adj octopuses) 9)
                      (push adj todo))))))))

    num-flashes))



(solve-one)


(defun all-flashed (octopuses)
  (loop for r below 10 do 
        (loop for c below 10 do 
              (when (not (zerop (gethash (list r c) octopuses)))
                (return-from all-flashed nil))))
  t)

(defun solve-two () 
  (let ((octopuses (read-input nil))
        (all-flashed nil)
        (step-count 0))
    (loop while (not all-flashed) do
        (incf step-count)
        (let ((todo '()))
      (loop for r below 10 do 
            (loop for c below 10 do 
                  (let ((current (gethash (list r c) octopuses)))
                    (setf (gethash (list r c) octopuses) (+ 1 current)))
                  (when ( > (gethash (list r c) octopuses) 9)
                    (push (list r c) todo))
                  ))
      (loop while todo do 
            (let ((point (pop todo)))
              (when (not (zerop (gethash point octopuses)))
                (setf (gethash point octopuses) 0)
            (loop for adj in (adjacent-points point) 
                when (and (gethash adj octopuses)
                          (not (zerop (gethash adj octopuses))))
                    do 
                    (setf (gethash adj octopuses) (+ 1 (gethash adj octopuses)))
                    (when ( > (gethash adj octopuses) 9)
                      (push adj todo)))))))
        (when (all-flashed octopuses)
          (setf all-flashed t)))

    step-count))

(solve-two)
