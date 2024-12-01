(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/25/"))
         (lines '())
         (rows 0)
         (cols 0)
         (scc (make-hash-table :test 'equal)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (push line lines)))
      (setf lines (reverse lines))
      (setf rows (length lines))
      (setf cols (length (first lines)))
      (loop for line in lines 
            for row from 0 
            while line do 
            (loop for char across line 
                  for col from 0 do 
                  (if (not (char= char #\.))
                      (setf (gethash (list row col) scc) char))))
  (values scc rows cols)))

(read-input t)


(defun print-map (scc rows cols)
  (loop for row from 0 below rows
        do (progn
             (loop for col from 0 below cols
                   do (format t "~a" 
                            (if (gethash (list row col) scc)
                                (gethash (list row col) scc)
                                #\.)))
             (format t "~%"))))

;; (defun make-set (&rest elements)
;;   (remove-duplicates elements :test #'equal))
;; 
;; (defun set-add (set element) 
;;   (if (member element set :test #'equal)
;;       set
;;       (cons element set)))
;; 
;; (defun set-remove (set element)
;;   (remove element set :test #'equal))
;; 
;; (defun set-member-p (set element)
;;   (member element set :test #'equal))
;; 
;; (defun set-union (set1 set2)
;;   (remove-duplicates (append set1 set2) :test #'equal))

(defun calculate-steps (scc rows cols)
  (let ((steps 0)
        (should-continue t))
    (loop while should-continue do 
        (incf steps)
        (let  ((east-changes '())
               (south-changes '())
               (east-deletes '())
               (south-deletes '()))

          (loop for key being the hash-keys of scc 
                using (hash-value value)
                when (char= value #\>)
                do 
                (let ((r (first key))
                      (c (second key)))
                    (unless (gethash (list r (mod (+ c 1) cols)) scc)
                      (push (list r (mod (+ c 1) cols)) east-changes)
                      (push (list r c) east-deletes))))
          (when east-deletes
              (dolist (del east-deletes)
                (remhash del scc))
              (dolist (change east-changes)
                (setf (gethash change scc) #\>)))

          (loop for key being the hash-keys of scc 
                using (hash-value value)
                when (char= value #\v)
                do 
                (let ((r (first key))
                      (c (second key)))
                    (unless (gethash (list (mod (+ r 1) rows) c) scc)
                      (push (list (mod (+ r 1) rows) c) south-changes)
                      (push (list r c) south-deletes))))
          (when south-deletes
            (dolist (delete south-deletes)
              (remhash delete scc))
            (dolist (change south-changes)
              (setf (gethash change scc) #\v)))

          (when (and 
                  (null east-changes)
                  (null south-changes))
            (setf should-continue nil))))
    steps))



(defun solve-one ()
  "Solve day 25, part one"
  (multiple-value-bind (scc rows cols) (read-input nil)
    (print-map scc rows cols)
    (format t "Rows: ~a~%" rows)
    (format t "Cols: ~a~%" cols)
  (calculate-steps scc rows cols)))



(solve-one)
