(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/05
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/05)

(defun read-input (is-test)
  (with-input (path :test is-test)  
    (let ((pages '())
          (updates '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line do 
              (cond 
                ((cl-ppcre:scan "\\|" line)
                 (let ((parts (cl-ppcre:split "\\|" line)))
                   (push (mapcar #'parse-integer parts) pages)))
                ((cl-ppcre:scan "," line)
                 (let ((parts (cl-ppcre:split "," line)))
                   (push (mapcar #'parse-integer parts) updates))))))
    (values (reverse pages) (reverse updates)))))

(read-input t)

(defun get-relevant-pages (pages num)
  (remove-if-not (lambda (page)
                   (= (first page) num)) pages))

(defun ordered-p (update pages)
    (loop for num in update 
          for relevant-pages = (get-relevant-pages pages num)
          for rest-nums = (cdr (member num update))
          always
           (loop for rnum in rest-nums always
             (some (lambda (page) 
                          (= (second page) rnum))
                     relevant-pages))))


(multiple-value-bind (pages updates) (read-input t)
  (ordered-p (first updates) pages))

(defun solve-one () 
  "Solve day 5, part 1" 
  (multiple-value-bind (pages updates) (read-input nil)
    (setf updates (remove-if-not (lambda (update) 
                                   (ordered-p update pages)) updates))
    (loop for update in updates sum 
          (nth (floor (/ (length update) 2)) update))))

(solve-one)

(defun get-before-pages (num pages)
  (loop for (from to) in pages 
        when (= to num)
          collect from))

(defun get-after-pages (num pages)
  (loop for (from to) in pages 
        when (= from num)
        collect to))

(defun reorder-update (update pages)
  (let ((result nil))
    (loop while update do 
        (let* ((valid-next
                (loop for num in update 
                      when (every (lambda (before-page)
                                    (not (member before-page update)))
                            (get-before-pages num pages))
                      collect num))
               (next (car valid-next)))
          (push next result)
          (setf update (remove next update))))
    (reverse result)))


(defun solve-two ()
  "Solve day 5, part 2"
  (multiple-value-bind (pages updates) (read-input nil)
    (setf updates (remove-if (lambda (update) 
                                   (ordered-p update pages)) updates))
    (loop for update in updates 
          for reordered = (reorder-update update pages)
          sum (nth (floor (/ (length reordered) 2)) reordered))))

(solve-two)
