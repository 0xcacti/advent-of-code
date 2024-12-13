(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/09
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/09)

(defun read-input (is-test)
  (let ((elements (make-array 0 :adjustable t :fill-pointer 0)))
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil)
              while line
              do 
              (loop for element in (cl-ppcre:split "" line)
                    do (vector-push-extend element elements)))))
    elements))

(read-input t)

(defun make-storage-map (elements)
  (let ((storage (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for element across elements 
          for i from 0 do 
          (cond ((evenp i) 
                 (loop for j from 0 below (parse-integer element) do 
                  (vector-push-extend (princ-to-string (/ i 2)) storage)))
                ((oddp i)
                  (loop for j from 0 below (parse-integer element) do
                    (vector-push-extend "." storage)))))
    storage))

(defun swap-elements (storage p1 p2)
  (let ((temp (aref storage p1)))
    (setf (aref storage p1) (aref storage p2))
    (setf (aref storage p2) temp))
  storage)

(defun remap (storage) 
  (let ((p1 0)
        (p2 (1- (length storage))))
    (loop while (< p1 p2) do
        (loop while (not (equal (aref storage p1) ".")) do 
          (incf p1))
        (loop while (equal (aref storage p2) ".") do 
          (decf p2))
        (when (< p1 p2)
          (setf storage (swap-elements storage p1 p2))))
  storage))

(defun count-up (storage)
  (let ((count 0)
        (counter 0))
    (loop for element across storage do 
          (when (not (equal element "."))
            (incf count (* (parse-integer element) counter)))
          (incf counter))
    count))

(defun solve-one () 
  "Solve part one day 9"
  (let* ((elements (read-input t))
         (storage (make-storage-map elements))
         (remapped (remap storage))
         (result (count-up remapped)))
    (format t "Storage: ~a~%" storage)
    (format t "Result: ~a~%" result)))

(solve-one)


(defun find-all-blocks (storage) 
  (let ((i 0)
        (blocks 0))
  (loop while (< i (length storage)) do
        (if (not (equal (aref storage i) "."))
          (let ((c (aref storage i)))
            (incf blocks)
            (loop while (equal (aref storage i) c) do 
              (incf i)))
          (incf i)
          ))
  blocks))


(defun count-consecutive (storage start id)
  (let ((count 0)
        (pos start))
    (loop while (and (< pos (length storage))
                     (equal (aref storage pos) (write-to-string id)))
          do
          (incf count)
          (incf pos))
    count))

(defun count-consecutive-dots (storage start)
  (let ((count 0)
        (pos start))
    (loop while (and (< pos (length storage))
                     (equal (aref storage pos) "."))
          do
          (incf count)
          (incf pos))
    count))

(defun move-file (storage start len target)
  (let ((file-contents (make-array len)))
    (loop for i from 0 below len do
          (setf (aref file-contents i) (aref storage (+ start i))))
    (loop for i from 0 below len do
          (setf (aref storage (+ start i)) "."))
    (loop for i from 0 below len do
          (setf (aref storage (+ target i)) (aref file-contents i)))
    storage))

(defun block-remap (storage blocks)
  (let ((p1 0)
        (p2 (1- (length storage)))
        (counter  0))
    (loop for current-id from (1- blocks) downto 0 do 
          (multiple-value-bind (start len) (find-file-position storage current-id)
            (when start 
              (let ((target-pos (find-leftmost-space storage len)))
                (when (and target-pos (< target-pos start))
                  (move-file storage start len target-pos))))))
  storage))

(defun find-file-position (storage id)
  (let ((pos 0))
    (loop while (< pos (length storage)) do
      (when (equal (aref storage pos) (write-to-string id))
        (let ((len (count-consecutive storage pos id)))
          (return-from find-file-position (values pos len))))
      (incf pos))
    (values nil nil)))

(defun find-leftmost-space (storage needed-len)
  (let ((pos 0))
    (loop while (< pos (length storage)) do
      (when (equal (aref storage pos) ".")
        (let ((space-len (count-consecutive-dots storage pos)))
          (when (>= space-len needed-len)
            (return-from find-leftmost-space pos))))
      (incf pos))
    nil))

(defun solve-two () 
  "Solve part two day 9"
  (let* ((elements (read-input nil))
         (storage (make-storage-map elements))
         (blocks (find-all-blocks storage))
         (remapped (block-remap storage blocks))
         (result (count-up remapped))
         )
    (format t "Result: ~a~%" result)))

(solve-two)
