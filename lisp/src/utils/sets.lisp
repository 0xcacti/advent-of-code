(in-package :advent-of-code/utils)

(defstruct (set (:constructor make-set))
  (storage (make-hash-table :test #'equal)))

(defun add (s item)
  (setf (gethash item (set-storage s)) t)
  s)

(defun remove-item (s item)
  (remhash item (set-storage s))
  s)

(defun contains (s item)
  (nth-value 0 (gethash item (set-storage s))))

(defun items (s)
  (loop for key being the hash-keys of (set-storage s)
        collect key))

(defun size (s)
  (hash-table-count (set-storage s)))
