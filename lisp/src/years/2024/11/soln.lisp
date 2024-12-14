(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/11
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/11)

(defun read-input (is-test)
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (let* ((first-line (read-line stream nil))
               (width (length first-line))
               (height 1))
          (loop while (read-line stream nil) 
                do (incf height))
          (file-position stream 0)
          (let ((grid (make-array (list height width))))
            (loop for r below height 
                  for line = (read-line stream)
                  do (loop for c below width 
                           for char = (char line c)
                           do (setf (aref grid r c) (digit-char-p char))))
            grid)))))

(read-input t)

