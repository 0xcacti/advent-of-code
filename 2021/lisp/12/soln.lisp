(ql:quickload "cl-ppcre")

;; use dict that maps node to all the nodes it can go to 
;; key is start and set is all the possible destinations

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/12/"))
         (caves (make-hash-table :test 'equal)))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
              (let ((line-array (cl-ppcre:split "-" line)))
              (setf (gethash (first line-array) caves) (second line-array))
              (setf (gethash (second line-array) caves) (first line-array)))))
    caves))

(read-input t)

(defun print-map(caves) 
    (maphash (lambda (k v) (format t "~a->~a~%" k v)) caves))

(print-map (read-input t))

(defun solve-one () 
  "Solution to day 12, part one" 
  (let* ((caves (read-input t))
         (todo '())
         (all-paths '()))
    (push "start" todo)
    (loop while todo 
          (let ((path (pop todo)))
          (when (string= (last path) "end")
            (push path all-paths))


  ))
