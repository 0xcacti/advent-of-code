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
         (todo '())  ;; This will hold paths as lists of caves
         (all-paths '()))
    (push '("start") todo)  ;; Push a list containing "start"
    (loop while todo
          do (let* ((path (pop todo))  ;; Pop the path (which is a list of caves)
                    (current (elt path (1- (length path)))))  ;; Get the last cave in the path using elt
               (format t "path: ~a~%" path)
               (when (string= current "end")
                 (push path all-paths))  ;; Add to the all-paths if we've reached the end
               (loop for next in (gethash current caves)
                     when (or (not (is-lower next)) (not (member next path)))
                     do (push (append path (list next)) todo)))))
    all-paths))  ;; Return the list of all paths

(solve-one)
