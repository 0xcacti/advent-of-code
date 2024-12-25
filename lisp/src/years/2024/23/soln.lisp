(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/23
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/23)

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((edges '()))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line do
              (push (cl-ppcre:split "-" line) edges)))
      (reverse edges))))

(read-input t)

(defun solve-one ()
  "Solve part one day 23"
  (let ((edges (read-input nil))
        (conns (make-hash-table :test #'equal))
        (sets (make-hash-table :test #'equal)))
    
    (loop for edge in edges do
          (let ((x (first edge))
                (y (second edge)))
            (unless (gethash x conns)
              (setf (gethash x conns) (make-hash-table :test #'equal)))
            (unless (gethash y conns)
              (setf (gethash y conns) (make-hash-table :test #'equal)))
            (setf (gethash y (gethash x conns)) t)
            (setf (gethash x (gethash y conns)) t)))
    
    (loop for x being the hash-keys of conns do
          (loop for y being the hash-keys of (gethash x conns) do
                (loop for z being the hash-keys of (gethash y conns) do
                      (when (and (not (string= x z))
                               (gethash x (gethash z conns)))
                        (let ((sorted-key (format nil "~{~A~^,~}" 
                                                (sort (list x y z) #'string<))))
                          (setf (gethash sorted-key sets) t))))))
    
    (loop for triangle being the hash-keys of sets
          count (let ((nodes (cl-ppcre:split "," triangle)))
                 (some (lambda (node) 
                        (char= (char node 0) #\t))
                      nodes)))))
(solve-one)

(defun make-set-key (req)
  (format nil "~{~A~^,~}" (sort (alexandria:hash-table-keys req) #'string<)))

(defun search-nodes (node req conns sets)
  (let ((key (make-set-key req)))
    (unless (gethash key sets)
      (setf (gethash key sets) t)
      ;; For each neighbor of current node
      (loop for neighbor being the hash-keys of (gethash node conns) do
            ;; Skip if neighbor already in req
            (unless (gethash neighbor req)
              ;; Check if neighbor is connected to all nodes in req
              (when (every (lambda (query)
                           (gethash neighbor (gethash query conns)))
                         (alexandria:hash-table-keys req))
                ;; Add neighbor to req and recurse
                (let ((new-req (alexandria:copy-hash-table req)))
                  (setf (gethash neighbor new-req) t)
                  (search-nodes neighbor new-req conns sets))))))))

(defun solve-two ()
  "Solve part two day 23"
  (let ((edges (read-input nil))
        (conns (make-hash-table :test #'equal))
        (sets (make-hash-table :test #'equal)))
    ;; Build connections
    (loop for edge in edges do
          (let ((x (first edge))
                (y (second edge)))
            (unless (gethash x conns)
              (setf (gethash x conns) (make-hash-table :test #'equal)))
            (unless (gethash y conns)
              (setf (gethash y conns) (make-hash-table :test #'equal)))
            (setf (gethash y (gethash x conns)) t)
            (setf (gethash x (gethash y conns)) t)))
    
    ;; Start search from each node
    (loop for x being the hash-keys of conns do
          (let ((initial-req (make-hash-table :test #'equal)))
            (setf (gethash x initial-req) t)
            (search-nodes x initial-req conns sets)))
    
    ;; Find the longest set and return it as comma-joined string
    (let ((longest-set nil)
          (max-length 0))
      (loop for set-key being the hash-keys of sets do
            (let ((current-length (length (cl-ppcre:split "," set-key))))
              (when (> current-length max-length)
                (setf max-length current-length)
                (setf longest-set set-key))))
      longest-set)))

(solve-two)
