(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/24
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/24)

(defun read-input (is-test)
  (with-input (path :test is-test)
    (let ((known (make-hash-table :test 'equal))
          (formulas (make-hash-table :test 'equal))
          (found nil))
      (with-open-file (stream path :direction :input)
        (loop for line = (read-line stream nil nil)
              while line do
              (cond 
                ((string= line "") (setf found t))
                ((not found) 
                 (let ((parts (cl-ppcre:split ": " line)))
                   (setf (gethash (first parts) known) (parse-integer (second parts)))))
                (found 
                  (let* ((parts (cl-ppcre:split " -> " line))
                         (k (second parts))
                         (sub-parts (cl-ppcre:split " " (first parts))))
                    (setf (gethash k formulas) (list (nth 1 sub-parts) (nth 0 sub-parts) (nth 2 sub-parts))))))))
      (values known formulas))))               

(read-input t)

(defun print-map (m)
  (maphash (lambda (k v) (format t "~a: ~a~%" k v)) m))

(defun calc (wire known formulas operators) 
  (cond 
    ((gethash wire known) 
     (let ((val (gethash wire known)))
       (format t "Found known ~a = ~a~%" wire val)
       val))
    (t 
     (let* ((triplet (gethash wire formulas))
            (op (first triplet))
            (x (second triplet))
            (y (third triplet)))
       (format t "Calculating ~a = (~a ~a ~a)~%" wire op x y)
       (let ((x-val (calc x known formulas operators))
             (y-val (calc y known formulas operators)))
         (format t "Got values ~a=~a, ~a=~a~%" x x-val y y-val)
         (let ((result (funcall (gethash op operators) x-val y-val)))
           (format t "~a ~a ~a = ~a~%" x-val op y-val result)
           (setf (gethash wire known) result)
           result))))))


(defun solve-one ()
  (multiple-value-bind (known formulas) (read-input t)
    (let ((operators (make-hash-table :test 'equal))
          (z-values '()))
      (setf (gethash "AND" operators) #'logand)
      (setf (gethash "OR" operators) #'logior)
      (setf (gethash "XOR" operators) #'logxor)
      
      (loop for i from 0
            for key = (format nil "z~2,'0d" i)
            while (gethash key formulas)
            do (setf z-values (append z-values (list (calc key known formulas operators)))))
      
      (values (parse-integer (format nil "~{~d~}" (reverse z-values)) :radix 2)))))

(solve-one)

(defun make-wire (c num)
  (concatenate 'string c (format nil "~2,'0d" num)))

(defun verify-intermediate-xor (wire num formulas)
  (if (not (gethash wire formulas)) nil)
  (let* ((triplet (gethash wire formulas))
         (op (first triplet))
         (x (second triplet))
         (y (third triplet)))
    (if (not (string= op "XOR")) nil)
    (equal (sort (list x y) #'string<) 
          (list (make-wire "x" num) (make-wire "y" num)))))

(defun verify-carry-bit (wire num formulas)
  (if (not (gethash wire formulas)) nil)
  (let* ((triplet (gethash wire formulas))
         (op (first triplet))
         (x (second triplet))
         (y (third triplet)))
    (if (= num 1)
        (and (string= op "AND")
             (equal (sort (list x y) #'string<)
                   (list "x00" "y00")))
        (and (string= op "OR")
             (or (and (verify-direct-carry x (- num 1) formulas)
                     (verify-recarry y (- num 1) formulas))
                 (and (verify-direct-carry y (- num 1) formulas)
                     (verify-recarry x (- num 1) formulas)))))))

(defun verify-direct-carry (wire num formulas)
  (if (not (gethash wire formulas)) nil)
  (let* ((triplet (gethash wire formulas))
         (op (first triplet))
         (x (second triplet))
         (y (third triplet)))
    (and (string= op "AND")
         (equal (sort (list x y) #'string<)
               (list (make-wire "x" num) (make-wire "y" num))))))

(defun verify-recarry (wire num formulas)
  (if (not (gethash wire formulas)) nil)
  (let* ((triplet (gethash wire formulas))
         (op (first triplet))
         (x (second triplet))
         (y (third triplet)))
    (and (string= op "AND")
         (or (and (verify-intermediate-xor x num formulas)
                 (verify-carry-bit y num formulas))
             (and (verify-intermediate-xor y num formulas)
                 (verify-carry-bit x num formulas))))))

(defun verify-z (wire num formulas)
  (if (not (gethash wire formulas)) nil)
  (let* ((triplet (gethash wire formulas))
         (op (first triplet))
         (x (second triplet))
         (y (third triplet)))
    (if (not (string= op "XOR")) nil)
    (if (= num 0) 
        (equal (sort (list x y) #'string<) 
              (list "x00" "y00"))
        (or (and (verify-intermediate-xor x num formulas) (verify-carry-bit y num formulas))
            (and (verify-intermediate-xor y num formulas) (verify-carry-bit x num formulas))))))

(defun verify (num formulas) 
  (verify-z (make-wire "z" num) num formulas))

(defun progress (formulas)
  (let ((i 0))
    (loop while t do 
        (if (not (verify i formulas)) (return i))
        (incf i))))

(defun solve-two ()
  "Solve part two day 24"
  (multiple-value-bind (known formulas) (read-input nil)
    (let ((swaps '()))
      (loop for i from 0 below 4 do 
        (let ((baseline (progress formulas)))
          (block outer
            (loop for x being the hash-keys of formulas do 
              (loop for y being the hash-keys of formulas do 
                (unless (string= x y)
                  (let ((temp-x (gethash x formulas))
                        (temp-y (gethash y formulas)))
                    (setf (gethash x formulas) temp-y
                          (gethash y formulas) temp-x)
                    (when (> (progress formulas) baseline)
                      (push x swaps)
                      (push y swaps)
                      (return-from outer))
                    (setf (gethash x formulas) temp-x
                          (gethash y formulas) temp-y))))))))
      (format nil "~{~a~^,~}" (sort swaps #'string<)))))

(solve-two)
