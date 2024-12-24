(ql:quickload :advent-of-code)

(in-package :cl-user)

(defpackage :advent-of-code/2024/17
  (:use :cl :advent-of-code/utils))

(in-package :advent-of-code/2024/17)

(defun read-input (is-test)
    (with-input (path :test is-test)
      (with-open-file (stream path :direction :input)
        (let ((a 0)
              (b 0)
              (c 0)
              (program (make-array 0 :fill-pointer 0 :adjustable t)))
          (loop for line = (read-line stream nil nil)
                for i from 0 
                while line
                do 
                (cond ((search "A" line)
                       (setf a (parse-integer (subseq line 12))))
                      ((search "B" line)
                       (setf b (parse-integer (subseq line 12))))
                      ((search "C" line)
                       (setf c (parse-integer (subseq line 12))))
                      ((search "Program" line)
                       (setf program (mapcar #'parse-integer (cl-ppcre:split "," (subseq line 9)))))))
          (values a b c program)))))


(defun combo (operand a b c)
  (cond ((and (<= 0 operand) (<= operand 3))
         (return-from combo operand))
        ((= operand 4)
         (return-from combo a))
        ((= operand 5)
         (return-from combo b))
        ((= operand 6)
         (return-from combo c))
        (t (error "Invalid operand: ~a" operand))))


(defun solve-one () 
  "Solve part one day 17"
  (multiple-value-bind (a b c program) (read-input nil)
    (let ((pointer 0)
          (output nil)
          (prog-array (coerce program 'vector)))
      
      (loop while (< pointer (length prog-array))
            do (let* ((ins (aref prog-array pointer))
                     (operand (aref prog-array (1+ pointer)))
                     (increment-pointer t))  ; Add flag to control pointer increment
                 
                 (case ins
                   (0  ; adv
                    (setf a (ash a (- (combo operand a b c)))))
                   (1  ; bxl
                    (setf b (logxor b operand)))
                   (2  ; bst
                    (setf b (mod (combo operand a b c) 8)))
                   (3  ; jnz
                    (when (not (= a 0))
                      (setf pointer operand)
                      (setf increment-pointer nil)))  ; Don't increment pointer after jump
                   (4  ; bxc
                    (setf b (logxor b c)))
                   (5  ; out
                    (push (mod (combo operand a b c) 8) output))
                   (6  ; bdv
                    (setf b (ash a (- (combo operand a b c)))))
                   (7  ; cdv
                    (setf c (ash a (- (combo operand a b c))))))
                 
                 ;; Only increment pointer if we didn't jump
                 (when increment-pointer
                   (incf pointer 2))))
      
      ;; Return output as comma-separated string
      (format nil "~{~a~^,~}" (reverse output)))))

(solve-one)


(defun find-answer (target ans program)
  "Recursively find the answer that generates the target sequence"
  (when (null target)
    (return-from find-answer ans))
  
  (loop for val from 0 below 8
        do (let* ((a (logior (ash ans 3) val))
                  (b 0)
                  (c 0)
                  (output nil)
                  (adv3 nil))
             
             (flet ((combo (operand)
                      (cond ((and (<= 0 operand) (<= operand 3))
                             operand)
                            ((= operand 4) a)
                            ((= operand 5) b)
                            ((= operand 6) c)
                            (t (error "Invalid operand: ~a" operand)))))
               
               (block process-program
                 (loop for pointer from 0 below (- (length program) 2) by 2
                       do (let ((ins (elt program pointer))
                               (operand (elt program (1+ pointer))))
                            (case ins
                              (0 ; adv
                               (when adv3 
                                 (error "Program has multiple ADVs"))
                               (unless (= operand 3)
                                 (error "Program has ADV with operand other than 3"))
                               (setf adv3 t))
                              (1 ; bxl
                               (setf b (logxor b operand)))
                              (2 ; bst
                               (setf b (mod (combo operand) 8)))
                              (3 ; jnz
                               (error "Program has JNZ inside expected loop body"))
                              (4 ; bxc
                               (setf b (logxor b c)))
                              (5 ; out
                               (when output
                                 (error "Program has multiple OUT"))
                               (setf output (mod (combo operand) 8)))
                              (6 ; bdv
                               (setf b (ash a (- (combo operand)))))
                              (7 ; cdv
                               (setf c (ash a (- (combo operand))))))
                            
                            (when (and output (= output (car (last target))))
                              (let ((sub (find-answer (butlast target) a program)))
                                (when sub
                                  (return-from find-answer sub)))
                              (return-from process-program nil)))))))))

(defun solve-two ()
  "Solve part two day 17"
  (multiple-value-bind (a b c program) (read-input nil)
    (let ((len (length program)))
      (unless (and (= (nth (- len 2) program) 3)
                   (= (nth (- len 1) program) 0))
        (error "Program does not end with JNZ 0")))
    (find-answer program 0 program)))

(solve-two)
