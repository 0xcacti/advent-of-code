(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/24/"))
         (instructions '()))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
            (push (cl-ppcre:split " " line) instructions)))
    (reverse instructions)))

(read-input t)

(defstruct alu
  w
  x 
  y 
  z)

(defun init-alu (x y z w) 
  (make-alu :x x :y y :z z :w w))

(defun alu-get-register-value (alu reg)
  (cond 
    ((string= reg "x") (alu-x alu))
    ((string= reg "y") (alu-y alu))
    ((string= reg "z") (alu-z alu))
    ((string= reg "w") (alu-w alu))))

(defun alu-set-register-value (alu reg value)
  "Set an integer value to a register in the ALU"
  (check-type value integer)
  (cond 
    ((string= reg "x") (setf (alu-x alu) value))
    ((string= reg "y") (setf (alu-y alu) value))
    ((string= reg "z") (setf (alu-z alu) value))
    ((string= reg "w") (setf (alu-w alu) value)))
  (ecase reg
    ("x" (setf (alu-x alu) value))
    ("y" (setf (alu-y alu) value))
    ("z" (setf (alu-z alu) value))
    ("w" (setf (alu-w alu)))))


(defun alu-parse-value (alu val)
  (cond 
    ((stringp val)
     (cond 
       ((string= val "x") (alu-x alu))
       ((string= val "y") (alu-y alu))
       ((string= val "z") (alu-z alu))
       ((string= val "w") (alu-w alu))
       (t (parse-integer val :junk-allowed t :start 0))))
    ((numberp val) val)  ; If VAL is already a number, use it directly
    (t (error "Invalid value for ALU parsing: ~A" val))))


(defparameter *alu* (init-alu 1 2 3 4))
(alu-get-register-value *alu* "x")
(alu-set-register-value *alu* "x" 10)
(alu-get-register-value *alu* "x")

(defun alu-add (alu reg val)
  (setf val (alu-parse-value alu val))
  (let* ((reg-val (alu-get-register-value alu reg))
         (new-val (+ reg-val val)))
    (alu-set-register-value alu reg new-val)))

(alu-add *alu* "x" 10)
(alu-get-register-value *alu* "x")

(defun alu-mul (alu reg val)
  (setf val (alu-parse-value alu val))
  (let* ((reg-val (alu-get-register-value alu reg))
         (new-val (* reg-val val)))
    (alu-set-register-value alu reg new-val)))

(alu-mul *alu* "x" 10)
(alu-get-register-value *alu* "x")

(defun alu-mod (alu reg val)
  (setf val (alu-parse-value alu val))
  (let* ((reg-val (alu-get-register-value alu reg))
         (new-val (mod reg-val val)))
    (alu-set-register-value alu reg new-val)))

(alu-mod *alu* "x" 10)
(alu-get-register-value *alu* "x")

(defun alu-div (alu reg val)
  (setf val (alu-parse-value alu val))
  (check-type val number)
  (assert (/= val 0)
          (val)
          "Cannot divide by zero.")
  (let* ((reg-val (alu-get-register-value alu reg)))
    (multiple-value-bind (quotient remainder)
        (truncate reg-val val)
      (alu-set-register-value alu reg quotient))))

(alu-set-register-value *alu* "x" 10)
(alu-get-register-value *alu* "x")
(alu-div *alu* "x" 2)
(alu-get-register-value *alu* "x")

(defun alu-eq (alu reg val)
  (setf val (alu-parse-value alu val))
  (let* ((reg-val (alu-get-register-value alu reg))
         (new-val (if (= reg-val val) 1 0)))
    (alu-set-register-value alu reg new-val)))

(alu-set-register-value *alu* "x" 10)
(alu-get-register-value *alu* "x")
(alu-eq *alu* "x" 10)
(alu-get-register-value *alu* "x")
(alu-eq *alu* "x" 11)
(alu-get-register-value *alu* "x")




(defun get-block-params (cmds block-index)
  "Get the three important parameters from a block of instructions"
  (let ((base-idx (* block-index 18)))
    (list 
     (parse-integer (third (nth (+ base-idx 4) cmds)))   ; div
     (parse-integer (third (nth (+ base-idx 5) cmds)))   ; chk
     (parse-integer (third (nth (+ base-idx 15) cmds))))))  ; add

(defun solve (initial-digits cmds)
  (let ((digits (copy-seq initial-digits))  ; Make a copy to modify
        (stack nil))
    
    ;; Process each digit position
    (dotimes (i 14)
      (destructuring-bind (div chk add) (get-block-params cmds i)
        (if (= div 1)
            ;; Push operation
            (push (cons i add) stack)
            ;; Pop operation
            (let* ((prev (pop stack))
                   (j (car prev))
                   (prev-add (cdr prev)))
              ;; Calculate new digit based on relationship
              (setf (aref digits i) 
                    (+ (aref digits j) prev-add chk))
              
              ;; Adjust if result is too high
              (when (> (aref digits i) 9)
                (decf (aref digits j) 
                      (- (aref digits i) 9))
                (setf (aref digits i) 9))
              
              ;; Adjust if result is too low
              (when (< (aref digits i) 1)
                (incf (aref digits j)
                      (- 1 (aref digits i)))
                (setf (aref digits i) 1))))))
    
    ;; Convert result to string
    (map 'string #'digit-char digits)))

(defun solve-one ()
  "Solve part one day 24"
  (let ((cmds (read-input nil)))
    ;; Part 1: Start with all 9s
    (format t "Part 1: ~A~%" 
            (solve (make-array 14 :initial-element 9) cmds))))

(solve-one)

(defun solve-two ()
  "Solve part two day 24"
  (let ((cmds (read-input nil)))
    ;; Part 2: Start with all 1s
    (format t "Part 2: ~A~%" 
            (solve (make-array 14 :initial-element 1) cmds))))


(solve-two)
