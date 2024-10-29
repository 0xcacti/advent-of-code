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

(defun alu-add (alu reg val)
  (cond ((string= val "x") (setf val (parse-integer (alu-x alu))))
        ((string= val "y") (setf val (parse-integer (alu-y alu))))
        ((string= val "z") (setf val (parse-integer (alu-z alu))))
        ((string= val "w") (setf val (parse-integer (alu-w alu))))
        (t (setf val (parse-integer reg))))
  (cond ((string= reg "x") (setf (alu-x alu) (+ (alu-x alu) val)))
        ((string= reg "y") (setf (alu-y alu) (+ (alu-y alu) val)))
        ((string= reg "z") (setf (alu-z alu) (+ (alu-z alu) val)))
        ((string= reg "w") (setf (alu-w alu) (+ (alu-w alu) val)))))

(defun alu-mul (alu reg val)
  (cond ((string= val "x") (setf val (parse-integer (alu-x alu))))
        ((string= val "y") (setf val (parse-integer (alu-y alu))))
        ((string= val "z") (setf val (parse-integer (alu-z alu))))
        ((string= val "w") (setf val (parse-integer (alu-w alu))))
        (t (setf val (parse-integer reg))))
  (cond ((string= reg "x") (setf (alu-x alu) (* (alu-x alu) val)))
        ((string= reg "y") (setf (alu-y alu) (* (alu-y alu) val)))
        ((string= reg "z") (setf (alu-z alu) (* (alu-z alu) val)))
        ((string= reg "w") (setf (alu-w alu) (* (alu-w alu) val)))))

(defun alu-mod (alu reg val)
  (cond ((string= val "x") (setf val (parse-integer (alu-x alu))))
        ((string= val "y") (setf val (parse-integer (alu-y alu))))
        ((string= val "z") (setf val (parse-integer (alu-z alu))))
        ((string= val "w") (setf val (parse-integer (alu-w alu))))
        (t (setf val (parse-integer reg))))
  (cond ((string= reg "x") (setf (alu-x alu) (mod (alu-x alu) val)))
        ((string= reg "y") (setf (alu-y alu) (mod (alu-y alu) val)))
        ((string= reg "z") (setf (alu-z alu) (mod (alu-z alu) val)))
        ((string= reg "w") (setf (alu-w alu) (mod (alu-w alu) val)))))

(defun alu-div (alu reg val) 
  (cond ((string= val "x") (setf val (parse-integer (alu-x alu))))
        ((string= val "y") (setf val (parse-integer (alu-y alu))))
        ((string= val "z") (setf val (parse-integer (alu-z alu))))
        ((string= val "w") (setf val (parse-integer (alu-w alu))))
        (t (setf val (parse-integer reg))))
  (cond ((string= reg "x") (setf (alu-x alu) (floor (alu-x alu) val)))
        ((string= reg "y") (setf (alu-y alu) (floor (alu-y alu) val)))
        ((string= reg "z") (setf (alu-z alu) (floor (alu-z alu) val)))
        ((string= reg "w") (setf (alu-w alu) (floor (alu-w alu) val))))

(defun alu-get-register-value (alu reg)
  (ecase reg
    ("x" (alu-x alu))
    ("y" (alu-y alu))
    ("z" (alu-z alu))
    ("w" (alu-w alu))))

(defun alu-set-register-value (alu reg value)
  (ecase reg
    ("x" (setf (alu-x alu) value))
    ("y" (setf (alu-y alu) value))
    ("z" (setf (alu-z alu) value))
    ("w" (setf (alu-w alu)))))

(defun alu-div (alu reg val)
  (let ((value (if (member val '("x" "y" "z" "w") :test #'string=)
                   (alu-get-register-value alu val)
                   (parse-integer val))))
    (alu-set-register-value alu reg (floor (alu-get-register-value alu reg) value))))

