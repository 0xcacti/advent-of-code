(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/16/"))
         (code ""))
    (with-open-file (stream path :direction :input)
        (loop for row from 0 
            for line = (read-line stream nil)
            while line do 
            (setf code (concatenate 'string code line))))
    code))

(defun hex-to-binary (hex-string)
  "Convert a hexadecimal string to its binary representation with leading zeros."
  (let ((integer-value (parse-integer hex-string :radix 16)))
    ;; Pad the binary string to ensure it has 4 bits per hex digit
    (format nil "~v,'0b" (* 4 (length hex-string)) integer-value)))


(defun solve-one () 
  (let* ((code (read-input nil))
         (test-code "38006F45291200"))
    (hex-to-binary test-code)))

(solve-one)  
