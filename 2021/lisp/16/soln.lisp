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

(defun get-version (code start)
  (let ((binary-subseq (subseq code start (+ start 3))))
    (parse-integer binary-subseq :radix 2)))

(defun get-message-type (code start) 
  (let ((binary-subseq (subseq code start (+ start 3))))
    (parse-integer binary-subseq :radix 2)))

(defun get-length-type-id (code start)
  (let ((binary-subseq (subseq code start (+ start 1))))
    (parse-integer binary-subseq :radix 2)))

(defun read-n-bits (code start n)
  (let ((binary-subseq (subseq code start (+ start n))))
    (parse-integer binary-subseq :radix 2)))

(defun n-bit-subseq (code start n)
  (subseq code start (+ start n)))


;; 001 110 0 000000000011011 11010001010 0101001000100100
;; VVV TTT I LLLLLLLLLLLLLLL AAAAAAAAAAA BBBBBBBBBBBBBBBB

;; read until you have a chunk starting with 0 
;; then read the 5 bits including the 0 
;; then the bits to get to the next multiple of 4 
;; and that's the new offset


(defun next-multiple-of-4-above (n)
  (let ((remainder (mod n 4)))
    (if (zerop remainder)
        n
        (+ n (- 4 remainder)))))


(defun literal-value (code start)
  (let* ((bit-string "")
         (amount-read 0)
         (keep-reading t))
    (loop while keep-reading
          do 
          (let ((chunk (n-bit-subseq code start 5)))
            (cond ((string= (n-bit-subseq chunk 0 1) "0")
                   (setf bit-string (concatenate 'string bit-string (subseq chunk 1 5)))
                   (setf keep-reading nil))
                  (t 
                   (setf bit-string (concatenate 'string bit-string (subseq chunk 1 5))))))
          (setf start (+ start 5))
          (setf amount-read (+ amount-read 5)))
    (format t "next multiple of 4 above start ~a: ~a~%" amount-read (next-multiple-of-4-above amount-read))
    (incf start (- (next-multiple-of-4-above amount-read) amount-read))
    (values (parse-integer bit-string :radix 2) start)))

(literal-value "101111111000101000" 0)
;;        10111 11110 00101 000
;; VVVTTT AAAAA BBBBB CCCCC

(defun operator-packet-type-0 (code start))

(operator-packet-type-0 "101111111000101000" 0)

(defun operator-packet-type-1 (code start))

(defun literal-value (code start)
  (let* ((bit-string "")
         (amount-read 0)
         (keep-reading t))
    (loop while keep-reading
          do 
          (let ((chunk (n-bit-subseq code start 5)))
            (cond ((string= (n-bit-subseq chunk 0 1) "0")
                   (setf bit-string (concatenate 'string bit-string (subseq chunk 1 5)))
                   (setf keep-reading nil))
                  (t 
                   (setf bit-string (concatenate 'string bit-string (subseq chunk 1 5))))))
          (setf start (+ start 5))
          (setf amount-read (+ amount-read 5)))
    (incf start (- (next-multiple-of-4-above amount-read) amount-read))
    (values (parse-integer bit-string :radix 2) start)))



(defun parse-packet (code start)
  (let* ((keep-reading t)
         (version (get-version code start))
         (message-type (get-message-type code (+ start 3)))
         (length-type-id (get-length-type-id code (+start 6))))
    (loop while keep-reading do
        (cond ((= message-type 4)
                (let (())))
              (t
               (let (())))))))

(defun packet (code start)




;; First 3 bits - Version - only the thing we sum
;; Second 3 bits - Type ID
    ;; == 4 -- literal value
    ;; Next Bit -- Length type Id
        ;; == 0 -- next 15 bits is the total number of bits of all packets

    ;; != 5 -- operator value
    ;; Next Bit -- Length type Id
        ;; == 0 -- the next 11 bits are the total number of sub packets contained


        
(defun parse-packet (code start)
  (let* ((version (get-version code start))
         (type-id (get-message-type code (+ start 3)))
         (current-pos (+ start 6))) ; After reading version and type ID
    (if (= type-id 4)
        ;; Literal value packet
        (multiple-value-bind (value new-pos) (literal-value code current-pos)
          ;; Return the packet data, new position, and version sum
          (values (list :version version :type-id type-id :value value) new-pos version))
        ;; Operator packet
        (let* ((length-type-id (get-length-type-id code current-pos))
               (current-pos (1+ current-pos)) ; Move past length type ID
               (version-sum version)) ; Start the version sum with the current version
          (if (= length-type-id 0)
              ;; Next 15 bits are total length in bits of sub-packets
              (let* ((total-length (read-n-bits code current-pos 15))
                     (current-pos (+ current-pos 15))
                     (end-pos (+ current-pos total-length))
                     (sub-packets '()))
                ;; Parse sub-packets until reaching end-pos
                (loop while (< current-pos end-pos)
                      do (multiple-value-bind (packet new-pos sub-version-sum)
                             (parse-packet code current-pos)
                           (push packet sub-packets)
                           (incf version-sum sub-version-sum)
                           (setf current-pos new-pos)))
                (values (list :version version :type-id type-id :sub-packets (nreverse sub-packets))
                        current-pos
                        version-sum))
              ;; Next 11 bits are number of sub-packets
              (let* ((num-sub-packets (read-n-bits code current-pos 11))
                     (current-pos (+ current-pos 11))
                     (sub-packets '()))
                ;; Parse num-sub-packets sub-packets
                (loop for i from 1 to num-sub-packets
                      do (multiple-value-bind (packet new-pos sub-version-sum)
                             (parse-packet code current-pos)
                           (push packet sub-packets)
                           (incf version-sum sub-version-sum)
                           (setf current-pos new-pos)))
                (values (list :version version :type-id type-id :sub-packets (nreverse sub-packets))
                        current-pos
                        version-sum)))))))


(defun solve-one () 
  (let* ((hex-code (read-input nil))
         (binary (hex-to-binary hex-code)))
    (multiple-value-bind (packet new-pos version-sum)
        (parse-packet binary 0)
      (format t "Version Sum: ~a~%" version-sum)
      version-sum)))

(solve-one)
