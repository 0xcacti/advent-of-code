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


(defun next-multiple-of-4-above (n)
  (let ((remainder (mod n 4)))
    (if (zerop remainder)
        n
        (+ n (- 4 remainder)))))


(defun parse-packet-literal (code start)
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
    ;; (incf start (- (next-multiple-of-4-above amount-read) amount-read))
    (values (parse-integer bit-string :radix 2) start)))


(defun parse-packet (code start)
  (let* ((version (get-version code start))
         (type-id (get-message-type code (+ start 3)))
         (current-pos (+ 6 start))) 
    (if (= type-id 4)
        ;; Literal value packet
        (multiple-value-bind (value new-pos) (parse-packet-literal code current-pos)
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
                (loop for i from 0 below num-sub-packets
                      do (multiple-value-bind (packet new-pos sub-version-sum)
                             (parse-packet code current-pos)
                           (push packet sub-packets)
                           (incf version-sum sub-version-sum)
                           (setf current-pos new-pos)))
                (values (list :version version :type-id type-id :sub-packets (nreverse sub-packets))
                        current-pos
                        version-sum)))))))

;; parse literal
(parse-packet (hex-to-binary "D2FE28") 0)

;; parse operator length type 0 
(parse-packet (hex-to-binary "38006F45291200") 0)

;; parse operator length type  1
(parse-packet (hex-to-binary "EE00D40C823060") 0)

;; test case one 
(parse-packet (hex-to-binary "8A004A801A8002F478") 0)


(defun solve-one () 
  (let* ((hex-code (read-input nil))
         (binary (hex-to-binary hex-code)))
    (multiple-value-bind (packet new-pos version-sum)
        (parse-packet binary 0)
      (format t "Version Sum: ~a~%" version-sum)
      version-sum)))

(solve-one)


;; Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
;; Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
;; Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
;; Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
;; Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
;; Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
;; Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.



(defun find-packet-value (packet)
  (let ((type-id (getf packet :type-id)))
  (cond ((= type-id 0)
         (let ((sum 0))
          (format t "in sum type")
          (loop for sub-packet in (getf packet :sub-packets) do 
           (incf sum (find-packet-value sub-packet)))
            sum))
         ((= type-id 1)
          (let ((product 1))
          (loop for sub-packet in (getf packet :sub-packets) do 
           (setf product (* product (find-packet-value sub-packet))))
            product))
         ((= type-id 2)
          (let ((mini 9999999999999999))
            (loop for sub-packet in (getf packet :sub-packets) do 
             (if (< (find-packet-value sub-packet) mini)
                 (setf mini (find-packet-value sub-packet))))
            mini))
         ((= type-id 3)
          (let ((maxi -99999999999999))
            (loop for sub-packet in (getf packet :sub-packets) do 
             (if (> (find-packet-value sub-packet) maxi)
                 (setf maxi (find-packet-value sub-packet))))
            maxi))
         ((= type-id 4)
           (getf packet :value))
         ((= type-id 5)
           (let* ((gt 0)
                  (sub-packets (getf packet :sub-packets))
                  (p1 (first sub-packets))
                  (p2 (second sub-packets)))
                (if (> (find-packet-value p1) (find-packet-value p2))
                    (setf gt 1))
             gt))
         ((= type-id 6)
           (let* ((lt 0)
                  (sub-packets (getf packet :sub-packets))
                  (p1 (first sub-packets))
                  (p2 (second sub-packets)))
                (if (< (find-packet-value p1) (find-packet-value p2))
                    (setf lt 1))
             lt))
         ((= type-id 7)
          (let* ((eqy 0)
                 (sub-packets (getf packet :sub-packets))
                 (p1 (first sub-packets))
                 (p2 (second sub-packets)))
            (if (= (find-packet-value p1) (find-packet-value p2))
                (setf eqy 1))
            eqy)))))

(find-packet-value (parse-packet (hex-to-binary "C200B40A82") 0))
(find-packet-value (parse-packet (hex-to-binary "04005AC33890") 0))
(find-packet-value (parse-packet (hex-to-binary "880086C3E88112") 0))
(find-packet-value (parse-packet (hex-to-binary "CE00C43D881120") 0))
(find-packet-value (parse-packet (hex-to-binary "D8005AC2A8F0") 0))
(find-packet-value (parse-packet (hex-to-binary "F600BC2D8F") 0))
(find-packet-value (parse-packet (hex-to-binary "9C005AC2F8F0") 0))
(find-packet-value (parse-packet (hex-to-binary "9C0141080250320F1802104A08") 0))


(defun solve-one () 
  (let* ((hex-code (read-input nil))
         (binary (hex-to-binary hex-code)))
    (multiple-value-bind (packet new-pos version-sum)
        (parse-packet binary 0)
      (format t "Version Sum: ~a~%" version-sum)
      version-sum)))

(defun solve-two () 
  "Solve day 16 part two" 
  (let* ((hex-code (read-input nil))
         (binary (hex-to-binary hex-code)))
    (multiple-value-bind (packet new-pos version-sum)
        (parse-packet binary 0)
        (find-packet-value packet))))

(solve-two)
