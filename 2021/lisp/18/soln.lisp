;; Rules
;; If any pair is nested inside four pairs, the leftmost such pair explodes.
;; If any regular number is 10 or greater, the leftmost such regular number splits.
;;  During reduction, at most one action applies, after which the process returns 
;;  to the top of the list of actions. For example, 
;;  if split produces a pair that meets the explode criteria, 
;;  that pair explodes before other splits occur.
(ql:quickload "cl-ppcre")

(defun find-simple-pairs (snail-num)
  "Find simple pairs in a snailfish number string."
  (let* ((pair-regex "\\[(\\d+),(\\d+)\\]")
         (positions '())
         (actual-pairs '()))
    
    ;; First, find all positions of matches
    (cl-ppcre:do-matches-as-strings (pair pair-regex snail-num)
      (let ((pos (cl-ppcre:scan pair-regex snail-num)))
        (push (list pos (+ pos (length pair))) positions)
        
        ;; Extract and parse the numbers from each pair
        (cl-ppcre:register-groups-bind (left-num right-num)
            ("(\\d+),(\\d+)" pair)
          (push (list (parse-integer left-num)
                     (parse-integer right-num))
                actual-pairs))))
    
    (values (reverse positions) (reverse actual-pairs))))

;; Helper function to reverse a string
(defun reverse-string (string)
  (map 'string #'identity (reverse (map 'list #'identity string))))

;; Find depth of a pair at a given position
(defun find-depth (snail-num slice)
  (let ((depth 0))
    (loop for char across snail-num
          for i from 0
          do (case char
               (#\[ (incf depth))
               (#\] (decf depth)))
          until (= i (first slice)))
    depth))

;; Find next number in either direction
(defun find-next (snail-num index offset left)
  (let* ((num-regex "\\d+")
         (substring (if left
                       (reverse-string (subseq snail-num 0 (+ index offset)))
                       (subseq snail-num (+ index offset))))
         (match-pos (cl-ppcre:scan num-regex substring)))
    (when match-pos
      (multiple-value-bind (start end) 
          (cl-ppcre:scan num-regex substring)
        (let* ((num-string (subseq substring start end))
               (result (if left
                          (list (- index (- end offset)) 
                                (- index (- start offset)))
                          (list (+ index start offset) 
                                (+ index end offset))))
               (num (parse-integer (if left 
                                     (reverse-string num-string)
                                     num-string))))
          (values result num))))))

;; Try to split a number
(defun try-split (snail-num)
  (let ((match (cl-ppcre:scan "\\d\\d+" snail-num)))
    (if match
        (multiple-value-bind (start end) 
            (cl-ppcre:scan "\\d\\d+" snail-num)
          (let* ((num (parse-integer (subseq snail-num start end)))
                 (left-num (floor num 2))
                 (right-num (ceiling num 2))
                 (new-pair (format nil "[~d,~d]" left-num right-num)))
            (values (concatenate 'string 
                               (subseq snail-num 0 start)
                               new-pair
                               (subseq snail-num end))
                   t)))
        (values snail-num nil))))

;; Complete the try-explode function
(defun try-explode (snail-num)
  (multiple-value-bind (simple-nums pairs) (find-simple-pairs snail-num)
    (loop for slice in simple-nums
          for pair in pairs
          for depth = (find-depth snail-num slice)
          when (>= depth 5)
          do (let ((left-num (first pair))
                  (right-num (second pair))
                  (offset-left 0)
                  (offset-mid 0))
               (multiple-value-bind (left-slice left-val) 
                   (find-next snail-num (first slice) 0 t)
                 (when left-slice
                   (let* ((new-left (+ left-val left-num))
                          (new-left-str (write-to-string new-left))
                          (left-diff (- (length new-left-str) 
                                      (- (second left-slice) 
                                         (first left-slice)))))
                     (setf snail-num 
                           (concatenate 'string
                                      (subseq snail-num 0 (first left-slice))
                                      new-left-str
                                      (subseq snail-num (second left-slice)))
                           offset-left left-diff))))
               
               (let ((new-mid (concatenate 'string
                                         (subseq snail-num 
                                                0 
                                                (+ (first slice) offset-left))
                                         "0"
                                         (subseq snail-num 
                                                (+ (second slice) offset-left)))))
                 (setf offset-mid (- 1 (- (second slice) (first slice)))
                       snail-num new-mid))
               
               (multiple-value-bind (right-slice right-val)
                   (find-next snail-num 
                             (second slice) 
                             (+ offset-left offset-mid) 
                             nil)
                 (when right-slice
                   (let ((new-right (+ right-val right-num)))
                     (setf snail-num 
                           (concatenate 'string
                                      (subseq snail-num 0 (first right-slice))
                                      (write-to-string new-right)
                                      (subseq snail-num (second right-slice)))))))
               (return-from try-explode (values snail-num t))))
    (values snail-num nil)))

;; Addition function
(defun add-nums (left-num right-num)
  (format nil "[~a,~a]" left-num right-num))

;; Calculate magnitude
(defun pair-magnitude (pair)
  (+ (* 3 (first pair))
     (* 2 (second pair))))

(defun magnitude (num)
  (loop while (find #\[ num)
        do (multiple-value-bind (positions pairs) 
               (find-simple-pairs num)
             (let* ((mag (pair-magnitude (first pairs)))
                    (mag-string (write-to-string mag))
                    (pos (first positions)))
               (setf num (concatenate 'string
                                    (subseq num 0 (first pos))
                                    mag-string
                                    (subseq num (second pos)))))))
  (parse-integer num))

;; Complete the reduce function
(defun reduce-snail (snail-num)
  (loop with modified = t
        while modified
        do (multiple-value-bind (new-num exploded) 
               (try-explode snail-num)
             (if exploded
                 (setf snail-num new-num
                       modified t)
                 (multiple-value-bind (new-num split) 
                     (try-split snail-num)
                   (if split
                       (setf snail-num new-num
                             modified t)
                       (setf modified nil)))))
        finally (return snail-num)))

;; Complete part one solution
(defun solve-one ()
  (let* ((lines (read-input t))
         (result (first lines)))
    (setf result (reduce-snail result))
    (loop for line in (rest lines)
          do (setf result (reduce-snail (add-nums result line))))
    (magnitude result)))

(solve-one)

;; Part two solution
(defun solve-part-two ()
  (let* ((lines (read-input t))
         (max-magnitude 0))
    (loop for i from 0 below (length lines)
          do (loop for j from 0 below (length lines)
                   when (/= i j)
                   do (let* ((sum (reduce-snail 
                                  (add-nums (nth i lines) 
                                          (nth j lines))))
                           (mag (magnitude sum)))
                        (when (> mag max-magnitude)
                          (setf max-magnitude mag)))))
    max-magnitude))
