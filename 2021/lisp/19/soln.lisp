(defstruct (scanner (:constructor make-scanner (&key name beacons)))
  (name "" :type string)
  (beacons nil :type list))

(defstruct (vector3d (:constructor make-vector3d (x y z))
                     (:print-function print-vector3d))
  (x 0 :type integer)
  (y 0 :type integer)
  (z 0 :type integer))

;; Custom print function for vector3d
(defun print-vector3d (vec stream depth)
  (declare (ignore depth))
  (format stream "#<VECTOR3D x:~A y:~A z:~A>" 
          (vector3d-x vec)
          (vector3d-y vec)
          (vector3d-z vec)))

(defun vector3d-length-squared (vec)
  "Return the squared length of the vector"
  (with-slots (x y z) vec
    (+ (* x x) (* y y) (* z z))))

(defun vector3d-length (vec)
  "Return the length of the vector"
  (sqrt (float (vector3d-length-squared vec))))

(defun vector3d-add (vec1 vec2)
  "Add two vectors"
  (make-vector3d
   (+ (vector3d-x vec1) (vector3d-x vec2))
   (+ (vector3d-y vec1) (vector3d-y vec2))
   (+ (vector3d-z vec1) (vector3d-z vec2))))

(defun vector3d-subtract (vec1 vec2)
  "Subtract vec2 from vec1"
  (make-vector3d
   (- (vector3d-x vec1) (vector3d-x vec2))
   (- (vector3d-y vec1) (vector3d-y vec2))
   (- (vector3d-z vec1) (vector3d-z vec2))))

(defun vector3d-divide (vec1 vec2)
  "Divide vec1 by vec2, handling zero division"
  (flet ((safe-divide (a b)
           (if (or (zerop b) (zerop a))
               0
               (truncate a b))))
    (make-vector3d
     (safe-divide (vector3d-x vec1) (vector3d-x vec2))
     (safe-divide (vector3d-y vec1) (vector3d-y vec2))
     (safe-divide (vector3d-z vec1) (vector3d-z vec2)))))

(defun vector3d-multiply (vec1 vec2)
  "Multiply two vectors component-wise"
  (make-vector3d
   (* (vector3d-x vec1) (vector3d-x vec2))
   (* (vector3d-y vec1) (vector3d-y vec2))
   (* (vector3d-z vec1) (vector3d-z vec2))))

(defun vector3d-dot-product (vec1 vec2)
  "Calculate dot product of two vectors"
  (+ (* (vector3d-x vec1) (vector3d-x vec2))
     (* (vector3d-y vec1) (vector3d-y vec2))
     (* (vector3d-z vec1) (vector3d-z vec2))))

(defun vector3d-cross-product (vec1 vec2)
  "Calculate cross product of two vectors"
  (make-vector3d
   (- (* (vector3d-y vec1) (vector3d-z vec2))
      (* (vector3d-z vec1) (vector3d-y vec2)))
   (- (* (vector3d-z vec1) (vector3d-x vec2))
      (* (vector3d-x vec1) (vector3d-z vec2)))
   (- (* (vector3d-x vec1) (vector3d-y vec2))
      (* (vector3d-y vec1) (vector3d-x vec2)))))

(defun vector3d-distance-to (vec1 vec2)
  "Calculate distance between two vectors"
  (vector3d-length (vector3d-subtract vec1 vec2)))

(defun vector3d-angle-between (vec1 vec2)
  "Calculate angle between two vectors in radians"
  (acos (/ (float (vector3d-dot-product vec1 vec2))
           (* (vector3d-length vec1)
              (vector3d-length vec2)))))

(defun vector3d-equal (vec1 vec2)
  "Test if two vectors are equal"
  (and (= (vector3d-x vec1) (vector3d-x vec2))
       (= (vector3d-y vec1) (vector3d-y vec2))
       (= (vector3d-z vec1) (vector3d-z vec2))))

(defun abs-vector (vec)
  "Return a new vector with the absolute values of the original vector's components."
  (make-vector3d
   (abs (vector3d-x vec))
   (abs (vector3d-y vec))
   (abs (vector3d-z vec))))

(defun get-vector-key (vec)
  "Generate a key based on the sorted absolute values of the vector's components."
  (sort (list (abs (vector3d-x vec))
              (abs (vector3d-y vec))
              (abs (vector3d-z vec)))
        #'<))

(defstruct beacon3d 
  (position nil :type vector3d)
  (edges (make-hash-table :test #'equal)))


(defun update-edges (scanner)
  (dolist (a (scanner-beacons scanner))
    (setf (beacon3d-edges a) (make-hash-table :test #'equal))
    (dolist (b (scanner-beacons scanner))
      (unless (eq a b)
        (add-edge a b)))))

(defun add-edge (a b)
  (let* ((edge (vector3d-subtract (beacon3d-position a) (beacon3d-position b)))
         (key (get-vector-key edge))
         (edges (gethash key (beacon3d-edges a))))
    (push edge edges)
    (setf (gethash key (beacon3d-edges a)) edges)))


(defun find-edge (beacon edge)
  (let ((edge-key (get-vector-key edge)))
    (values (gethash edge-key (beacon3d-edges beacon))
            (not (null (gethash edge-key (beacon3d-edges beacon)))))))

(defun parse-scanners (data)
  (let ((scanners nil)
        (current-scanner nil))
    (dolist (line data)
      (cond
        ;; Scanner header line
        ((cl-ppcre:scan "---" line)
         (let ((scanner (make-scanner :name (string-trim " -" line))))
           (setf current-scanner scanner)
           (push scanner scanners)))
        
        ;; Empty line
        ((string= (string-trim " " line) "")
         nil)
        
        ;; Beacon coordinates
        (t 
         (multiple-value-bind (valid x y z)
             (parse-coordinate-line line)
           (when valid
             (push (make-beacon3d 
                    :position (make-vector3d x y z)
                    :edges (make-hash-table :test #'equal))
                   (scanner-beacons current-scanner)))))))
    
    ;; Update edges for all scanners
    (dolist (scanner (reverse scanners))
      (update-edges scanner))
    
    (reverse scanners)))

;; Helper function to parse coordinate lines
(defun parse-coordinate-line (line)
  (multiple-value-bind (match registers)
      (cl-ppcre:scan-to-strings "(-?\\d+),(-?\\d+),(-?\\d+)" line)
    (if match
        (values t
                (parse-integer (aref registers 0))
                (parse-integer (aref registers 1))
                (parse-integer (aref registers 2)))
        (values nil 0 0 0))))

(defun shares-at-least-n-edges (beacon-a beacon-b n)
  (let ((shared 0))
    (maphash 
     (lambda (key edges)
       (declare (ignore key))
       (dolist (edge edges)
         (multiple-value-bind (found-edges found)
             (find-edge beacon-b edge)
           (declare (ignore found-edges))
           (when found
             (incf shared)
             (when (= shared n)
               (return-from shares-at-least-n-edges t))))))
     (beacon3d-edges beacon-a))
    nil))

(defun get-orientation-diff (beacon-a beacon-b)
  (let (signs order)
    (maphash
     (lambda (key edges)
       (declare (ignore key))
       (dolist (edge edges)
         (multiple-value-bind (alt-edges found)
             (find-edge beacon-b edge)
           (when (and found (= (length alt-edges) 1))
             (let ((alt-edge (first alt-edges))
                   (axes (vector (vector3d-x edge)
                                 (vector3d-y edge)
                                 (vector3d-z edge)))
                   (alt-axes (vector (vector3d-x alt-edge)
                                     (vector3d-y alt-edge)
                                     (vector3d-z alt-edge))))
               (setf order (make-array 3))
               (dotimes (i 3)
                 (dotimes (j 3)
                   (when (= (abs (aref axes i))
                            (abs (aref alt-axes j)))
                     (setf (aref order i) j))))
               (setf signs (vector3d-divide edge
                                            (align alt-edge order signs)))
               (return-from get-orientation-diff 
                 (values signs order)))))))
     (beacon3d-edges beacon-a))
    (values nil nil)))

(defun align (vec order signs)
  (let* ((arr (vector (vector3d-x vec)
                      (vector3d-y vec)
                      (vector3d-z vec)))
         (new-vec (make-vector3d
                   (aref arr (aref order 0))
                   (aref arr (aref order 1))
                   (aref arr (aref order 2)))))
    (when signs
      (setf new-vec (vector3d-multiply new-vec signs)))
    new-vec))

(defun compare-scanner (scanner-a scanner-b min-shared-beacons)
  (let ((unmatched (copy-list (scanner-beacons scanner-b)))
        (remaining (length (scanner-beacons scanner-a)))
        (shared 0)
        self-beacon alt-beacon
        new-beacons scanner-position)
    
    (dolist (a (scanner-beacons scanner-a))
      (when (< (+ remaining shared) min-shared-beacons)
        (return))
      (decf remaining)
      (dotimes (i (length unmatched))
        (let ((b (nth i unmatched)))
          (when (shares-at-least-n-edges a b (1- min-shared-beacons))
            (incf shared)
            (setf unmatched (append (subseq unmatched 0 i)
                                    (subseq unmatched (1+ i)))
                  self-beacon a
                  alt-beacon b)
            (return)))))
    
    (when (>= shared min-shared-beacons)
      (multiple-value-bind (signs order)
          (get-orientation-diff self-beacon alt-beacon)
        (let ((alt (align (beacon3d-position alt-beacon) order signs)))
          (dolist (b unmatched)
            (let* ((aligned (align (beacon3d-position b) order signs))
                   (diff (vector3d-subtract aligned alt))
                   (new-position (vector3d-add (beacon3d-position self-beacon) 
                                               diff)))
              (push (make-beacon3d :position new-position) new-beacons)))
          (setf scanner-position 
                (vector3d-subtract (beacon3d-position self-beacon) alt)))))
    
    (values new-beacons shared scanner-position)))

(defun add-beacons (scanner new-beacons)
  (setf (scanner-beacons scanner)
        (append (scanner-beacons scanner) new-beacons))
  (dolist (a new-beacons)
    (setf (beacon3d-edges a) (make-hash-table :test #'equal))
    (dolist (b (scanner-beacons scanner))
      (unless (eq a b)
        (add-edge a b)
        (add-edge b a)))))

(defun merge-scanners (content &key (min-shared-beacons 12))
  (let* ((scanners (parse-scanners content))
         (composite (first scanners))
         (relative-positions (list (make-vector3d 0 0 0)))
         (queue (rest scanners))
         last-scanner)
    
    (loop while queue do
      (let ((scanner (pop queue)))
        (when (eq scanner last-scanner)
          (error "Repeat scanner found beacons: ~A" 
                 (length (scanner-beacons scanner))))
        
        (setf last-scanner scanner)
        
        (multiple-value-bind (new-beacons count relative-scanner)
            (compare-scanner composite scanner min-shared-beacons)
          (if (> count 0)
              (progn
                (add-beacons composite new-beacons)
                (push relative-scanner relative-positions))
              (setf queue (append queue (list scanner)))))))
    
    (values composite (nreverse relative-positions))))

(defun manhattan-distance (a b)
  (+ (abs (- (vector3d-x a) (vector3d-x b)))
     (abs (- (vector3d-y a) (vector3d-y b)))
     (abs (- (vector3d-z a) (vector3d-z b)))))

(defun solve-one ()
  "Solve day 19 part one"
  (multiple-value-bind (composite positions)
      (merge-scanners (read-input t))
    (length (scanner-beacons composite))))

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/19/"))
         (lines '()))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do 
              (push line lines)))
    (reverse lines)))

(solve-one)

(defun solve-two (is-test)
  "Solve day 19 part two"
  (multiple-value-bind (composite positions)
      (merge-scanners (read-input is-test))
    (let ((max-distance 0))
      (dolist (a positions)
        (dolist (b positions)
          (setf max-distance 
                (max max-distance (manhattan-distance a b)))))
      max-distance)))
