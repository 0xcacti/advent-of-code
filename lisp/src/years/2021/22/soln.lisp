(ql:quickload "cl-ppcre")

(defstruct cuboid 
  on-off
  x-start
  x-end
  y-start
  y-end
  z-start
  z-end)

(defmethod print-object ((c cuboid) stream)
  (format stream "#<CUBOID ~A x=~A..~A, y=~A..~A, z=~A..~A>"
          (cuboid-on-off c)
          (cuboid-x-start c) (cuboid-x-end c)
          (cuboid-y-start c) (cuboid-y-end c)
          (cuboid-z-start c) (cuboid-z-end c)))


(defun read-input (is-test) 
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/22/"))
         (cuboids '()))
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line do
            (let* ((parts (cl-ppcre:split " " line))
                   (coord-parts (cl-ppcre:split "," (second parts)))
                   (x-parts (cl-ppcre:split "\\.\\." (second (cl-ppcre:split "=" (first coord-parts)))))
                   (y-parts (cl-ppcre:split "\\.\\." (second (cl-ppcre:split "=" (second coord-parts)))))
                   (z-parts (cl-ppcre:split "\\.\\." (second (cl-ppcre:split "=" (third coord-parts))))))
              (push (make-cuboid
                      :on-off (first parts)
                      :x-start (parse-integer (first x-parts))
                      :x-end (parse-integer (second x-parts))
                      :y-start (parse-integer (first y-parts))
                      :y-end (parse-integer (second y-parts))
                      :z-start (parse-integer (first z-parts))
                      :z-end (parse-integer (second z-parts))) cuboids)))) (reverse cuboids)))
                      


(defun flatten (array)
  "Flatten a three-dimensional array into a list."
  (loop for x from 0 below (array-dimension array 0)
        append (loop for y from 0 below (array-dimension array 1)
                     append (loop for z from 0 below (array-dimension array 2)
                                  collect (aref array x y z)))))

(defun solve-one ()
  "Solve part one day 22" 
  (let* ((cuboids (read-input nil))
         ;; Array size adjusted to cover -50 to +50 by using an offset
         (cube (make-array '(101 101 101) :initial-element 0))) ; 101 to include both -50 and 50
    (loop for cuboid in cuboids do 
        (loop for x from (max -50 (cuboid-x-start cuboid)) to (min 50 (cuboid-x-end cuboid)) do
            (loop for y from (max -50 (cuboid-y-start cuboid)) to (min 50 (cuboid-y-end cuboid)) do
            (loop for z from (max -50 (cuboid-z-start cuboid)) to (min 50 (cuboid-z-end cuboid)) do
                  ;; Apply offset of 50 to convert range -50 to +50 to 0 to 100 for indexing
                  (setf (aref cube (+ x 50) (+ y 50) (+ z 50))
                        (if (string= (cuboid-on-off cuboid) "on") 1 0))))))
    (format t "Number of lights on: ~A~%" (count 1 (flatten cube)))))


(solve-one)

(defun cuboid-volume (cuboid)
  "Calculate the volume of a cuboid."
  (let ((x-length (1+ (- (cuboid-x-end cuboid) (cuboid-x-start cuboid))))
        (y-length (1+ (- (cuboid-y-end cuboid) (cuboid-y-start cuboid))))
        (z-length (1+ (- (cuboid-z-end cuboid) (cuboid-z-start cuboid)))))
    (* x-length y-length z-length)))

(defun cuboids-overlap (cuboid1 cuboid2)
  "Determine if two cuboids overlap, and return the overlapping cuboid if they do."
  (let ((x-start (max (cuboid-x-start cuboid1) (cuboid-x-start cuboid2)))
        (x-end (min (cuboid-x-end cuboid1) (cuboid-x-end cuboid2)))
        (y-start (max (cuboid-y-start cuboid1) (cuboid-y-start cuboid2)))
        (y-end (min (cuboid-y-end cuboid1) (cuboid-y-end cuboid2)))
        (z-start (max (cuboid-z-start cuboid1) (cuboid-z-start cuboid2)))
        (z-end (min (cuboid-z-end cuboid1) (cuboid-z-end cuboid2))))
    (when (and (<= x-start x-end) (<= y-start y-end) (<= z-start z-end))
      (make-cuboid :on-off "on" :x-start x-start :x-end x-end
                   :y-start y-start :y-end y-end
                   :z-start z-start :z-end z-end))))

(defun subtract-cuboid (cuboid sub-cuboid)
  "Generate a list of cuboids representing the volume of CUBOID minus SUB-CUBOID."
  (let ((overlap (cuboids-overlap cuboid sub-cuboid)))
    (if (not overlap)
        (list cuboid)  ; If no overlap, return the original cuboid
      ;; Split cuboid based on the overlap into non-overlapping parts
      ;; Generate non-overlapping parts around the overlap
      (remove nil
              (list
               (when (< (cuboid-x-start cuboid) (cuboid-x-start overlap))
                 (make-cuboid :on-off "on" :x-start (cuboid-x-start cuboid) :x-end (1- (cuboid-x-start overlap))
                              :y-start (cuboid-y-start cuboid) :y-end (cuboid-y-end cuboid)
                              :z-start (cuboid-z-start cuboid) :z-end (cuboid-z-end cuboid)))
               (when (> (cuboid-x-end cuboid) (cuboid-x-end overlap))
                 (make-cuboid :on-off "on" :x-start (1+ (cuboid-x-end overlap)) :x-end (cuboid-x-end cuboid)
                              :y-start (cuboid-y-start cuboid) :y-end (cuboid-y-end cuboid)
                              :z-start (cuboid-z-start cuboid) :z-end (cuboid-z-end cuboid)))
               (when (< (cuboid-y-start cuboid) (cuboid-y-start overlap))
                 (make-cuboid :on-off "on" :x-start (cuboid-x-start overlap) :x-end (cuboid-x-end overlap)
                              :y-start (cuboid-y-start cuboid) :y-end (1- (cuboid-y-start overlap))
                              :z-start (cuboid-z-start cuboid) :z-end (cuboid-z-end cuboid)))
               (when (> (cuboid-y-end cuboid) (cuboid-y-end overlap))
                 (make-cuboid :on-off "on" :x-start (cuboid-x-start overlap) :x-end (cuboid-x-end overlap)
                              :y-start (1+ (cuboid-y-end overlap)) :y-end (cuboid-y-end cuboid)
                              :z-start (cuboid-z-start cuboid) :z-end (cuboid-z-end cuboid)))
               (when (< (cuboid-z-start cuboid) (cuboid-z-start overlap))
                 (make-cuboid :on-off "on" :x-start (cuboid-x-start overlap) :x-end (cuboid-x-end overlap)
                              :y-start (cuboid-y-start overlap) :y-end (cuboid-y-end overlap)
                              :z-start (cuboid-z-start cuboid) :z-end (1- (cuboid-z-start overlap))))
               (when (> (cuboid-z-end cuboid) (cuboid-z-end overlap))
                 (make-cuboid :on-off "on" :x-start (cuboid-x-start overlap) :x-end (cuboid-x-end overlap)
                              :y-start (cuboid-y-start overlap) :y-end (cuboid-y-end overlap)
                              :z-start (1+ (cuboid-z-end overlap)) :z-end (cuboid-z-end cuboid))))))))

(defun solve-two ()
  "Solve part two day 22"
  (let* ((cuboids (read-input nil))
         (processed-cuboids '()))
    (dolist (cuboid cuboids)
      (let ((new-cuboids '()))
        (dolist (processed processed-cuboids)
          (let ((overlap (cuboids-overlap cuboid processed)))
            (when overlap
              ;; Add overlap with opposite sign
              (push (make-cuboid
                     :on-off (if (string= (cuboid-on-off processed) "on") "off" "on")
                     :x-start (cuboid-x-start overlap)
                     :x-end (cuboid-x-end overlap)
                     :y-start (cuboid-y-start overlap)
                     :y-end (cuboid-y-end overlap)
                     :z-start (cuboid-z-start overlap)
                     :z-end (cuboid-z-end overlap))
                    new-cuboids))))
        ;; Add the new cuboid if it's an "on" instruction
        (when (string= (cuboid-on-off cuboid) "on")
          (push cuboid new-cuboids))
        ;; Update processed cuboids
        (setq processed-cuboids (nconc processed-cuboids new-cuboids))))
    ;; Calculate the total volume
    (let ((total-volume 0))
      (dolist (cuboid processed-cuboids)
        (let ((sign (if (string= (cuboid-on-off cuboid) "on") 1 -1)))
          (incf total-volume (* sign (cuboid-volume cuboid)))))
      (format t "Total volume of 'on' cubes: ~A~%" total-volume))))



(solve-two)
