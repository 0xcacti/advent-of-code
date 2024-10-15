(ql:quickload "cl-ppcre")

(defun read-input (is-test)
  (let* ((input-file (if is-test "test-input.txt" "input.txt"))
         (path (merge-pathnames input-file #P"~/code/challenges/aoc/2021/lisp/17/"))
         (code "")
         (parts '())
         (xmin 0)
         (xmax 0)
         (ymin 0)
         (ymax 0))
    (with-open-file (stream path :direction :input)
        (loop for row from 0 
            for line = (read-line stream nil)
            while line do 
            (setf code (concatenate 'string code line))))
    (setf code (second (cl-ppcre:split ": " code)))
    (setf parts (cl-ppcre:split ", " code))
    (setf parts (map 'list (lambda (x) (subseq x 2)) parts))
    (setf xmin (parse-integer (first (cl-ppcre:split "\\.\\." (first parts)))))
    (setf xmax (parse-integer (second (cl-ppcre:split "\\.\\." (first parts)))))
    (setf ymin (parse-integer (first (cl-ppcre:split "\\.\\." (second parts)))))
    (setf ymax (parse-integer (second (cl-ppcre:split "\\.\\." (second parts)))))
    (format t "target area ~%x: (~a, ~a) y: (~a, ~a)~%" xmin xmax ymin ymax)
    (values (list xmin xmax) (list ymin ymax))))

(read-input t)

;; inp = 'target area: x=138..184, y=-125..-71'
;; x_min, x_max = [int(x) for x in inp.split(', ')[0].split('=')[1].split('..')] 
;; y_min, y_max = [int(x) for x in inp.split(', ')[1].split('=')[1].split('..')]
;; 
;; vx, vy, n = 0, 0, 0
;; run = 400 
;; starting_velocities = [] 
;; for vx in range(1, run): 
;;     vx_start = vx 
;;     for vy in range(-run, run): 
;;         vy_start = vy 
;;         k = vx 
;;         l = vy x,y = 0,0 
;;         for n in range(1,run): 
;;             x += k 
;;             k = k-1 if k>0 else k+1 if k<0 else k 
;;             y += l 
;;             l = l-1 
;;             if x_min <= x <= x_max and y_min <= y <= y_max:
;;                 starting_velocities.append((vx_start,vy_start))
;; 

(defun in-range (x y x-min x-max y-min y-max) 
  (and (and (>= x x-min) (<= x x-max)) 
           (and (>= y y-min) (<= y y-max))))


(defun get-starting-velocities (x-min x-max y-min y-max) 
    (let ((run 400)
          (starting-velocities '()))
        (loop for vx from 0 below run do 
              (format t "step ~a of ~a~%" vx run)
              (let ((vx-start vx))
                (loop for vy from (* -1 run) below run do 
                      (let ((vy-start vy)
                        (k vx)
                        (l vy)
                        (x 0)
                        (y 0))
                        (loop for n from 1 below run do 
                           (incf x k)
                           (if (> k 0) (decf k)
                           (if (< k 0) (incf k)))
                           (incf y l)
                           (setf l (- l 1))
                           (when (in-range x y x-min x-max y-min y-max)
                             (format t "we are pushing~%")
                             (push (list vx-start vy-start) starting-velocities)))))))
      starting-velocities))

(defun find-max-y (vs)
  (let ((maxi -100000000))
    (loop for v in vs do 
        (let* ((vx (first v))
               (vy (second v))
               (k vy)
               (x 0)
               (y 0))
          (loop while (> k 0) do 
                (setf y (+ y k))
                (decf k)
                (setf maxi (max maxi y)))))
    maxi))

(defun solve-one () 
  "Solve day 17 part one" 
  (multiple-value-bind (x-range y-range)
      (read-input nil)
    (let* ((x-min (first x-range))
           (x-max (second x-range))
           (y-min (first y-range))
           (y-max (second y-range))
           (starting-velocities (get-starting-velocities x-min x-max y-min y-max)))
      (format t "~a~%" (find-max-y starting-velocities)))))

(solve-one)

(defun solve-two ()
  "Solve day 17 part two"
  (multiple-value-bind (x-range y-range)
      (read-input nil)
    (let* ((x-min (first x-range))
           (x-max (second x-range))
           (y-min (first y-range))
           (y-max (second y-range))
           (starting-velocities (get-starting-velocities x-min x-max y-min y-max)))
      (format t "~a~%" (length (remove-duplicates starting-velocities :test #'equal))))))

(solve-two)
