(in-package :advent-of-code/utils)

(defun get-year-day-from-package ()
  "Extract year and day from current package name e.g., :advent-of-code/2024/01"
  (let* ((pkg-name (package-name *package*))
         (parts (cl-ppcre:split "/" pkg-name)))
    (when (= (length parts) 3)  ; Should be "advent-of-code", "2024", "01"
      (values (second parts) (third parts)))))

(defmacro with-input ((var &key (test nil)) &body body)
  `(multiple-value-bind (year day) (get-year-day-from-package)
     (let* ((base-path (uiop:getcwd))
            (input-file (if ,test "test-input.txt" "input.txt"))
            (relative-path (format nil "src/years/~A/~A/~A" year day input-file))
            (,var (merge-pathnames relative-path base-path)))
       ,@body)))
