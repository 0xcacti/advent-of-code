(defpackage :advent-of-code/utils
  (:use :cl :cl-ppcre)
  (:nicknames :aoc/utils)
  (:export 
    #:manhattan-distance
    #:neighbors-4
    #:neighbors-8
    #:get-year-day-from-package
    #:with-input))
