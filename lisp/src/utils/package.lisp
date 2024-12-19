(defpackage :advent-of-code/utils
  (:use :cl :cl-ppcre)
  (:nicknames :aoc/utils)
  (:export 
    ;; math exports
    #:manhattan-distance
    #:neighbors-4
    #:neighbors-8

    ;; file reading exports
    #:get-year-day-from-package
    #:with-input

    ;; queue exports
    #:queue     
    #:make-queue    
    #:queue-p       
    #:queue-elements
    #:queue-head
    #:queue-tail
    #:enqueue       
    #:dequeue
    #:queue-empty-p))
