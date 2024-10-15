;; Rules
;; If any pair is nested inside four pairs, the leftmost such pair explodes.
;; If any regular number is 10 or greater, the leftmost such regular number splits.
;;  During reduction, at most one action applies, after which the process returns 
;;  to the top of the list of actions. For example, 
;;  if split produces a pair that meets the explode criteria, 
;;  that pair explodes before other splits occur.
