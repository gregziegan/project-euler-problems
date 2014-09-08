
(defun calculate-mult-3-or-5 (sum current-number target)
  (cond
    ((= current-number target) sum)
    ((or (= (mod current-number 3) 0) (= (mod current-number 5) 0))
     (calculate-mult-3-or-5 (+ sum current-number) (1+ current-number) target))
    (t (calculate-mult-3-or-5 sum (1+ current-number) target)))) 
