
(defun even-term-sum (sum prev-term cur-term target)
  (let ((next-term (+ prev-term cur-term)))
    (cond
      ((> cur-term target) sum)
      ((= (mod cur-term 2) 0) (even-term-sum (+ sum cur-term) cur-term next-term target))
      (t (even-term-sum sum cur-term next-term target)))))
