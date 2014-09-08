
(defparameter *known-primes* '(5 3 2))

(defun find-prime-factors (num target prime-factors)
  (cond
    ((> num target) prime-factors)
    ;((> num target) "There are no prime factors")
    ((and (is-number-prime num) (= (mod target num) 0)) (find-prime-factors (1+ num) target (cons num prime-factors)))
    (t (find-prime-factors (1+ num) target prime-factors))))


(defun is-number-prime (num)
  (let ((highest-known-prime (car (last *known-primes*))))
    (cond
      ((is-new-prime num *known-primes* 5 highest-known-prime)
        (progn 
          (setf *known-primes* (cons num *known-primes*))
          t))
      (t nil))))


(defun is-new-prime (candidate prime-list divisor highest-known-prime)
  (let* ((current-prime (car prime-list))
         (upper-bound (ceiling (/ highest-known-prime divisor))))
    (cond
      ((eq current-prime nil) t)
      ((= (mod candidate current-prime) 0) nil)
      ((or (and (= divisor 1) (eq (cdr prime-list) nil))
           (and (= divisor 1) (> current-prime upper-bound))) t)
      ((> current-prime upper-bound) (is-new-prime candidate (cdr prime-list) (1- divisor) highest-known-prime))
      (t (is-new-prime candidate (cdr prime-list) divisor highest-known-prime)))))
