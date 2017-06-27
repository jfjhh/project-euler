;;;;
;;;; Project Euler: Problem 1.
;;;; Multiples of 3 and 5.
;;;;
;;;;   If we list all the natural numbers below 10 that are multiples of 3 or
;;;;   5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;;;
;;;;   Find the sum of all the multiples of 3 or 5 below 1000.
;;;;
;;;; Alex Striff.
;;;;

(defun filter (f xs)
  (if (null xs) xs
    (let ((head (car xs))
	  (tail (filter f (cdr xs))))
      (if (funcall f head) (cons head tail) tail))))

(defun incrange (a b i)
  (let ((d (if (> a b) (- i) i)))
    (if (> a b) '() (cons a (incrange (+ a d) b i)))))

(defun range (a b) (incrange a b 1))

(defun rangeto (b) (incrange 1 (- b 1) 1))

(defun sum (xs)
  (cond ((null xs) 0)
	((= (length xs) 1) (car xs))
	(t (+ (car xs) (sum (cdr xs))))))

(defun conds (p)
  (cond ((null p) nil)
	((car p) t)
	(t (conds (cdr p)))))

(defun div? (m n k)
  (let ((q (* m k)))
    (cond ((> q n) nil)
	  ((= q n) t)
	  (t (div? m n (+ k 1))))))

(defun divides? (m n)
  (div? m n 0))


(print
  (sum (filter (lambda (n) (or (divides? 3 n) (divides? 5 n))) (rangeto 1000))))

