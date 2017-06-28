;;;;
;;;; Project Euler: Problem 9.
;;;; Special Pythagorean triplet.
;;;;
;;;;   A Pythagorean triplet is a set of three natural numbers, a < b < c, for
;;;;   which,
;;;;
;;;;   a^2 + b^2 = c^2
;;;;
;;;;   For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;;;;
;;;;   There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;;;;   Find the product abc.
;;;;
;;;; Alex Striff.
;;;;

;;;
;;; Euclid's formula for generating Pythagorean triples is:
;;;
;;;   a = m^2 - n^2
;;;   b = 2mn
;;;   c = m^2 + n^2
;;;
;;; Which implies m > n. This only results in a primitive triple if m is coprime
;;; to and has different parity to n.
;;;
;;; Given that (m^2 - n^2) + (2mn) + (m^2 + n^2) = 1000
;;; can be simplified to m (m + n) = 500, we must find integers m and n such
;;; that the previous equality is true. because m is an integer and that it
;;; is 500 / (m + n), m must be a factor of 500. Because m < m + n, m must also
;;; be less than sqrt(50). Because m + n must be greater than sqrt(50) and also
;;; remain a factor of 500, the smallest potential value of n is 5 (in the case
;;; that m=20, m+n = 25 > sqrt(500)) and the largest is 25 (m=1, m+n = 26 >
;;; sqrt(500)). All values in between must be a factor of 500 in the range of 25
;;; to 500 (i.e. the factor is greater than sqrt(500)). Adding the restriction
;;; that n < m, the range for n-values shrinks. For a maxial possible m=20, n
;;; must be less than 20, making 5 and 15 the only possible values for which
;;; m + n is a factor of 500 greater than 20 for the given possible values for m
;;; above.
;;;
;;; Outside of these range restrictions, the code will test if these restricted
;;; potential m- and n-values result in a pythagorean triple with aforementioned
;;; property that a + b + c = 1000.
;;;

;;; The +ms+ are all positive factors of 500 that are less than sqrt(500).
;;; This would be '(1 2 4 5 10 20), but applying that m > n, this is reduced to
;;; only '(10 and 20).
(defconstant +ms+ '(10 20))

;;; The +ns+ are all integers such that (m + n) is a factor of 500 above
;;; sqrt(500) for one the +ms+ above, and that 5 <= n < (max +ms+).
;;; 5 <= n because 5 is the smallest integer such that the above property holds.
;;; n < (max +ms+) because for m < sqrt(500) < m + n for natural numbers m and
;;; n, n must be less than m for all m in +ms+, thus (max +ms+).
(defconstant +ns+ '(5 15))

;;; With the above restrictions on the +ms+ and +ns+, the total number of
;;; permutations to test is reduced from infinitely many pythagorean triples to
;;; only (* (length +ms+) (length +ns+)), which is 12 (nearly instant to
;;; search!).

;;; Target triple sum.
(defconstant +target-sum+ 1000)

(defun mapcar-perms-h (f a b)
  (when a
    (cons (mapcar #'(lambda (x) (funcall f (car a) x)) b)
          (mapcar-perms-h f (cdr a) b))))

(defun mapcar-perms (f a b)
  (when (and b a)
    (reduce #'append (mapcar-perms-h f a b))))

(defun triple-target-p (m n)
  (let* ((mm (* m m))
         (nn (* n n))
         (a  (- mm nn))
         (b  (* 2 m n))
         (c  (+ mm nn)))
    (list (= (+ a b c) +target-sum+)
          m
          n)))

(defun euclid-triple (m n)
  (let* ((mm (* m m))
         (nn (* n n))
         (a  (- mm nn))
         (b  (* 2 m n))
         (c  (+ mm nn)))
    (list a b c)))

(defparameter *candidates* (mapcar-perms #'triple-target-p +ms+ +ns+))

(defparameter *results*
  (mapcar #'cdr
          (remove-if-not #'(lambda (x) (first x)) *candidates*)))

(defparameter *natural-result*
    (car (remove-if-not
      #'(lambda (x)
          (and (> (first x) (second x))
               (every #'(lambda (y) (> y 0)) x)))
      *results*)))

(defparameter *result-triple*
  (apply #'euclid-triple *natural-result*))

(defparameter *triple-product*
  (apply #'* *result-triple*))

(print
  *triple-product*)

