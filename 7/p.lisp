;;;;
;;;; Project Euler: Problem 7.
;;;; 10001st prime.
;;;;
;;;;   By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can
;;;;   see that the 6th prime is 13.
;;;;
;;;;   What is the 10 001st prime number?
;;;;
;;;; Alex Striff.
;;;;

;;;
;;; Because all primes above 3 are of the form 6*i +/- 1, numbers (n) above 4
;;; are checked for primality by dividing them by
;;;   d = 6*k +/- 1,
;;;     for all k such that 4 < d < sqrt(n).
;;;
;;; The search space is also reduced by only checking numbers of the previous
;;; form, instead of just iterating through all positive integers.
;;;

;;; If one knew the number of primes below a given number, *nprimes* and
;;; +startmult+ could be set to reduce the search space.
(defparameter *prime* nil)
(defparameter *nprimes* 2) ; Start past 2 and 3.
(defconstant +endprime+ 10001)
(defconstant +startmult+ 1)

(defun divides? (m n)
  (= (mod n m) 0))

(defun trial-div-p (n &optional (f 5))
  "Check if a number is divisible by 6*k +/- 1, k > 0"
  (let ((r (sqrt n)))
    (if (> f r) t
      (if (or (divides? f n) (divides? (+ f 2) n))
        nil (trial-div-p n (+ f 6))))))

(defun prime-p (n)
  "When n is prime, add n to the list of primes (t), otherwise nil."
  ;; A number is prime if it is not divisible by 2, 3, or 6*k +/- 1, k > 0.
  (when (and (not (divides? 2 n)) (not (divides? 3 n)) (trial-div-p n))
    (setf *prime* n)))

;;; Keep generating primes until +endprime+ is reached.
(do ((i +startmult+ (1+ i)))
  ;; Iterate through all primes of the for 6*i +/- 1.
  ((>= *nprimes* +endprime+))
  (when (prime-p (1- (* 6 i)))
    (incf *nprimes*))
  (unless (>= *nprimes* +endprime+) ; Skip the (n+1)st prime if n is +endprime+.
    (when (prime-p (1+ (* 6 i)))
      (incf *nprimes*))))

(print *prime*)

