;;;;
;;;; Project Euler: Problem 5.
;;;; Smallest Multiple.
;;;;
;;;;   2520 is the smallest number that can be divided by each of the numbers
;;;;   from 1 to 10 without any remainder.
;;;;  
;;;;   What is the smallest positive number that is evenly divisible by all of
;;;;   the numbers from 1 to 20?
;;;;
;;;; Alex Striff.
;;;;

;;;
;;; For a number to be divisible in a larger number, all of its prime factors must
;;; be present in the product of prime factors that composes the larger number.
;;;
;;; Starting at 1, multiply integers as prime factorizations together, only
;;; writing new numbers as necessary. It makes more sense when you look at the
;;; series of prime factors needed to be divisible into N.
;;;
;;; TL;DR At step k, add only prime factors that cannot already be found in the
;;; previous series, i.e. primes or some missing prime factor.
;;;
;;; 01: 1                               ; Start at 1.
;;; 02: 1 2                             ; Add prime.
;;; 03: 1 2 3                           ; Add prime.
;;; 04: 1 2 3 2                         ; Added 2, so 2 * (previous 2) can make 4.
;;; 05: 1 2 3 2 5                       ; Add prime.
;;; 06: 1 2 3 2 5                       ; No need to change anything.
;;; 07: 1 2 3 2 5 7                     ; Add prime.
;;; 08: 1 2 3 2 5 7 2                   ; Added 2 to multiply two existing 2's.
;;; 09: 1 2 3 2 5 7 2 3                 ; Added 3 to multiply previous 3.
;;; 10: 1 2 3 2 5 7 2 3                 ; No need to change anything.
;;; 11: 1 2 3 2 5 7 2 3 11              ; Add prime.
;;; 12: 1 2 3 2 5 7 2 3 11              ; No need to change anything.
;;; 12: 1 2 3 2 5 7 2 3 11              ; No need to change anything.
;;; 13: 1 2 3 2 5 7 2 3 11 13           ; Add prime.
;;; 14: 1 2 3 2 5 7 2 3 11 13           ; No need to change anything.
;;; 15: 1 2 3 2 5 7 2 3 11 13           ; No need to change anything.
;;; 16: 1 2 3 2 5 7 2 3 11 13 2         ; Added 2 to get 2^4.
;;; 17: 1 2 3 2 5 7 2 3 11 13 2 17      ; Add prime.
;;; 18: 1 2 3 2 5 7 2 3 11 13 2 17      ; No need to change anything.
;;; 19: 1 2 3 2 5 7 2 3 11 13 2 17 19   ; Add prime.
;;; 20: 1 2 3 2 5 7 2 3 11 13 2 17 19   ; No need to change anything.
;;;

(defun any? (f xs)
  (cond ((null xs) nil)
        ((funcall f (car xs)) t)
        (t (any? f (cdr xs)))))

(defun incrange (a b i)
  (let ((d (if (> a b) (- i) i)))
    (if (> a b) '() (cons a (incrange (+ a d) b i)))))

(defun range (a b) (incrange a b 1))

(defun div_check (s n)
  (not (any? (lambda (x) (> (mod s x) 0)) (range 1 n))))

(defconstant *smalldiv20* (* 1 2 3 2 5 7 2 3 11 13 2 17 19))

(print
  (if (div_check *smalldiv20* 20) *smalldiv20* 42))

