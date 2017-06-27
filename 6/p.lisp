;;;;
;;;; Project Euler: Problem 6.
;;;; Sum Square Distance.
;;;;
;;;;   The sum of the squares of the first ten natural numbers is,
;;;;  
;;;;   1^2 + 2^2 + ... + 10^2 = 385
;;;;  
;;;;   The square of the sum of the first ten natural numbers is,
;;;;  
;;;;   (1 + 2 + ... + 10)^2 = 55^2 = 3025
;;;;  
;;;;   Hence the difference between the sum of the squares of the first ten
;;;;   natural numbers and the square of the sum is 3025 − 385 = 2640.
;;;;  
;;;;   Find the difference between the sum of the squares of the first one
;;;;   hundred natural numbers and the square of the sum.
;;;;
;;;; Alex Striff.
;;;;

;;;
;;; The sum of the natural numbers from 1 to n is:
;;;
;;; T_n = n * (n + 1)
;;;       -----------
;;;            2     .
;;;
;;; So the square of the sum is:
;;;
;;; (T_n)^2 = (T_n * T_n).
;;;
;;; From pounding inductive arguments, the sum of the squares is:
;;;
;;;                 /   n                  \
;;;                 | .---                 |
;;; (3 * T_n) + 2 * |  \   i * (n - i - 1) |
;;;                 |  /                   |
;;;                 | '---                 |
;;;                 \  i=0                 /
;;;
;;;         / 2 * n + 1 \
;;; = T_n * | --------- |
;;;         \     3     /.
;;;
;;; So the square of the sum minus the sum of the squares is:
;;;
;;;                 / 2 * n + 1 \
;;; (T_n)^2 - T_n * | --------- |
;;;                 \     3     /.
;;;

(defun diff (n)
  (defun triangle (n)
    (floor (* n (+ n 1)) 2))
  (let ((tn (triangle n)))
    (- (* tn tn)
       (* tn (+ 3 (floor (* 2 (- n 4)) 3))))))

(print
  (diff 100))

