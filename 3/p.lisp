;;;;
;;;; Project Euler: Problem 3.
;;;; Largest Prime Factor.
;;;;
;;;;   The prime factors of 13195 are 5, 7, 13 and 29.
;;;;  
;;;;   What is the largest prime factor of the number 600851475143 ?
;;;;
;;;; Alex Striff.
;;;;

(defun maxfactor (n)
  (defun maxfactor* (n i)
    (cond ((= i n) n)
	  ((= (mod n i) 0) (maxfactor* (floor n i) 2))
	  (t (maxfactor* n (+ i 1)))))
  (maxfactor* n 2))

(print
  (maxfactor 600851475143))

