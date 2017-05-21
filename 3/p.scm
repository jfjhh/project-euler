;
; Project Euler: Problem 3.
; Largest Prime Factor.
;
;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?
;
; Alex Striff.
;

(define (maxfactor n)
  (define (maxfactor* n i)
    (cond ((= i n) n)
          ((= (modulo n i) 0) (maxfactor* (quotient n i) 2))
          (else (maxfactor* n (+ i 1)))))
  (maxfactor* n 2))

(maxfactor 600851475143)

