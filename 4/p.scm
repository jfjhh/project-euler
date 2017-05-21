;
; Project Euler: Problem 4.
; Largest Palindrome Product.
;
;; A palindromic number reads the same both ways. The largest palindrome made
;; from the product of two 2-digit numbers is 9009 = 91 × 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.
;
; Alex Striff.
;

; Because the palindrome (P) is of the form abccba, it can be written as:
;
; P
; = (100000a + 10000b + 1000c + 100c + 10b + 1a)
; = (100001a + 10010b + 1100c)
; = (  9091a +   910b +  100c) * 11.
;
; So at least one of the three-digit factors must be divisible by 11.
; This is used to speed up the search through the entire space.
;

(define (first n)
  (let ((q (quotient n 10)))
    (if (= q 0) n (first q))))

(define (last n)
  (modulo n 10))

(define (digits n)
  (if (= 0 n) 0 (+ 1 (digits (quotient n 10)))))

(define (shrink n)
  (define (shrink* n)
    (quotient (modulo n (expt 10 (- (digits n) 1))) 10))
  (let ((s (shrink* n)))
    (if (= (- (digits n) (digits s)) 2)
      s
      (if (= (modulo s 10) 0) (quotient s 10) 1337))))

(define (even n)
  (= (modulo n 2) 0))

(define (palindrome? n)
  (cond ((< n 10) #t)
        ((and (= (first n) (last n))
              (even (- (digits n) (digits (shrink n)))))
         (palindrome? (shrink n)))
        (else #f)))

(define (pals a b m)
  (if (and (<= a 100) (<= b 100))
    m
    (let ((p (if (< a 100) 999 (- a 1)))
          (q (if (< a 100) (- b 11) b))
          (c (* a b)))
      (pals p q (if (and (> c m) (palindrome? c)) c m)))))


(pals 999 990 0)

