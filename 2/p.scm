;
; Project Euler: Problem 2.
; Even Fibonacci Numbers.
;
;; Each new term in the Fibonacci sequence is generated by adding the previous
;; two terms. By starting with 1 and 2, the first 10 terms will be:
;;
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;
;; By considering the terms in the Fibonacci sequence whose values do not exceed
;; four million, find the sum of the even-valued terms.
;
; Alex Striff.
;

(define (filter f xs)
  (if (null? xs) xs
    (let ((head (car xs))
          (tail (filter f (cdr xs))))
      (if (f head) (cons head tail) tail))))

(define (sum xs)
  (cond ((null? xs) 0)
        ((= (length xs) 1) (car xs))
        (else (+ (car xs) (sum (cdr xs))))))

(define (even? n)
  (= (modulo n 2) 0))

(define (fibs n)
  (define (fibs* n i l)
    (if (or (and (not (null? l)) (>= (car l) 4000000)) (< n i))
      l (let ((fib (cond ((= i 0) 0)
                         ((or (= i 1) (= i 2)) i)
                         (else (+ (car l) (cadr l))))))
          (fibs* n (+ i 1) (cons fib l)))))
  (reverse (cdr (fibs* n 1 '()))))

(sum (filter even? (fibs 1337)))

