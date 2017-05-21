;
; Project Euler: Problem 1.
; Multiples of 3 and 5.
;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
;
; Alex Striff.
;

(define (filter f xs)
  (if (null? xs) xs
    (let ((head (car xs))
          (tail (filter f (cdr xs))))
      (if (f head) (cons head tail) tail))))

(define (incrange a b i)
  (let ((d (if (> a b) (- i) i)))
    (if (> a b) '() (cons a (incrange (+ a d) b i)))))

(define (range a b) (incrange a b 1))

(define (rangeto b) (incrange 1 (- b 1) 1))

(define (sum xs)
  (cond ((null? xs) 0)
        ((= (length xs) 1) (car xs))
        (else (+ (car xs) (sum (cdr xs))))))

(define (conds p)
  (cond ((null? p) #f)
        (((car p)) #t)
        (else (conds (cdr p)))))

(define (divides? m n)
  (define (div? m n k)
    (let ((q (* m k)))
      (cond ((> q n) #f)
            ((= q n) #t)
            (else (div? m n (+ k 1))))))
  (div? m n 0))


(sum (filter (lambda (n) (or (divides? 3 n) (divides? 5 n))) (rangeto 1000)))

