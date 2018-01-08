;;;;
;;;; Project Euler: Problem 10.
;;;; Summation of primes.
;;;;
;;;;   The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;;;
;;;;   Find the sum of all the primes below two million.
;;;;
;;;; Alex Striff.
;;;;

;;;
;;; Simply use the Sieve of Erastosthenes, implemented as a lazy infinite stream,
;;; to generate the primes and sum them.
;;;

(defmacro lazy (x)
  `(lambda () ,x))

(defun force (x)
  (if (functionp x)
      (funcall x)
      x))

(defmacro lcons (x y)
  `(lazy (cons ,x ,y)))

(defmacro with-lcons (x a b &body body)
  `(when ,x
     (let* ((cons (force ,x))
	    (,a (car cons))
	    (,b (cdr cons)))
       ,@body)))

(defun llist (&rest args)
  (when args
    (lcons (car args) (apply #'llist (cdr args)))))

(defun take (n xs)
  (when (and xs (plusp n))
      (with-lcons xs head tail
	(cons head (take (1- n) tail)))))

(defun take-all (xs)
  (with-lcons xs head tail
    (cons head (take-all tail))))

(defun replicate (x)
  (lcons x (replicate x)))

(defun iterate (f x)
  (lcons x (iterate f (funcall f x))))

(defun lmapcar (f xs)
  (with-lcons xs head tail
    (lcons (funcall f head) (lmapcar f tail))))

(defun lreduce (f xs)
  (with-lcons xs head tail
    (or
     (with-lcons tail beg end
       (lreduce f (lcons (funcall f head beg) end)))
     head)))

(defun lreducen (f xs n)
  (with-lcons xs head tail
    (or
     (and (plusp (1- n))
	  (with-lcons tail beg end
	    (lreducen f (lcons (funcall f head beg) end) (1- n))))
     head)))

(defun id (x) x)

(defun compose (g f)
  (lambda (&rest args) (funcall g (apply f args))))

(defun compose* (&rest args)
  (cond ((null args) #'id)
	((not (cdr args)) (car args))
	((= (length args) 2) (apply #'compose args))
	(t (compose (car args) (apply #'compose* (cdr args))))))

(defun curry (f &rest args)
  (lambda (&rest extargs) (apply f (append args extargs))))

(defun flip (f)
  (lambda (x y) (funcall f y x)))

(defun flip* (f)
  (lambda (x &rest args) (apply f (append args (cons x nil)))))

(defun div? (n)
  (compose #'zerop (curry (flip #'mod) n)))

(defun lfilter (p xs)
  (with-lcons xs head tail
    (if (and head (funcall p head))
	(lcons head (lfilter p tail))
	(lfilter p tail))))

(defun lfilter* (p xs)
  (with-lcons xs head tail
    (when (and head (funcall p head))
      (lcons head (lfilter* p tail)))))

(defun era (xs)
  (with-lcons xs head tail
    (lcons head (era (lfilter (complement (div? head)) tail)))))

(defparameter *primes* (era (iterate #'1+ 2)))

(defun primes-below (n)
  (lfilter* (curry #'> n) *primes*))

;;; Real Sieve of Erastosthenes implementation.

(defun make-gen (init f)
  (cons init f))

(defun gen-car (gen)
  (car gen))

(defun gen-cdr (gen)
  (let ((generator (cdr gen)))
    (make-gen (funcall generator (gen-car gen)) generator)))

(defun make-composite (p)
  (make-gen (* p p) (curry #'+ p)))

(defun insert-composite (p composites)
  (append composites (list (make-composite p))))

(defparameter *candidates* (make-gen 3 (curry #'+ 2)))

(defun composite-lookup (candidate composites)
  (remove-if-not (compose (div? candidate) #'gen-car) composites))

(defun iterate-composites (composites found)
  (append composites (mapcar #'gen-cdr found)))

(defun real-era (xs &optional composites)
  (unless (null xs)
    (let* ((candidate (gen-car xs))
	   (rest (gen-cdr xs))
	   (found (composite-lookup candidate composites)))
      (if found
	  (real-era rest (iterate-composites composites found))
	  (lcons candidate
		 (real-era rest (insert-composite candidate composites)))))))

(print
 42
 ;(lreduce #'+ (primes-below 2000000))
 )

