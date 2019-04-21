;; Chapter 2, part 1


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

;; (define (make-rat n d)
;;   (let ((g (gcd n d)))
;;     (cons (/ n g) (/ d g))))
;; + / -
;; + / +
;; - / +
;; - / - 

(define (make-rat n d)
  (let ((g (gcd n d))
	(s (if (negative? d) - +)))
	   (cons (s (/ n g))
		 (s (/ d g)))))	  

(define (numer x) (car x))

(define (denom x) (cdr x))
