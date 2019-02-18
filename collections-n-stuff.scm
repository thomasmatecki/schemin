#lang sicp

(define xs (list 4 2 391 394 29 19 31 1 384 ))

;; Simple Map
(define (map op ls)
  (if (null? ls)
      nil
      (cons
       (op (car ls))
       (map op (cdr ls)))))

;; Simple Filter
(define (filter op? ls)
    (cond ((null? ls) nil)
        ((op? (car ls)) (cons (car ls) (filter op? (cdr ls))))
        (else (filter op? (cdr ls)))))

;; Lazy Let-Filter
(define (lz-filter op? ls)
  (let ((first (lambda nil (car ls)))
        (rest (lambda nil (cdr ls))))
    (cond ((null? ls) nil)
        ((op? (first)) (cons (first) (lz-filter op? (rest))))
        (else (lz-filter op? (rest))))))

;; If-Filter
(define (i-filter op? ls)
  (if (null? ls)
      nil
      (let ((first (car ls))
            (rest (i-filter op? (cdr ls))))
        (if (op? first) (cons first rest)  rest))))

;; Iterative All
(define (all ls)
  (or (null? ls)
      (and (car ls)
           (all (cdr ls)))))

;; Right Associative Reduce
(define (r-fold init op ls)
  (if (null? ls)
      init
      (op
       (car ls)
       (r-fold init op (cdr ls)))))


;; Map Using Right Associative Reduce
(define (rf-map op ls)
  (r-fold
   nil
   (lambda (first rest) (cons (op first) rest))
   ls))

;; Filter Using Right Associative Reduce
(define (rf-filter op? ls)
  (r-fold
   nil
   (lambda (first rest)
     (if (op? first) (cons first rest) rest))
   ls))

;; All Using Right Associative Reduce
(define (rf-all op? ls)
  (r-fold
   #t
   (lambda (first rest) (and (op? first) rest))
   ls))

;; The 'kernel' used to define `rd-filter` below
(define (filter-k op?)
  (lambda (first rest)
     (if (op? first) (cons first rest) rest)))

;; ... a kernel for map ...
(define (map-k op)
  (lambda (first rest) (cons (op first) rest)))

(define (all-k op?)
   (lambda (first rest)
     (and (op? first) rest)))

;; Reduce-Right using `kernel`
(define (ls-ducer kernel)
  (lambda (op ls)
    (r-fold
     nil
     (kernel op)
     ls)))

;; Filter - as Right Associative Transducer
(define rd-filter
  (ls-ducer filter-k))

;; Map - as Right Associative Transducer
(define rd-map
  (ls-ducer map-k))

;; Take a reduction function(i.e. left or right-fold)
;; and a kernel...
(define (over reduce kernel zero)
  (lambda (op ls)
    (reduce
     zero
     (kernel op)
     ls)))

;; ... create Filter ...
(define o-filter
  (over r-fold filter-k nil))

;; ... Map ...
(define o-map
  (over r-fold map-k nil))

;; ... All ...
(define o-all
  (over r-fold all-k #t))


(define (add3 x) (+ 3 x))


(#%provide )