#lang sicp

(define xs (list 4 2 391 394 29 19 31 1 384 ))

;; Simple Map
(define (map op coll)
  (if (null? coll)
      nil
      (cons
       (op (car coll))
       (map op (cdr coll)))))

;; Simple Filter
(define (filter op? coll)
    (cond ((null? coll) nil)
        ((op? (car coll)) (cons (car coll) (filter op? (cdr coll))))
        (else (filter op? (cdr coll)))))

;; Lazy Let-Filter
(define (l-filter op? coll)
  (let ((first (lambda nil (car coll)))
        (rest (lambda nil (cdr coll))))
    (cond ((null? coll) nil)
        ((op? (first)) (cons (first) (l-filter op? (rest))))
        (else (l-filter op? (rest))))))

;; If-Filter
(define (i-filter op? coll)
  (if (null? coll)
      nil
      (let ((first (car coll))
            (rest (i-filter op? (cdr coll))))
        (if (op? first) (cons first rest)  rest))))

;; Right Associative Reduce
(define (r-fold init op coll)
  (if (null? coll)
      init
      (op
       (car coll)
       (r-fold init op (cdr coll)))))

;; Map Using Right Associative Reduce
(define (rf-map op coll)
  (r-fold
   nil
   (lambda (first rest) (cons (op first) rest))
   coll))

;; Filter Using Right Associative Reduce
(define (rf-filter op? coll)
  (r-fold
   nil
   (lambda (first rest)
     (if (op? first) (cons first rest) rest))
   coll))

;; The 'kernel' used to define `rd-filter` below
(define (filter-k op?)
  (lambda (first rest)
     (if (op? first) (cons first rest) rest)))

;; How about a kernel for map?
;(define (filter-m op)...

;; Right Associative Transducer
(define (r-ducer kernel)
  (lambda (op coll)
    (r-fold
     nil
     (kernel op)
     coll)))

;; Filter - as Right Associative Transducer
(define rd-filter
  (r-ducer filter-k))

;; Is this a transducer? Take a reduction function(i.e. left or right-fold)
;; and a kernel to produce map, filter, etc...
(define (transducer reduce kernel)
  (lambda (op coll)
    (reduce
     nil
     (kernel op)
     coll)))

(define tdr-filter
  (transducer r-fold filter-k))

(define (add3 x) (+ 3 x))


