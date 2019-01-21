#lang sicp

(define x 3) ; x is 3

(define (square x) (* x x))

(square 21)

(square (+ 2 5))

(define (sum-of-squares x y) (+ (square x) (square y)))

(sum-of-squares 2 3)

(define (f a)
  (sum-of-squares (+ a 1) (+ a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 1
                  5)))))
   (* 3
      (- 6
         2)
      (- 2
         7))) ;Exercise 1.2

(define (sum-of-largest-two-squares a b c)
  (define ac (< a c))
  (cond
    ((and (< a b) ac) (sum-of-squares b c))
    ((not ac) (sum-of-squares a b))
    (else (sum-of-squares a c)))) ;Excercise 1.3


