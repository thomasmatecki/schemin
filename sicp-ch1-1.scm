#lang sicp

(#%require
 "collections-n-stuff.scm"
 "testin.scm")

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

(sum-of-largest-two-squares 1 2 3)
(sum-of-largest-two-squares 2 3 2)
(sum-of-largest-two-squares 3 2 1)


(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))  ;Exercise 1.4 -
; IF b is greater than zeros THEN add a to b ELSE subtract b from a,
; I think the same as:
(define (a-plus-abs-b-2 a b)
  (+ a (abs b)))


; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;(test 0 (p))
; applicative-order evaluation - "percolate values upward"
;  (test 0 (p))
;  (test 0 (p))
;  (test 0 (p))
;... (p) always evaluates to (p) ad-infinitum.
; normal-order evaluation - "fully expand and then reduce"
;  (test 0 (p))
;  (if (= 0 0) 0 (p)) <- Expand
;  (if True 0 (p)) <- Reduce
;  0

; I find the 'laziness' concept to be a bit more intuitive(https://courses.cs.washington.edu/courses/cse505/99au/functional/applicative-normal.pdf).

    "The contrast between function and procedure is a reflection of the
     general distinction between describing properties of things and
     describing how to do things, or, as it is sometimes referred to, the
     distinction between declarative knowledge and imperative knowledge.
     In mathematics we are usually concerned with declarative (what is)
     descriptions, whereas in computer science we are usually concerned
     with imperative (how to) descriptions."

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)) ; Is the difference between squaring our guess and x less than 0.001?

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Exercise 1.6 - `new-if is applicatively evaluated, whereas `if isn't?
; (if true 1 (p)) is 1 whereas (new-if true 1 (p)) evals forever. In other words 'new-if doesn't "Short-circuit".
; Looking at our `sqrt-iter, the same applies
; (define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x) x)))
; Will run for ever - continually evaluating 
;          (sqrt-iter (improve guess x) x))


; Exercise 1.7
; (square (sqrt .0000001))
; > 0.000976629102245155

(define (changed-alot? curr prev)
  (> (abs (- curr prev)) .0001))

(define (sqrt-iter2 guess last-guess x)
  (if (not (changed-alot? guess last-guess ))
      guess
      (sqrt-iter2 (improve guess x) guess x)))

(define (sqrt2 x) (sqrt-iter2 1 0 x))


; Exercise 1.8 (x/y^2+2y) / 3
 
(define (cbrt-iter y prev-y x)
  (if (not (changed-alot? y prev-y))
      y
      (cbrt-iter (/ (+ (/ x (square y)) (* 2 y)) 3) y x)))
