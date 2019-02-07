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
; >>>> 1.9 here?



(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1))))); Linear Recursion - Note: the process builds
                                 ; up a chain of deferred operations

(define (factorial2 n)

  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n)
) ;, an iterative process is one whose state can be summarized by a fixed number of state variables,


 "As a consequence, these languages can describe iterative processes
  only by resorting to special-purpose “looping constructs” such as
  do, repeat, until, for, and while. e implementation of Scheme we
  shall consider in Chapter 5 does not share this defect."


; Exercise 1.9
(define (add0 a b)
  (if (= a 0)
      b
      (inc (add0 (dec a) b))))

;(add0 4 5)
;(inc (add0 3 5))
;(inc (inc (add0 2 5))))
;(inc (inc (inc (add0 1 5))))
;(inc (inc (inc (inc (add0 0 5)))))
(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

(define (add1 a b)
  (if (= a 0)
      b
      (add1 (dec a) (inc b))))

;(add1 4 5)
;(add1 3 6)
;(add1 2 7)
;(add1 1 8)
;(add1 0 9)
;9

; An aside - extensional vs intensional? 
(define (add2 a b)
  (if (= a 0)
      b
      (inc (add3 (dec a) b))))

(define (add3 a b)
  (if (= a 0)
      b
      (add2 (dec a) (inc b))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(= 1024 (A 1 10))
(= 65536 (A 2 4))
(= 65536 (A 3 3)) 


(define (f0 n) (A 0 n)) ; 2*n
(define (g n) (A 1 n))  ; 2^n
(define (h n) (A 2 n))  ; 2^(2^n)
(define (k n) (* 5 n n))

(= (g 65536) (h 5))
(= (g 16) (h 4))
(= (g 4) (h 3))

(= (h 4) (expt 2 (expt 2 (expt 2 2)))); 2^(2^(2^2) = 2^16
(= (h 3) (expt 2 (expt 2 2))) ; 2^4
(= (h 2) (expt 2 2)); 2^2

(define (check-h n)
  (= (h n) (expt 2 (h (- n 1)))))
; h(n+1) = 2^h(n)
; h(n) = 2^h(n-1)
; h(n+1) - 2^h(n) = 0
; h(n) - 2^h(n-1) = 0
; h(n+1) - 2^h(n) = h(n) - 2^h(n-1)
(define (h0 n) (if (= n 0) 1 (expt 2 (h0 (- n 1)))))


(define (fib n)
  (fib-iter 1 0 n))


(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;Tail recursive
;(fib 7)
;(fib-iter 1 0 7)
;(fib-iter (+ 1 0) 1 6)
;(fib-iter 1 1 6)
;(fib-iter (+ 1 1) 1 5)
;(fib-iter 2 1 5)
;(fib-iter (+ 2 1) 2 4)
;(fib-iter 3 2 4)
;(fib-iter (+ 3 2) 3 3)
;(fib-iter 5 3 3)
;(fib-iter (+ 5 3) 5 2)
;(fib-iter 8 5 2)
;(fib-iter (+ 8 5) 8 1)
;(fib-iter 13 8 1)
;(fib-iter (+ 13 8) 13 0)
;13

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1) ; If a is exactly 0, we should count that as 1 way to make change.
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount(- kinds-of-coins 1));the number of ways to change amount a using all but the first kind of coin
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)) 
              )))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Given 10 , 5 , 1 coins, number of ways to change 17
; - the number of ways to change amount a using all but the first kind of coin, plus
; - the number of ways to change amount a - d using all n kinds of coins, where d is the denomination of the first kind of coin.
; (num-ways 17 (10 , 5 , 1))
; (+ (num-ways 17 (5  1))                  (num-ways 7 ( 10 , 5 , 1)))
;    (+ (num-ways 17 (1) (num-ways 12 (1)) (+ (num-ways 7 (5 , 1) (num-ways -3 (5 , 1)))

; Alternate to above, using car and cdr
(define (change-ways amount denomxs)
  (cond ((= amount 0) 1) ; If a is exactly 0, we should count that as 1 way to make change.
        ((or (< amount 0) (null? denomxs)) 0)
        (else (+ (change-ways amount (cdr denomxs))
                 (change-ways (- amount (car denomxs)) denomxs)))))

(= (change-ways 100 '(50 25 10 5 1)) 292)

; Exercise 1.11
; f (n - 1) + 2f (n - 2) + 3f (n - 3)
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;(define (f-iterative n)
;  (iterate-f 0 n))

;(define (iterate-f p1 p2 p3 n)
; ....   (+ )
  ;)
