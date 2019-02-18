#lang sicp
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

(define (fib-r n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-r (- n 1))
                 (fib-r (- n 2))))))

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
  (cond ((null? amount) '())
        ((= amount 0) 1) ; If a is exactly 0, we should count that as 1 way to make change.
        ((or (< amount 0) (null? denomxs)) 0)
        (else (+ (change-ways amount (cdr denomxs))
                 (change-ways (- amount (car denomxs)) denomxs)))))

(= (change-ways 100 '(50 25 10 5 1)) 292)

; Exercise 1.11
; f(n - 1) + 2f(n - 2) + 3f(n - 3)
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;f(2) f(3) f(4) f(5)   f(6)  f(7)  f(8)
;  2  66   130  218    330   466   626
;   64  64    64    112   136   160
;     0    24    24    24    24

; So, for n > 2 initialize:
; a <- 66
; b <- 0
; ... Iterate (n - 2) times:
; a <- (a + 64 + b)
; b <= (b + 24)

(define (iterate-f a b n)
  (if (= n 2)
      a
      (iterate-f
       (+ a 64 b)
       (+ b 24)
       (dec n))))

(define (f-iterative n)
  (if (< n 3)
      n
      (iterate-f 66 0 (dec n))))

(define (f-test n)
  (= (f-recursive n)
     (f-iterative n)))

(is-it-true?
 "f-recursive and f-iterative produce the same result for n=0...99"
 (all (o-map f-test (range-asc 0 100))))

(define (pascal-recur rs ls)
  (if (null? xs)
      '(1)))

(define ps '(1 3 3 1))

(pascal-recur)

; Exercise 1.12
(define (next-pascals last)

  )