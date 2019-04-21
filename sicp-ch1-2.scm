;;(load "collections-n-stuff.scm")
;;(load "testin.scm") 
; Ex. 1.9 
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; (+ 4 5)
;; (if (= 4 0) 5 (inc (+ (dec 4) 5))))
;; (if #t 5 (inc (+ 3 5))))
;; (inc (+ 3 5)))
;; (inc (if (= 3 0) 5 (inc (+ (dec 3) 5))))
;; (inc (inc (+ 2 5)))
;; ...
;; (inc (inc (inc (+ 1 5)))
;; (inc (inc (inc (inc (+ 0 5))))))
;; 9 <- this is recursive

;;(define (+ a b)
;;  (if (= a 0)
;;      b
;;      (+ (dec a) (inc b)))) <- iterative?

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

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
  )

;; an iterative process is one whose state can be summarized by a
;; fixed number of state variables,



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
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

;; (define (add1 a b)
;;   (if (= a 0)
;;       b
;;       (add1 (dec a) (inc b))))

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

; Alternate to above, using car and cdr and in coins.scm
; Exercise 1.11
; f(n - 1) + 2f(n - 2) + 3f(n - 3)
; http://oeis.org/search?q=0%2C1%2C2%2C4%2C11%2C25%2C59%2C142%2C335&sort=&language=english&go=Search
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))


; f(0) = 0
; f(1) = 1
; f(2) = 2
; f(3) = f(2) + 2f(1) + 3f(0) = 1f(2) + 2f(1) + 0 = 4
; f(4) = f(3) + 2f(2) + 3f(1) = 1f(2) + 2f(1) + 3f(0) + 2f(2) + 3f(1)
;                             = 3f(2) + 5f(1) + 3f(0) = 11
; f(5) = f(4) + 2f(3) + 3f(2) = 3f(2) + 5f(1) + 3f(0) + 2f(2) + 4f(1) + 6f(0) + 3f(2)
;                             = 8f(2) + 9f(1) + 9f(0) = 25 
; f(6) = f(5) + 2f(4) + 3f(3) = 8f(2) + 9f(1) + 9f(0) + 6f(2) + 10f(1) + 6f(0) + 3f(2) + 6f(1)
;                             = 17f(2) + 25f(1) + 15f(0) = 59
; f(7) = f(6) + 2f(5) + 3f(4) = 17f(2) + 25f(1) + 16f(2) + 18f(1) + 9f(2) + 15f(1)
;                             = 42f(2) + 58f(1) = 44 + 58 = 142

 
; a 1 3 8 17 42
; b 2 5 9 25 58 ..?
; 2
; 5  = 2 +              [3]
; 9  = 2 + 3 +          [4]
; 25 = 2 + 3 + 4 +      [10 + 6]
; 58 = 2 + 3 + 4 + 10 + [6 + 18 + 16]
; a_{n+1} <- a_{n} + b_{n}
; b_{n+1} <- ???


; a <- 2b + a + c
; b <- 2 * a
; c <- 3 * b

; n 0 1 2  3  4  5
; +-------------------------
; a 2 4 11 25 59 142 335
; b 1 2 4  11 25 59  142 335
; c 0 1 2  4  11 25  59  142 335

(define (iterate-f a b c n)
  (cond
    ((< n 2) n)
    ((= n 2) a)
    (else (iterate-f
          (+ a (* 2 b) c)
          a
          (* 3 b)
          (dec n)))))

(define (f-iterative n)
  (iterate-f 2 1 0 n))

(define (f-test n)
  (= (f-recursive n)
     (f-iterative n)))

;(is-it-true?
; "f-recursive and f-iterative produce the same result for n=0...19"
; (all (o-map f-test (range-asc 0 20))))

; + r
; c
;   1
;   1 1
;   1 2 1

(define (pascal r c)
  (if (or (= 1 r) (= r c))
      1
      (+ (pascal (dec r) c)
         (pascal (dec r) (dec c) ))))

; Ex 1.15
(define (cube x)
  (* x x x))

(define (p x)
  (display "p called\n")
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; a) 5 times
; b) log(a)?

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

(define (square n) (* n n))

; i <- a ; initialize a to 1
; iterate if n > 0:
;     if n is odd:
;        a <- a * b
;        n <-  n - 1 ; make n even
;     otherwise:
;        n <- n/2
;        b <- b^2
; so... ab^n = (ab)b^{n-1}          <- our "invariant quantity"
;       ab^n = a(b^2)^{n/2}
; E.g: 6^27 -
; b = 6, n=27, a=1 
; 

; Exercise 1.16
(define (fast-expt-iterate b n a)
  (cond
    ((= n 0) a)
    ((odd? n) (fast-expt-iterate b (dec n) (* a b)))    ; ab^{n} = (ab)b^{n-1}
    (else (fast-expt-iterate (square b) (/ n 2) a))))   ; ab^{n} = a(b^2)^{n/2}

(define (iterative-fast-expt b n)
  (fast-expt-iterate b n 1))

; Excercise 1.17
(define (half i) (/ i 2))
(define (double i) (+ i i))

(define (mult a b)
  (if (zero? a)
      0
      (if (even? a)
	  (double (mult (half a) b))   ; 2 * ( (a/2) b) = ab
	  (+ (mult (dec a) b) b))))    ; (a - 1)b + b   = ab
	  
; Exercise 1.18
; Again, think of an "invariant quantity" ....
;       a + bc = (a + c) + (b - 1)c
;              = a + (b/2)(2*c) = a + (b/2)(c + c)
; ... half or decrement b until it is 1...
(define (iterate-fast-mult a b c)
  (cond
    ((= b 1) (+ a c))
    ((odd? b) (iterate-fast-mult (+ a c) (dec b) c))
    (else (iterate-fast-mult a (/ b 2) (+ c c)))))

(define (fast-mult a b)
  (iterate-fast-mult 0 a b))

; Ex. 1.19
;; T: a <- a + b
;;    b <- a
;; If a_0 = 1, b_0 =0, then T^n is the Fibonacci sequence.
;;
;; Now consider T to be the special case T_{0 1} :
;;     a <- b(1) + a(1) + a (1)
;;     b <- b(0) + a(1)
;; ..of T_{p q}  tranforming (a,b):
;;     a <-bq + aq + ap
;;     b <-bp + aq
;; .. so let's apply such a tranformation twice:
;;     a_2 <-b_1q + a_1q + a_1p
;;     b_2 <-b_1p + a_1q
;; .. .. ..
;;     a_1 <-b_0q + a_0q + a_0p
;;     b_1 <-b_0p + a_0q
;; .. .. substituting ..
;;     a_2 <- (b_0p + a_0q)q + (b_0q + a_0q + a_0p)q + (b_0q + a_0q + a_0p)p
;;          = a_0(q^2 + q^2 + pq + qp + p^2) + b_0(pq + q^2 + qp) 
;;          = a_0(2q^2 + 2pq +  p^2) + b_0(2pq + q^2p) 
;;          = b_0(2pq + q^2p) + a_0(2pq + q^2) + a_0(q^2 + p^2)
;;     b_2 <- (b_0p + a_0q)p + (b_0q + a_0q + a_0p)q
;;          = b_0(p^2 + q^2) + a_0(qp + q^2 + pq) 
;;          = b_0(p^2 + q^2) + a_0(2qp + q^2) 
;;          = b_0(p^2 + q^2) + a_0(2qp + q^2) 
;; ... so ...
;; p' = (p^2 + q^2)
;; q' = (2pq + q^2p) 
;; ... So now we know how to "square" T

(define (log-fib n)
  (log-fib-iter 1 0 0 1 n))

(define (log-fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count) ; Apply T^2
	 (log-fib-iter a
		   b
		   (+ (* p p)         ; compute p
		      (* q q)) 
		   (+ (* 2 p q)       ; compute q
		      (* q q p)) 
		   (/ count 2)))
	(else (log-fib-iter (+ (* b q) (* a q) (* a p)) ; Apply T once
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;(define (gcd-c a b conditional)
;;  (conditional (= b 0)
;;	       a
;;	       (gcd b (remainder a b))))

;; Ex 1.20
;; (if (= b 0) a (gcd b (remainder a b))))
;; * ------------------------------------*
;; (gcd 206 40)
;; (if (= 40 0) 206 (gcd 40 (remainder 206 40))))
;; (if #f 206 (gcd 40 6)))
;; (if #f 206 (if (= 6 0) 40 (gcd 6 (remainder 40 6))))))
;; (if #f 206 (if #f 40 (gcd 6 4)))))
;; (if #f 206 (if #f 40 (if (= 4 0) 6 (gcd 4 (remainder 6 4))))))))
;; (if #f 206 (if #f 40 (if #f 6 (gcd 4 2)))))))
;; (if #f 206 (if #f 40 (If #f 6 (if (= 2 0) 4 (gcd 2 (remainder 4 2)))))))))
;; (if #f 206 (if #f 40 (if #f 6 (if #f 4 (gcd 2 0))))))))
;; (if #f 206 (if #f 40 (if #f 6 (if #f 4 (if (= 0 0) 4 (gcd 4 (remainder 2 0)))))))))))
;; (if #f 206 (if #f 40 (if #f 6 (if #f 4 (if #t 4 (gcd 4 (remainder 2 0)))))))))))
;; ... so it goes on forever...


"Fermat’s Little Theorem: If n is a prime number and a is any positive 
integer less than n, then a raised to the nth power is congruent to a 
modulo n.
"
;; a^n = n
