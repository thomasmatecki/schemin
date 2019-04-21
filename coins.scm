;;
;;
;;
(load "testin.scm")

(define (change-ways amount denomxs)
  (cond ((or (< amount 0) (null? denomxs)) 0)
	((= 0 amount) 1)
	(else (+ (change-ways amount (cdr denomxs))
		 (change-ways (- amount (car denomxs)) denomxs)))))


;; Iterative solution
;; .. to start we'll need something to memoize in -
;; .. (integer, integer) -> integer

(define (null-leaf i j)
  '())


(define (node l v r)
  (lambda () '(l v r))
 )


;; Tests
(is-it-true?
 (null? (null-leaf 1 1))
 (null? (null-leaf 1 0))
 (null? (null-leaf 0 0))
 (null? (null-leaf 2 2))
)

(is-it-true?
 (= '() '() 1 '())
    (node '() 1 '())
)
