;;
;;
;;
(load "collections-n-stuff.scm")

;; generate a list of n random numbers between 1 and 1000
(define (n-randos n)
  (map (lambda (_) (random 1000)) (range-asc 0 1000)))


(define (quick-sort ls cmp)
  (define (iter-quick-sort lts eqs gts)
    (append
     (quick-sort lts cmp)
     eqs
     (quick-sort gts cmp)))

  ;; Partitions `ls into 3 lists:
  ;;    (lts, eqs, gts) <=> less than, equal to, greater than
  ;; `pivot comparing using `cmp.
  (define (cmp-partition pivot)
    (define (iter-partition lsx lts eqs gts)
      (if (null? lsx)
	  (list lts eqs gts)
	  (let ((first (car lsx))
		(next (lambda (ltsx eqsx gtsx)
		    (iter-partition (cdr lsx) ltsx eqsx gtsx))))
	    (cond
	     ((= first pivot) (next lts (cons first eqs) gts))
	     ((cmp first pivot) (next (cons first lts) eqs gts))  
	     (else (next lts eqs (cons first gts)))))))

    iter-partition)
  
  (if (null? ls)
      ls
      (apply iter-quick-sort
	     ((cmp-partition (car ls))
	      ls '() '()'() )))


(define (insert-sort ls cmp)

  '()

  )
