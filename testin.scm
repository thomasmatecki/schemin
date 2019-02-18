#lang sicp
(define (is-it-true? desc b)
  (if b
      (string-append "Yep - it's true: " desc)
      "WRONG WRONG WRONG"))

(#%provide is-it-true?)