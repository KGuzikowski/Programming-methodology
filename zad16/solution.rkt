#lang racket

(provide lcons lnull lnull? lcar lcdr)

(define (lcons x f) (cons x (mcons f null)))
(define lnull null)
(define lnull? null?)
(define (lcar xs) (car xs))
(define (lcdr xs)
  (if (null? (mcdr (cdr xs)))
      (let*
          [(v ((mcar (cdr xs))))
           (a (set-mcdr! (cdr xs) v))]
        v)
      (mcdr (cdr xs))))