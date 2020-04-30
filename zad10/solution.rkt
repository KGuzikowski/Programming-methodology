#lang racket

(provide (struct-out complex) parse eval)

(struct complex (re im) #:transparent)

(define value?
  complex?)

;; Ponizej znajduje sie interpreter zwyklych wyrazen arytmetycznych.
;; Zadanie to zmodyfikowac go tak, by dzialal z liczbami zespolonymi.

;; do obliczania wartości wyrażeń użyję wzory na dodawanie, odejmowanie, mnożenie i dzielenie liczb zespolonych w postacji algebraicznej

(struct binop (op l r) #:transparent)
(struct const (val)    #:transparent)

(define (pow x) (* x x))

(define (add-complex re1 im1 re2 im2)
  (complex (+ re1 re2) (+ im1 im2)))

(define (sub-complex re1 im1 re2 im2)
  (complex (- re1 re2) (- im1 im2)))

(define (mult-complex re1 im1 re2 im2)
  (complex (- (* re1 re2) (* im1 im2)) (+ (* re1 im2) (* im1 re2))))

(define (divide-complex re1 im1 re2 im2)
  (if (and (= re2 0) (= im2 0))
      (error "nie można dzielić przez zero")
      (complex (/ (+ (* re1 re2) (* im1 im2)) (+ (pow re2) (pow im2))) (/ (- (* im1 re2) (* re1 im2)) (+ (pow re2) (pow im2))))))

(define (calculate-complex op c1 c2)
  (let ([re1 (complex-re c1)]
         [im1 (complex-im c1)]
         [re2 (complex-re c2)]
         [im2 (complex-im c2)])
     (cond
       [(eq? '+ op) (add-complex re1 im1 re2 im2)]
       [(eq? '- op) (sub-complex re1 im1 re2 im2)]
       [(eq? '* op) (mult-complex re1 im1 re2 im2)]
       [(eq? '/ op) (divide-complex re1 im1 re2 im2)])))

(define (expr? e)
  (match e
    [(complex re im) (and (number? re) (number? im))]
    [(binop op l r)  (and (symbol? op) (expr? l) (expr? r))]
    [_ false]))

(define (eval e)
  (match e
    [(complex re im) e]
    [(binop op l r) (calculate-complex op (eval l) (eval r))]))

(define (parse q)
  (cond [(number? q) (complex q 0)]
        [(eq? q 'i) (complex 0 1)]
        [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
         (binop (first q) (parse (second q)) (parse (third q)))]))

(define a (parse '(* (- (+ (+ 3 (* i 2)) (- (+ 2 (* i 4)) (+ 1 (* i 2)))) 2) i)))
a
(expr? a)
(define b (eval a))
(value? b)
b