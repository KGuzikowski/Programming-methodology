#lang racket

(require racket/contract)

(define with-labels/c
  (parametric->/c [a b] (-> (-> a b) (listof a) (listof (list/c b a)))))

(define (with-labels f lst)
  (if (null? lst)
      null
      (cons
       (list (f (car lst)) (car lst))
       (with-labels f (cdr lst)))))

;;(with-labels number->string (list 1 2 3))

(define foldr-map/c
  (parametric->/c [x a b]
                  (->
                   (-> x a (cons/c b a))
                   a
                   (listof x)
                   (cons/c (listof b) a))))

(define (foldr-map f a xs)
  (define (it a xs ys)
    (if (null? xs)
        (cons ys a)
        (let* [(p (it a (cdr xs) ys))
              (fc (f (car xs) (cdr p)))]
          (cons (cons (car fc) (car p)) (cdr fc)))))

  (it a xs null))

;;(foldr-map (lambda (x a) (cons a (+ a x))) 0 '(1 2 3))

(define pair-from/c
  (parametric->/c [a b c]
                  (->
                   (-> a b)
                   (-> a c)
                   (-> a (cons/c b c)))))

(define (pair-from f g)
  (lambda (x)
    (let [(fx (f x))
          (gx (g x))]
      (cons fx gx))))

;;((pair-from (lambda (x) (+ x 1)) (lambda (x) (* x 2))) 2)

(provide (contract-out
          [with-labels with-labels/c]
          [foldr-map foldr-map/c]
          [pair-from pair-from/c]))
(provide with-labels/c foldr-map/c pair-from/c)