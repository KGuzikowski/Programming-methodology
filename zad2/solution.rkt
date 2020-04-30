#lang racket

;; zad 1

(define tolerance 0.00001)

(define (inc number)
  (+ number 1))

(define (good-enough? a b)
  (> tolerance (abs (- a b))))

(define (square a)
  (* a a))

(define (calculate-nth-value num den x-1 x-2 k)
  (+ (* (den k) x-1) (* (num k) x-2)))

;; prodecura główna
(define (Fx num den)
  (define (calculate-f_k A B next-A next-B k)
    (if (good-enough? (/ A B) (/ next-A next-B))
        (/ next-A next-B)
        (calculate-f_k next-A
                       next-B
                       (calculate-nth-value num den next-A A (inc k))
                       (calculate-nth-value num den next-B B (inc k))
                       (inc k))))
    (calculate-f_k 0    ; A_0
                   1.0  ; B_0
                   (calculate-nth-value num den 0 1 1) ; A_1
                   (calculate-nth-value num den 1 0 1) ; B_1
                   1))


; Procedura wyznaczajaca arctan(x):

(define (arctg x)
  (define (num k)
    (if (= k 1)
        x
        (square (* (- k 1) x))))
  (define (den k)
    (- (* 2 k) 1))
  (Fx num den))

; Testy sprawdzajace dzialanie:

(arctg 0)
(arctg 0.08)
(arctg 0.5)
(arctg 1)
(arctg 5)
(arctg 13)

(provide arctg)
