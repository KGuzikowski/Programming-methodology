#lang racket

(provide (struct-out const) (struct-out binop) rpn->arith)

;; -------------------------------
;; Wyrazenia w odwr. not. polskiej
;; -------------------------------

(define (rpn-expr? e)
  (and (list? e)
       (pair? e)
       (andmap (lambda (x) (or (number? x) (member x '(+ - * /))))
               e)))

;; ----------------------
;; Wyrazenia arytmetyczne
;; ----------------------

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)

(struct stack (xs))

(define empty-stack (stack null))
(define (empty-stack? s) (null? (stack-xs s)))
(define (top s) (car (stack-xs s)))
(define (push a s) (stack (cons a (stack-xs s))))
(define (pop s) (stack (cdr (stack-xs s))))

(define (arith-expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r)
     (and (symbol? op) (arith-expr? l) (arith-expr? r))]
    [_ false]))

;; ----------
;; Kompilacja
;; ----------

(define (rpn->arith-am e s)
  (cond [(null? e)
         (top s)]
        [(number? (car e))
         (rpn->arith-am (cdr e) (push (const (car e)) s))]
        [(and (symbol? (car e)) (member (car e) '(+ - * /)))
         (let*
             ([op (car e)]
              [rest (cdr e)]
              [second (top s)]
              [first (top (pop s))])
           (rpn->arith-am rest (push (binop op first second) (pop (pop s)))))]))

(define (rpn->arith e)
  (rpn->arith-am e empty-stack))
