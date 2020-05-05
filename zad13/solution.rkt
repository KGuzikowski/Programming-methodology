#lang racket

(provide (struct-out const) (struct-out binop) (struct-out var-expr) (struct-out var-dead) (struct-out let-expr) find-dead-vars)

; --------- ;
; Wyrazenia ;
; --------- ;

(struct const    (val)      #:transparent)
(struct binop    (op l r)   #:transparent)
(struct var-expr (id)       #:transparent)
(struct var-dead (id)       #:transparent)
(struct let-expr (id e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x) (symbol? x)]
    [(var-dead x) (symbol? x)]
    [(let-expr x e1 e2) (and (symbol? x) (expr? e1) (expr? e2))]
    [_ false]))

(define (parse q)
  (cond
    [(number? q) (const q)]
    [(symbol? q) (var-expr q)]
    [(and (list? q) (eq? (length q) 3) (eq? (first q) 'let))
     (let-expr (first (second q))
               (parse (second (second q)))
               (parse (third q)))]
    [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
     (binop (first q)
            (parse (second q))
            (parse (third q)))]))

; ---------------------------------- ;
; Wyszukaj ostatnie uzycie zmiennych ;
; ---------------------------------- ;

(define (is-dead? var e)
  (match e
    [(const n) true]
    [(var-expr x) (if (eq? x (var-expr-id var)) false true)]
    [(let-expr x e1 e2) (if (eq? x (var-expr-id var)) (is-dead? var e1) (and (is-dead? var e1) (is-dead? var e2)))]
    [(binop op l r) (and (is-dead? var l) (is-dead? var r))]
    [_ true]))

(define (find-dead-vars e)
  (match e
    [(const n)          (const n)]
    [(binop op l r)     (cond
                          [(and (var-expr? l) (var-expr? r)) (if (eq? (var-expr-id l) (var-expr-id r))
                                                                 (binop op l (var-dead (var-expr-id r)))
                                                                 (binop op (var-dead (var-expr-id l)) (var-dead (var-expr-id r))))]
                          [(var-expr? l) (if (is-dead? l r)
                                             (binop op (var-dead (var-expr-id l)) (find-dead-vars r))
                                             (binop op l (find-dead-vars r)))]
                          [(var-expr? r) (if (is-dead? r l)
                                             (binop op (find-dead-vars l) (var-dead (var-expr-id r)))
                                             (binop op (find-dead-vars l) r))]
                          [else (binop op (find-dead-vars l) (find-dead-vars r))])]
    [(let-expr x e1 e2) (cond
                          [(var-expr? e1) (if (or (eq? (var-expr-id e1) x) (is-dead? e1 e2))
                                              (let-expr x (var-dead (var-expr-id e1)) (find-dead-vars e2))
                                              (let-expr x e1 (find-dead-vars e2)))]
                          [(var-expr? e2) (if (eq? (var-expr-id e1) (var-expr-id e2))
                                              (let-expr x (find-dead-vars e1) (var-dead (var-expr-id e2)))
                                              (let-expr x (find-dead-vars e1) e2))]
                          [else (let-expr x (find-dead-vars e1) (find-dead-vars e2))])]))

;;(define e (parse '(let [x 3] (+ x (let [y x] (+ y y))))))
(define e (parse '(let [x y] x)))
(find-dead-vars e)