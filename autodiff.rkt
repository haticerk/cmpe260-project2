;hatice erk
;2018400090
;compiling: yes 
;complete: yes 

#lang racket
(provide (all-defined-out))

(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))

(define get-value(lambda (num) (if (list? num) (map (lambda (x) (list-ref x 1)) num) (num-value num))))

(define get-grad(lambda (num) (if (list? num) (map (lambda (x) (list-ref x 2)) num) (num-grad num))))

(define add(lambda args (let ([x (apply + (map (lambda (k) (num-value k )) args))] [y (apply + (map (lambda (l) (num-grad l)) args))]) (num x y))))

(define mul(lambda args (let ([x (apply * (map (lambda (k) (num-value k)) args))] [y (apply + (map (lambda (l) (* (num-grad l) (apply * (map (lambda (h) (num-value h)) (remove l args))))) args))]) (num x y))))

(define sub(lambda (num1 num2) (let ([x (- (num-value num1) (num-value num2))] [y (- (num-grad num1)(num-grad num2))]) (num x y))))

(define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))

(define mse (lambda (x y) (mul (sub x y) (sub x y))))

(define create-hash(lambda (args1 args2 x) (make-hash (map (lambda (a b) (if (eq? a x) (cons a (num b 1.0)) (cons a (num b 0.0)))) args1 args2))))

(define parse(lambda (hash1 expr) (map (lambda (k) (cond [(eqv? k '+) 'add]
                                                         [(equal? k '*) 'mul]
                                                         [(equal? k '-) 'sub]
                                                         [(equal? k 'mse) 'mse]
                                                         [(equal? k 'relu) 'relu]
                                                         [(number? k) (num k 0.0)]
                                                         [(hash-has-key? hash1 k) (hash-ref hash1 k)]
                                                         [append (parse hash1 k)]
                                                          ))  expr)))

(define grad(lambda (names values var expr) (num-grad (eval (parse (create-hash names values var) expr)))))

(define partial-grad(lambda (names values vars expr) (map (lambda (k) (if (memq k vars) (grad names values k expr) 0.0 )) names)))  

(define gradient-descent(lambda (names values vars lr expr) (map - values (map (lambda (n) (* lr n)) (partial-grad names values vars expr)))))

(define optimize(lambda (names values vars lr k expr) (if (> k 0) (optimize names (gradient-descent names values vars lr expr) vars lr (- k 1) expr) values  )))