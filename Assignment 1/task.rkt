#lang racket

; Exercise 1.1
; check whether a given expression is a variable
(define (variable? expr)
  (not (procedure? expr))
  )

; Check whether a given expression is a sum
(define (sum? expr)
  (match expr
    [(list '+ _ _) #t]
    [_ #f]
    ))

; extract first summand from a sum
(define (summand-1 expr)
  (match expr
    [(list '+ a _) a]
    ))

; extract second summand from a sum
(define (summand-2 expr)
  (match expr
    [(list '+ _ a) a]
    ))

; Check whether a given expression is a product
(define (product? expr)
    (match expr
    [(list '* _ _) #t]
    [_ #f]
    ))

; extract first multiplier from a product
(define (multiplier-1 expr)
    (match expr
    [(list '* a _) a]
    ))

; extract second multiplier from a product
(define (multiplier-2 expr)
    (match expr
    [(list '* _ a) a]
    ))

; Exercise 1.2
; compute a symbolic derivative of a given expression with respect to a given variable.
(define (derivative expr wrt)
  (cond
    [(sum? expr) (list '+
                       (derivative (summand-1 expr) wrt)
                       (derivative (summand-2 expr) wrt))]
    [(product? expr) (list '+
                           (list '*
                                 (derivative (multiplier-1 expr) wrt)
                                 (multiplier-2 expr))
                           (list '*
                                 (multiplier-1 expr)
                                 (derivative (multiplier-2 expr) wrt))
                                 )]
    [(variable? expr) (cond
                        [(equal? expr wrt) 1]
                        [else 0])]
    [(number? expr) 0]
    ))
                        
; Exercise 1.3

(define (simplify-at-root expr)
  (match expr
    [(list '+ 0 e) e]
    [(list '+ e 0) e]
    [(list '* 1 e) e]
    [(list '* e 1) e]
    [(list '* 0 e) 0]
    [(list '* e 0) 0]
    [(list '+ a b)
     #:when (and (number? a) (number? b)) (+ a b)]
    [(list '* a b)
     #:when (and (number? a) (number? b)) (* a b)]
    [_ expr]
    ))
     
(define (simplify expr)
  (cond
    [(sum? expr) (simplify-at-root(list '+
                                        (simplify (summand-1 expr))
                                        (simplify (summand-2 expr))))]
    [(product? expr) (simplify-at-root(list '*
                                        (simplify (multiplier-1 expr))
                                        (simplify (multiplier-2 expr))))]
    [else expr]
    ))
    

; Exercise 1.4

; Simplifies any expression down to a poly- nomial of its variables.
;(define (normalize expr))

; Exercise 1.5

; Converts an expression into an infix form
(define (to-infix expr)
  (cond
    [(sum? expr) (list
                  (to-infix (summand-1 expr))
                  '+
                  (to-infix (summand-2 expr)))]
    [(product? expr) (list
                  (to-infix (multiplier-1 expr))
                  '*
                  (to-infix (multiplier-2 expr)))]
    [else expr]
    ))

; Exercise 1.6
