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
    [(exponentiation? expr)
     (let ([base (exp-base expr)]
           [index (exp-index expr)])
     (cond
       [(number? base) (list '*
                             (derivative index wrt)
                             (list '* expr (list 'log base)))]
       [(variable? base) (list '*
                               (derivative base wrt)
                               (list '* index (list '^ base (- index 1))))]
       [else (list '*
                   (derivative index wrt)
                   expr)]
       ))]
 
    [(polyvariadic-sum? expr) (cons '+
                                    (map (lambda (x)
                                           (derivative x wrt))
                                    (rest expr))
                                 )]
    
    [(polyvariadic-product? expr) (cons '+
                                    (map (lambda (x)
                                           (cons '*
                                                 (cons 
                                                 (derivative x wrt)
                                                 (remove x (rest expr)))
                                                 ))
                                    (rest expr))
                                 )]
    
    [(sin? expr) (list '*
                       (derivative (sin-arg expr) wrt)
                       (list 'cos (sin-arg expr))
                       )]

    [(cos? expr) (list '*
                       (derivative (cos-arg expr) wrt)                       
                       (list '* (- 0 1) (list 'sin (cos-arg expr)))
                       )]
        
    
    [(tan? expr) (list '*
                       (derivative (tan-arg expr) wrt)
                       (list '^ (list 'cos (tan-arg expr)) (- 0 2))
                       )]

    [(log? expr) (list '*
                       (derivative (log-arg expr) wrt)
                       (list '^ (log-arg expr) (- 0 1))
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
    [(list '^ e 1) e]
    [(list '^ 1 e) 1]
    [(list 'sin 0) 0]
    [(list 'cos 0) 1]
    [(list 'tan 0) 0]
    [(list 'sin 90) 1]
    [(list 'cos 90) 0]
    [(list 'log 'e) 1]
    [_
     (cond
       [(polyvariadic-sum? expr) (let
                                     ([sum (foldr + 0 (filter number? (rest expr)))])
                                   (cons '+
                                         (cond
                                           [(equal? sum 0) (filter-not number? (rest expr))]
                                           [else (cons
                                                  sum
                                                  (filter-not number? (rest expr))
                                                  )])))]
       [(polyvariadic-product? expr) (let
                                     ([mul (foldr * 1 (filter number? (rest expr)))])
                                   (cond
                                     [(equal? mul 0) null]
                                     [else (cons '*
                                                 (cons
                                                  mul
                                                  (filter-not number? (rest expr)))
                                                 )]
                                     ))]
                                            
                                   
       [else expr]
      )]
    ))
     
(define (simplify expr)
  (cond
    [(sum? expr) (simplify-at-root(list '+
                                        (simplify (summand-1 expr))
                                        (simplify (summand-2 expr))))]
    [(product? expr) (simplify-at-root(list '*
                                        (simplify (multiplier-1 expr))
                                        (simplify (multiplier-2 expr))))]
    [(polyvariadic-sum? expr)  (simplify-at-root(cons '+
                                                      (map (lambda (x)
                                                             (simplify x))
                                                           (rest expr))
                                 ))]
    [(polyvariadic-product? expr)  (simplify-at-root(cons '*
                                                          (map (lambda (x)
                                                                 (simplify x))
                                                               (rest expr))
                                     ))]
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


; Check whether a given expression is an exponentiation 
(define (exponentiation? expr)
  (match expr
    [(list '^ _ _) #t]
    [_ #f]
    ))

; extract base from an exponentiation
(define (exp-base expr)
  (match expr
    [(list '^ a _) a]))


; extract index from an exponentiation
(define (exp-index expr)
  (match expr
    [(list '^ _ a) a]))

; Check whether a given expression is a sin 
(define (sin? expr)
  (match expr
    [(list 'sin _ ) #t]
    [_ #f]
    ))

; extract argument from a sin
(define (sin-arg expr)
  (match expr
    [(list 'sin a) a]))

; Check whether a given expression is a cos 
(define (cos? expr)
  (match expr
    [(list 'cos _ ) #t]
    [_ #f]
    ))

; extract argument from a cos
(define (cos-arg expr)
  (match expr
    [(list 'cos a) a]))

; Check whether a given expression is a tan 
(define (tan? expr)
  (match expr
    [(list 'tan _ ) #t]
    [_ #f]
    ))

; extract argument from a cos
(define (tan-arg expr)
  (match expr
    [(list 'tan a) a]))

; Check whether a given expression is a tan 
(define (log? expr)
  (match expr
    [(list 'log _ ) #t]
    [_ #f]
    ))

; extract argument from a cos
(define (log-arg expr)
  (match expr
    [(list 'log a) a]))

; Exercise 1.7

(define (polyvariadic-sum? expr)
  (cond 
    [(and (list? expr) (> (length expr) 3 ) (equal? (first expr) '+)) #t]
    [else #f]
    ))

(define (polyvariadic-product? expr)
    (cond 
    [(and (list? expr) (> (length expr) 3 ) (equal? (first expr) '*)) #t]
    [else #f]
    ))