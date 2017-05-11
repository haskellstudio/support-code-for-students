#lang racket
(require racket/fixnum)

(require rackunit)

(require "interp.rkt")
(require "compiler.rkt") 
(require "utilities.rkt")

#|
 exp ::= int | (read) | (- exp) | ( + exp exp)
 R0  ::= (program exp)
|#


;(define ast1.4 `(- 8))
;(define ast1.1 `(+ (+ 4 (read)) ast1.4))

(define (print_ex e indent)
  ;(print_space indent)
  (map (lambda (i) (printf "~a" i)) indent)
  (printf "~a" e)
  (newline))



(define (new_indent)
  (list ))

(define (add_indent indent)
  (cons  "  " indent))


(define (print_ast ast indent)
  (match ast
    [`(,op ,l ,r)  (print_ex op  indent) (print_ast l (add_indent indent)) (print_ast r (add_indent indent))]

    ;[`(,op ,l ,r ,t)  (print_ex op  indent) (print_ast l (add_indent indent)) (print_ast r (add_indent indent))  (print_ast t (add_indent indent))]
    
    [`(,op ,l) (print_ex op indent) (print_ast l (add_indent indent))]

    [else (print_ex ast indent)]
    ))



;(print_ast ast1.1 (new_indent))

#| 
(print_ast


`(define (new_indent)
  (list ))


 (new_indent))

|#


(define (is_leaf_node ast)
  (match ast
    [(? fixnum?) #t]
    [`(read) #t]
    [else #f]))


#|
(is_leaf_node `(read))
(is_leaf_node `(- 8))
(is_leaf_node `(+ (read) (- 8)))
|#



#|
 exp ::= int | (read) | (- exp) | ( + exp exp)
 R0  ::= (program exp)
|#


(define (is_ro ast)
  (match ast
    [`(- ,e)  (is_ro e)]
    [(? fixnum?) #t]
    [`(read) #t]
    [`(+ ,l ,r) (and (is_ro l) (is_ro r))]
    [`(program ,exp) (is_ro exp)]
    [else #f]))


;(is_ro `(- 8))
;(is_ro `(+ (read) (- 8)))
;(is_ro `(- (read) (+ 8)))





#|
 exp ::= int | (read) | (- exp) | ( + exp exp)
 R0  ::= (program exp)
|#
;(define ast1.4 `(- 8))
;(define ast1.1 `(+ (+ 4 (read)) ,ast1.4))



(define (i0 ast)
  (match ast
    [(? fixnum?) ast]
    [`(read)  (let [(r (read))]
                (cond
                  [(fixnum? r) r]
                  [else (error   `i0  " ~a is not an integer" r ) ]))]
    [`(- ,e) (- (i0 e))]
    [`(+ ,l ,r) (+ (i0 l) (i0 r))]
    [`(program ,e) (i0 e)]
    [else (printf "ilegal: ~a" ast)]))



(define (i00 ast)
  (match ast
    [(? fixnum?) ast]
    [`(read) (let [(r (read))]
               (cond [(fixnum? r) r]
                     [else (error `I00 "~a is not an integer" r)]))]
    [`(- ,(app i00 e)) (- e)]
    [`(+ ,(app i00 e1) ,(app i00 e2)) (+ e1 e2)]
    [`(program ,(app i00 e)) e]
    ))


;(i00 ast1.1)
;(i00 (+ 42 2))





#|
 exp ::= int | (read) | (- exp) | ( + exp exp)
 R0  ::= (program exp)
|#

#|
exp ::= (read) | (- (read)) | (+ exp exp)
residual ::= int | (+ int exp) | exp
|#
(define (pe ast)
  (match ast
    [`(read) `(read)]
    [`(-  ,(app pe e))  (cond
                        [(fixnum? e) (- e)]
                        [(match e
                          [`(- ,s) s]
                          [else `(- ,e)])]
                        )]

    [`(+ ,(app pe e1) ,(app pe e2))  (cond
                                       [(and (fixnum? e1) (fixnum? e2) ) (+ e1 e2)  ]
                                       [(fixnum? e1)  (match e2
                                                        [`(+ ,l ,r)(cond
                                                                           [(fixnum? l) `(+ ,(+ e1 l) ,r )]
                                                                           [(fixnum? r) `(+ ,(+ e1 r) ,l)])]
                                                        [else `(+ ,e1 ,e2)]
                                                        )]
                                       [(fixnum? e2) (match e1
                                                        [`(+ ,l ,r) (cond
                                                                     [(fixnum? l) `(+ ,r ,(+ l e2))]
                                                                     [(fixnum? r) `(+ ,l ,(+ r e2))])]
                                                       [else `(+ ,e1 ,e2)])]

                                       )]
    
    [(? fixnum?) ast]
    [else (error `pe: "~a ilegal ast" ast)]))




#|
(define (test-pe p)
  (print (pe p))
  (check-equal? (i00 p) (i00 (pe p)))
  )
|#

;(pe `(+  (+ 1 (read)) (- (- 1))))
#|
(pe `(+ 1 (- (- (- (read))))))
(pe `(+ 2 (- (+ 5 3))))
(pe `(+  (+ 1 (read)) 1))
(pe `(+ 1 (+ 1 (read))))
(pe `(- (+ (read) (- 5))))
|#

;(check-equal? 1 2)





 
(define (new-env) (list))

(define (append-env env var value)
  (cond
    [(symbol? var) (cons (cons var value) env)]
    [else (error `append-env: "~a is not symbol" var)]))


(define (look-up env var)
  (if
    (null? env) (error `look-up "can not find ~a" var)
    ( let ([head (car env)] [tail (cdr env)])
       (if (pair? (car env))
           (if (equal? (car head) var)
               (cdr head)
               (look-up tail var))
           (error 'look-up "fial to look up ~a, bcz env is not a pair" var)))))

#|
(define g-var (append-env
            (append-env (new-env) 'a 1)
            `b
            2
            ))
(look-up g-var 'a)
|#


#|

exp ::= int | (read) | (- exp) | (+ exp exp)
| var | (let ([var exp]) exp)
R1 ::= (program exp)

|#
 
(define (i1 env )
  (lambda (ast)
    (define i1-env (i1 env) )
    (match ast
      [(? fixnum?) ast]
      [`(read) (let ([r (read)])
                 (if (fixnum? r)
                     r
                     (error `i1: "~a is not a fixnum" r)))]
      [`(- ,(app i1-env e )) (- e)]
      [`(+ ,(app i1-env e1) ,(app i1-env e2 ))   (+ e1 e2)]
      [`(- ,(app i1-env e1) ,(app i1-env e2 ))   (- e1 e2)]
      [(? symbol?)   (look-up env ast)]
      [`(let ([,x ,(app i1-env e)]) ,body) ( (i1 (append-env env x e)) body)]
      [`(program ,(app i1-env e)) e]
      
      )
    ))
#|
( (i1 (new-env)) `(program
                   (let ([x (+ 12 20)])
                     (+ 10 x))))

( (i1 (new-env)) `(program
                   (let ([x 32])
                     (+ (let ([x 10]) x)
                        x))))

( (i1 (new-env)) `(program (let ([x (read)])
                             (let ([y (read)])
                               (- x y)))))
|#

(define-syntax ++
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1)) x))
    ((_ x n) (begin (set! x (+ x n)) x))))



(define (gen-var var index)
  (string->symbol ( format "~a.~a" var index)))



(define (uniquify env index  ast)
  (match ast
    [`(let ([,var ,val]) ,body)  (let ([new-var (gen-var var index)])
                                   `(let ([,new-var ,val])
                                      ,(uniquify (append-env env var new-var) (++ index) body )))   ]
    [(? fixnum?) ast]
    [(? symbol?) (look-up env ast)]
    [`(+ ,e1 ,e2) `(+ ,(uniquify env index e1) ,(uniquify env index e2)) ]
    [`(program ,e) `(program ,(uniquify env index e))]
    [`(- ,e) `(- ,(uniquify env index e))]
    )
  )


#|
(uniquify
 (new-env)
 0
 `(program
   (let ([x 32])
     (+ (let ([x 10]) (- x))
        x)))
 )
|#


#|
(uniquify
(new-env)
0
`(let ([x 32])
  (+ (+ x 2) (+ x 3)))

 )
|#


#|
newly flattened expression


assignment statements


variables
|#

(define gen-sys-index 0)
(define (gen-sys)
  (let ( [return (format "g.~a" gen-sys-index)])
    (++ gen-sys-index)
    (string->symbol return)
    ))

(define (flatten ast asgns vars )
  (match ast
    [`(program ,e) (flatten e asgns vars)]
    [(? fixnum?) (values ast asgns vars)]
    [`(- ,e)  (let ([tmp-var (gen-sys)])
                (let-values ([ (return a v) (flatten e asgns vars)] )
                  (values tmp-var (append a `(  (assign ,tmp-var (- ,return)) )) (append v `(,tmp-var)))))
                ]
    [`(+ ,e1 ,e2) (let ([tmp-var (gen-sys)])
                    (let-values ([(return1 a1 v1) (flatten e2 asgns vars)])
                        (let-values ([(return2 a2 v2) (flatten e1 a1 v1)])
                          (values tmp-var
                                  (append a2 `( (assign ,tmp-var (+ ,return2 ,return1) )))
                                  (append v2 `(,tmp-var))))))]
    [`(let ([,var ,val]) ,body) (let-values  ([ (return codes vs) (flatten val asgns vars)])
                                  
                                  (let-values ([(rb codesb varsb) (flatten
                                                                   body
                                                                   (append  codes `( (assign ,var ,return) ))
                                                                   vs)])
                                    (values rb
                                            codesb
                                            (append varsb `(,var)))))    ]

    [`(read) (let ([tmp-var (gen-sys)])
               (values tmp-var
                       (append asgns `( (assign ,tmp-var ,ast)))
                       (append vars `(,tmp-var)))
               )]
    [(? symbol?)  (values ast asgns vars)]))



(define (print-vars vars)
  (map (lambda(var)
         (print (format "~a " var)))
       vars)
  (newline)
  (newline))

(define (print-asgns asgns)
  (map (lambda(asgn)
         (print (format "~a " asgn))
         (newline))
       asgns)
  (newline))

(define (flat exp)
  (let-values ([(flattened asgns vars) (flatten exp `() `())])
    ;`(program ,vars ,asgns (return ,flattened))
    (print-vars vars)
    (print-asgns asgns)
    (print (format "return ~a" flattened))
    ;`(main  ,(append vars (append asgns `(return ,flattened))))
  ))


;(flat  `(- (- (- 1))))
;(flat `(+ 52 (- (- (- (- 1))))))
;(flat `( let ([x (+ 52 (- (- (- (- 1)))))]) (+ x 2)))
;(flat `( let ([x (+ 52 (- (- (- (- (read))))))] (+ x 2))))
;(flat `(+ 2 10))

;(flat `(- 10))

;(flat `( let ([x (- 1)] 1)))



(define (select-instructions codes x64s)
  (if (empty? codes) x64s
      (let ([code (car codes)]
            [restCodes (cdr codes)])
        (match code
          [`(assign ,var ,val)
           (match val
             [(? fixnum?) (select-instructions restCodes (append x64s `((movq (int ,val) (var ,var)))))]
             [(? symbol?) (select-instructions restCodes (append x64s `((movq (var ,val) (var ,var)))))]
             [`(read)     (select-instructions restCodes (append x64s `((callq read_int)
                                                                        (movq (reg rax) (var ,var))) ))]
             [`(- ,e)      (select-instructions restCodes (append x64s (let ([varOrVal (if (symbol? e)
                                                                                          `var
                                                                                          `int)])
                                                                        `((movq (,varOrVal ,e ) (var ,var))
                                                                          (negq (var ,var))))))]

             [`(+ ,e1 ,e2 ) (select-instructions restCodes (append x64s (letrec ([type1 (if (symbol? e1) `var `int)]
                                                                              [type2 (if (symbol? e2) `var `int)]
                                                                              [needNotMove (or (eqv? e1 var) (eqv? e2 var))]
                                                                              [theOneNeedMove (if(eqv? e1 var) e2 e1)]
                                                                              [theOneNeedMoveType (if(eqv? e1 var) type2 type1)])
                                                                              (if needNotMove
                                                                                  `((addq (,theOneNeedMoveType ,theOneNeedMove) (var ,var)))
                                                                                  `((movq (,type1 ,e1) (var ,var))
                                                                                    (addq (,type2 ,e2) (var ,var)))
                                                                                  ))))]
             
             )]))))



;(select-instructions `((assign g.0 (- 1)) (assign x g.0) ) `())

;(select-instructions `(  (assign x (+ 10 x)) ) `())  ; 1, (assign x (+ 10 x)) ⇒ (addq (int 10) (var x))   do not need move!!!


(define (flat-select exp)
  (let-values ([(flattened asgns vars) (flatten exp `() `())])
    
    ;`(program ,vars ,asgns (return ,flattened))

    (print-vars vars)
    ;(print-asgns asgns)
    (print-asgns (select-instructions asgns `()) )
    
    (print (format "return ~a" flattened))


  ))


;(flat-select  `(- (- (- 1))))
;(flat-select `(+ 52 (- (- (- (- 1))))))
;(flat-select `( let ([x (+ 52 (- (- (- (- 1)))))] (+ x 2))))
;(flat-select `( let ([x (+ 52 (- (- (- (- (read))))))]) (+ x 2))  )
;(flflat-selectat `(+ 2 10))

;(flat-select `(- 10))


; todo 
;       1, (let ((x (+ 2 2)) 1))     ((x (+ 2 2))) do not need compute
;       2, do not support (let ([x 2][y 3]) (+ x y))



(define (get-var-maps vars init) ;init -8 byte = 64bit   
  (let ([res `()]
        [i 1])
    (map (lambda (var)
           (begin
                  (set! res (cons `(,var ,(* init i)) res))
                  (++ i)))
         vars)
    res))



(define (assign-homes inst offsetMaps)
  (match inst
    [`(callq ,label) `( (,inst))]
    [`(,unaryOp ,arg ) `( (,inst))]
    [`(,binary-op ,arg1 ,arg2) `((,inst))]
    ))




(define (map-to-ebp exp)
  (let-values ([(flattened asgns vars) (flatten exp `() `())])
    (let ([codes (select-instructions asgns `())])
      (let ([var-maps (get-var-maps vars -8)]
          [res `()])
     
      (map (lambda (code)
             (set! res (cons (assign-homes code var-maps) res)))
           codes)
      res))))

    ;(print-vars vars)
    
   
    ;(print-asgns (select-instructions asgns `()) )
    
    ;(print (format "return ~a" flattened))






(map-to-ebp `( let ([x (+ 52 (- (- (- (- (read))))))]) (+ x 2))  )



