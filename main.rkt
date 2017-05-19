#lang racket
(require racket/fixnum)

(require rackunit)

(require "interp.rkt")
(require "compiler.rkt") 
(require "utilities.rkt")
(require graph)


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
       (if (pair? head)
           (begin
             ;(print (car head))
             ;(newline)
             ;(print var)
             ;(newline)
             (if (equal? (car head) var)
                 (if (equal? (cadr head) `reg)
                     (cdr head)
                     (cadr head))
                 (look-up tail var))
             )

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
;(flat `( let ([x (+ 52 (- (- (- (- (read))))))]) (+ x 2)))
;(flat `(+ 2 10))

;(flat `(- 10))

;(flat `( let ([x (- 1)]) 1))



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
             
             )]
          [`(return ,e) (select-instructions restCodes (append x64s (let ([varOrVal (if (symbol? e)
                                                                                          `var
                                                                                          `int)])
                                                                      `((movq  (,varOrVal ,e) (reg rax))
                                                                        (retn)))))]
          ))))

;(flat `( let ([x (- 1)]) 1))
;(select-instructions (flat `( let ([x (- 1)]) 1)) `())
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
    [`(callq ,label) (list inst)]
    [`(,unaryOp ,arg ) (if (eqv? (car arg) `var)
                           (let ([val (look-up offsetMaps (cadr arg))])
                             (if (equal? (car val) `reg)
                                 (list unaryOp val)
                                 (list `(,unaryOp (l_ rbp,(look-up offsetMaps (cadr arg)))))))
                           ;(list `(,unaryOp (l_ rbp,(look-up offsetMaps (cadr arg)))))
                           (list inst))] ;`(,inst)
    [`(,binary-op ,arg1 ,arg2) (list `(,binary-op ,(if (eqv? (car arg1) `var)
                                                       (let ([val (look-up offsetMaps (cadr arg1))])
                                                         (if (equal? (car val) `reg)
                                                               val
                                                             `(#|local var|#l_ rbp ,(look-up offsetMaps (cadr arg1)))))
                                                       
                                                       arg1)
                                           ,(if (eqv? (car arg2) `var)
                                                ;`(#|local var|#l_ rbp ,(look-up offsetMaps (cadr arg2)))
                                                (let ([val (look-up offsetMaps (cadr arg2))])
                                                  (if (equal? (car val) `reg)
                                                      val
                                                      `(#|local var|#l_ rbp ,(look-up offsetMaps (cadr arg2)))))
                                                arg2)))]
    [`(retn) inst]
    ))




(define (map-to-ebp exp)
  (let-values ([(flattened asgns vars) (flatten exp `() `())])
    ;(set! asgns (append asgns `(return flattened)))
    (let ([codes (select-instructions asgns `())])
      (let ([var-maps (get-var-maps vars -8)]
          [res `()])
        (map (lambda (code) (set! res (append res  `(,(assign-homes code var-maps)) ))) codes)
        (print-asgns res)
        ))))


    ;(print-vars vars)
    
   
    ;(print-asgns (select-instructions asgns `()) )
    
    ;(print (format "return ~a" flattened))




;(map-to-ebp `(+ 1 (read)))

;(map-to-ebp `( let ([x (+ 52 (- (- (- (- (read))))))]) (+ x 2))  )



(define (patch-instructions ex)
  #|
  (print ex)
  (newline)
  |#
  (match ex
    [`(,op ,arg1 ,arg2) (if (and (equal? (car arg1) `l_) (equal? (car arg2) `l_))
                            (list `(movq ,arg1 (reg rax))
                                  `(,op (reg rax) ,arg2))
                            (list ex)) ]
    [else  (list ex)])
  )
;(movq (l_ rbp -8) (l_ rbp -16))"


(define (patch exp)
  (let-values ([(flattened asgns vars) (flatten exp `() `())])
    ;(set! asgns (append asgns `(return flattened)))
    (let ([codes (select-instructions asgns `())])
      (let ([var-maps (get-var-maps vars -8)]
          [res `()])
        
        (map (lambda (code) (set! res (append res  (assign-homes code var-maps) ))) codes)
        
        (let ([patched `()])
          (map (lambda (code) (set! patched (append patched (patch-instructions code))  ) )res)
          patched)
       
        ;(print-asgns res)
        ))))


;(patch `( let ([x (+ 52 (- (- (- (- (read))))))]) (+ x 2)))





(define raw-uncover-test '(program (let ([v 1]) (let ([w 46]) (let ([x (+ v 7)]) (let ([y (+ 4 w)]) (let ([z (+ x w)]) (+ z (- y)))))))))
;(define test (select-instructions ((flatten-R1) raw-uncover-test)))

(define manual-test '(program (v w x y z t.1 t.2)
                              (movq (int 1) (var v))
                              (movq (int 46) (var w))
                              (movq (var v) (var x))
                              (addq (int 7) (var x))
                              (movq (var x) (var y))
                              (addq (int 4) (var y))
                              (movq (var x) (var z))
                              (addq (var w) (var z))
                              (movq (var y) (var t.1))
                              (negq (var t.1))
                              (movq (var z) (var t.2))
                              (addq (var t.1) (var t.2))
                              (movq (var t.2) (reg rax))))




(define caller-save-registers '(rax rdx rcx rsi rdi r8 r9 r10 r11))
(define callee-save-registers '(rsp rbp r12 r13 r14 r15))


    (define (free-vars a)
      (match a
	 [`(var ,x) (list x)]
	 [`(reg ,r) (list r)]
	 [`(deref ,r ,i) (list r)]
	 [`(int ,n) (list)]
	 [else (error "free-vars, unhandled" a)]))

    (define (get-read-vars instr)
      (match instr
         [`(movq ,s ,d) (free-vars s)]
	 [(or `(addq ,s ,d) `(subq ,s ,d) `(imulq ,s ,d)) 
	  (append (free-vars s) (free-vars d))]
	 [`(negq ,d) (free-vars d)]
	 [`(callq ,f) (list)]
         [`(return ,d) (free-vars d) ]
        [`(retn) (list)]
	 [else (error "read-vars unmatched" instr)]
	 ))
  
    (define (get-write-vars instr)
      (match instr
         [`(movq ,s ,d) (free-vars d)]
	 [(or `(addq ,s ,d) `(subq ,s ,d) `(imulq ,s ,d)) 
	  (free-vars d)]
	 [`(negq ,d) (free-vars d)]
	 [`(callq ,f) caller-save-registers]
         ;[`(return ,d) (list) ]
        [`(retn) (list)]
	 [else (error "write-vars unmatched" instr)]
	 ))



(define (get-live-regs instr live-reg-list)
  (let ([w (get-write-vars instr)]
        [r (get-read-vars  instr)]
        [the-nxt-line-lives-regs (car live-reg-list)])
    #|
    (print instr)
    (newline)

    (print w)
    (newline)

    (print the-nxt-line-lives-regs)
    (newline)
    
    (print r)
    (newline)
    
    (print  (append (remv* w the-nxt-line-lives-regs) r) )
    (newline)

    (print "---")
    (newline)
|#
    (cons (remove-duplicates
           (append (remv* w the-nxt-line-lives-regs) r))
          live-reg-list))) 

(define (rmv-at lst index)
  (define (rmv-at-t lst accu)
    (if (>= accu index )
        `()
        (cons (car lst) (rmv-at-t (cdr lst) (+ accu 1)))))
  (rmv-at-t lst 1)
  )

(define (rmv-tail lst)
  (let ([len (length lst)])
    (rmv-at lst len)))



(define (uncover-live exp)
  (match-define `(program ,vars ,codes ...) exp)
  ;(print codes)
  (list* `program `(,vars ,(cdr (foldr get-live-regs `(()) codes))) codes)
  )




;((v w x y z t.1 t.2) ((v) (w v) (w x) (w x) (w x y) (y w x) (y w z) (z y) (z t.1) (t.1 z) (t.1 t.2) (t.2) ()))

#|(define (build-interference prog)
  (match-define `(program (,vars ,live-afters) ,code ...) prog)
  (define (make-adjacencies excludes live-after)
    (foldr
     (lambda (v prev)
       (cond [(list? (memq v excludes)) prev]
             [else (cons (list (car excludes) v) prev)]))
     null
     live-after))
  (define (callq-helper label)
    null)
  (define (helper live-after instr prev)
    (match instr
      [`(movq (var ,s) (var ,d))
       (append prev (make-adjacencies (list d s) live-after))]
      [`(,_ ,_ (var ,d))
       (append prev (make-adjacencies (list d) live-after))]
      [`(callq ,label)
       prev]
      [_ prev]))
  (list* 'program
         (list vars
               (undirected-graph (filter-not null?(foldl helper '() live-afters  code))))
         code))
|#


;(uncover_live manual-test)


(define (build-interference prog)
  (match-define `(program (,vars ,live-afters) ,code ...) prog)
   ;(print live-afters)
  (define (make-adjacencies excludes live-after)
  
    ;(newline)(newline)(print excludes ) (print "---")  (print live-after)(newline)(newline)
  
    (foldr
     (lambda (v prev)
       (cond [(list? (memq v excludes))
              (cons (list (car excludes) `unknow) prev)]
             [else (cons (list (car excludes) v) prev)]))
     null
     live-after))
  (define (callq-helper label)
    null)
  (define (helper live-after instr prev)
    (match instr
      [`(movq (var ,s) (var ,d))
       (append prev (make-adjacencies (list d s) live-after))]
      [`(movq (var ,s) (reg ,d))
       (append prev (make-adjacencies (list d s) live-after))]
      [`(,_ ,_ (var ,d))
       (begin
         ;(print "!")(print d)
         (append prev (make-adjacencies (list d) live-after))
         )
       ]
      [`(callq ,label)
       ;prev
       (append 
         (make-adjacencies (list `(reg rax)) live-after) 
        prev)


       ]
      [_ prev]))
  (list* 'program
         (list vars
               (undirected-graph (filter-not null? (foldl helper '() live-afters code))))
         code))


(define (print-graph prog)
  (match-define `(program (,vars ,graph) ,code ...) prog)
  (get-edges graph)
  )

(define td

  `(program
   (g.4 g.3 g.2 g.1 g.0)

   (movq (int 1) (var g.4))
  (negq (var g.4))
  (movq (var g.4) (var g.3))
  (negq (var g.3))
  (movq (var g.3) (var g.2))
  (negq (var g.2))
  (movq (var g.2) (var g.1))
  (negq (var g.1))
  (movq (int 52) (var g.0))
  (addq (var g.1) (var g.0))
  (movq (var g.0) (reg rax))
  (retn))
  )
;(print-graph (build-interference (uncover-live td)))

;(print-graph (build-interference (uncover-live manual-test)))


;get-read-vars get-write-vars
#|
`((v) (w v) (w x) (w x) (w x y) (y w x) (y z w) (z y) (z t.1) (t.1 z) (t.2 t.1) (t.2) ())  ;live after
 `((movq (int 1) (var v))
  (movq (int 46) (var w))
  (movq (var v) (var x))
  (addq (int 7) (var x))
  (movq (var x) (var y))
  (addq (int 4) (var y))
  (movq (var x) (var z))
  (addq (var w) (var z))
  (movq (var y) (var t.1))
  (negq (var t.1))
  (movq (var z) (var t.2))
  (addq (var t.1) (var t.2))
  (movq (var t.2) (reg rax))) |#; code
#|
(define (t-fold lst1 lst2 lst3 init)
  (+ lst1 lst2 lst3 init))

(foldl t-fold 1 `(1 1) `(2 2) `(3 3))
|#





(define (color-graph g)
  (define-values (i h) (coloring/greedy g))
  h)



;(define all-registers (append caller-save-registers callee-save-registers))

(define (allocate-registers registers g)
  ;(print(get-edges g) )
  (define-values (i h) (coloring/greedy g))
  ;(print h)
  (let ([num-registers (length registers)])
    (hash-map
     h
     (λ (key val)
       (cond [(> num-registers val)
              (begin
               ; (print key)
                (cons key `(reg ,(list-ref registers val)))
                )]
             [else (let ([spill (* -8 (add1 (- val num-registers)))])
                     (begin
                ;       (print key)
                       (cons key `(deref rbp ,spill))))])))))
;(build-interference (uncover-live manual-test))

;(allocate-registers caller-save-registers (cadadr (build-interference (uncover-live manual-test))))


  ;(print codes)
  ;(foldr get-live-regs `(()) codes))


(define (flat-select- exp)
  (let-values ([(flattened asgns vars) (flatten exp `() `())])
    ;(list* `program vars (append asgns (list (list `return flattened))) )
    (let ([si (select-instructions (append asgns (list (list `return flattened))) `()) ])
      (let ([prog (list* `program vars si)])
        (let ([ul (uncover-live prog)])
          (let ([bi (build-interference ul)])
            ;(print (get-edges (cadadr bi)))
            ;(print bi)
            (let([regs (allocate-registers caller-save-registers (cadadr bi)  ) ])
              (print bi)
               (map (λ(code) (assign-homes code regs))  (cdr (cdr bi) ))
              
              ;(newline)
             ; regs
              ;bi
              ;regs
              ;(assign-homes code regs)
              )
            )
        
        )))))
        ;  (let ([ul (uncover-live- )])
          ;  si))))
            
            
           ; (build-interference ul))
      
    ;(list* `program `(,vars ,(cdr (foldr get-live-regs `(()) codes))) codes)
    ;(select-instructions asgns `())
    ;(print-vars vars)
    ;(print-asgns asgns)
    ;(print-asgns (select-instructions asgns `()) )
    ;(print (format "return ~a" flattened))
;(build-interference  (uncover-live '(program (g.0) (movq (int 52) (var g.0)) (addq (int 1) (var g.0)) (movq (var g.0) (reg rax)) (retn)) ))
;(get-edges (cadadr (build-interference  (uncover-live '(program (g.0) (movq (int 52) (var g.0)) (addq (int 1) (var g.0)) (movq (var g.0) (reg rax)) (retn)) ))))
;(flat-select- `(+ 52 1))
;(flat-select- `(+ 52 (- (- (- (- 1))))))
;(uncover-live (flat-select- `(+ 52 (- (- (- (- 1)))))))


;(flat-select- (- (- (- 1))))

;(flat-select-  `(- (- (- 1))))

;(flat-select- `( let ([x (+ 52 (- (- (- (- 1)))))] (+ x 2))))
(flat-select- `( let ([x (+ 52 (- (- (- (- (read))))))]) (+ x 2))  )
;(flat-select- `(+ 2 10))

;(flat-select- `(- 10))
;(get-edges(cadadr 
;(build-interference (uncover-live '(program (g.0) (movq (int 52) (var g.0)) (addq (int 1) (var g.0)) (movq (var g.0) (reg rax)) (retn))))
;))
