#lang racket
(require racket/fixnum)
(require "interp.rkt")
(require rackunit)

;; This exports r0-passes, defined below, to users of this file.
(provide r0-passes)

;; The following pass is just a silly pass that doesn't change anything important,
;; but is nevertheless an example of a pass. It flips the arguments of +. -Jeremy
(define (flipper e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) `(- ,(flipper e1))]
    [`(+ ,e1 ,e2) `(+ ,(flipper e2) ,(flipper e1))]
    [`(program ,e) `(program ,(flipper e))]
    ))


;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (cond [(fixnum? r) (fx- 0 r)]
	[else `(- ,r)]))

(define (pe-add r1 r2)
  (cond [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)]
	[else `(+ ,r1 ,r2)]))

(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,e1) (pe-neg (pe-arith e1))]
    [`(+ ,e1 ,e2) (pe-add (pe-arith e1) (pe-arith e2))]
    [`(program ,e) `(program ,(pe-arith e))]
    ))   

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r0-passes
  `( ("flipper" ,flipper ,interp-scheme)
     ("partial evaluator" ,pe-arith ,interp-scheme)
     ))



(define ast1.4 `(- 8))
(define ast1.1 `(+ (read) ,ast1.4))

;(match ast1.1
 ; [`(,op ,child1 ,child2)
  ;  (print op)  (newline)
;
 ;   
  ;  (print child1) (newline)
;
 ;   (print child2) (newline)
  ;  ])

(define (leaf? arith)
  (match arith
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,c1) #f]
    [`(+ ,c1 ,c2) #f])
    )


;(leaf? `(read))



;(leaf? `(- 8))

;(leaf? `(+ 1 1))



(define (R0? sexp)
(match sexp
[(? fixnum?) #t]
[`(read) #t]
[`(- ,e) (R0? e)]
[`(+ ,e1 ,e2)
(and (R0? e1) (R0? e2))]
[`(program ,e) (R0? e)]
[else #f]))



;(R0? `(+ (read) (- 8)))
;(R0? `(- (read) (+ 8)))


;(interp-R0 ast1.1)
#| and
(define (interp-R0 e)
  (match e
    [ (? fixnum?) e]
    [`(read)
     (let [ (r (read))]
       (cond [ (fixnum? r) r]
             [else (error `interp-R0 "input not an integer")]))]
    [`(- ,(app interp-R0 v1) ,(app interp-R0 v2))
     (fx- v1 v2)]

    [`(+ ,(app interp-R0 v1) ,(app interp-R0 v2))
     (fx+ v1 v2)]

    [`(program ,(app interp-R0 v)) v]
    ))


 |#

(define (pe-arith_ e)
  (match e
    [(? fixnum?) e]
    [`(read) `(read)]
    [`(- ,(app pe-arith_ r1))   (cond [(fixnum? r1) (fx- 0 r1)]
                                      [else  `(- ,r1)])]
    [`(+ ,(app pe-arith_ r1) ,(app pe-arith_ r2)) (cond
                                                    [(fixnum? r1)
                                                     (match r2
                                                       [(? fixnum?) (+ r1 r2)]
                                                       [`(read) `(+ ,r1 (read))]
                                                       [`(- ,e) `(+ ,r1 ,r2)]
                                                       [`(+ ,(app pe-arith_ s1) ,(app pe-arith_ s2))
                                                        (cond [(and (fixnum? s1) (fixnum? s2))     (+ r1 (+ s1 s2))]
                                                              [(fixnum? s2) `(+ ,(+ r1 s2) ,s1)]
                                                              [(fixnum? s1) '(+ ,(+ r1 s1) ,s2) ]
                                                              [else `(+ ,r1 ,r2)])]
                                                       )]
                                                    [(fixnum? r2)
                                                     (match r1
                                                       [(? fixnum?) (+ r1 r2)]
                                                       [`(read) `(+  (read) ,r2)]
                                                       [`(+ ,(app pe-arith_ s1) ,(app pe-arith_ s2))
                                                        (cond
                                                          [(and (fixnum? s1) (fixnum? s2))     (+  (+ s1 s2) r2)]
                                                          [(fixnum? s1) `(+ ,s2 ,(+ r2 s1))  ]
                                                          [(fixnum? s2) `(+ ,s1 ,(+ r2 s2))]
                                                          [else `(+ ,r1 ,r2)])])]
                                                    [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)]
                                                    [else `(+ ,r1 ,r2)]) ]
    ))


;(pe-arith_ '(+ 1 (+ (read) 2)))
;(pe-arith_ '(+ (+ (read) 1) 2))
(pe-arith_ '(+ 1 (- (- (read)))))
;(pe-arith_ '(+  2 (+ (read) (read))))
;(pe-arith_ '(+ (+ (read) (read)) 2))



