#lang racket
(require racket/fixnum)

(require rackunit)

#|
 exp ::= int | (read) | (- exp) | ( + exp exp)
 R0  ::= (program exp)
|#


(define ast1.4 `(- 8))
(define ast1.1 `(+ (+ 4 (read)), ast1.4))


(define (print_space indent)
  (map (lambda (i) (printf "~a" i)) indent))


(define (print_ex e indent)
  (print_space indent)
  (printf "~a" e)
  (newline))



(define (new_indent)
  (list ))

(define (add_indent indent)
  (cons  "  " indent))


(define (print_ast ast indent)
  (match ast
    [`(,op ,l ,r) (print_ex op  indent) (print_ast l (add_indent indent)) (print_ast r (add_indent indent))]
    [`(,op ,l) (print_ex op indent) (print_ast l (add_indent indent))]
    [number (print_ex ast indent) ]
    [`(read) (print_ex ast indent)]
    ))



(print_ast ast1.1 (new_indent))
