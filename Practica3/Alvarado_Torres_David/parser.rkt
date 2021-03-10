#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (car sexp)
       [(+) (op + (map (lambda (i) (parse i)) (cdr sexp)))]
       [(-) (op - (map (lambda (i) (parse i)) (cdr sexp)))]
       [(*) (op * (map (lambda (i) (parse i)) (cdr sexp)))]
       [(/) (op / (map (lambda (i) (parse i)) (cdr sexp)))]
       [(modulo) (op modulo (map (lambda (i) (parse i)) (cdr sexp)))]
       [(expt) (op expt (map (lambda (i) (parse i)) (cdr sexp)))]
       [(add1) (op add1 (list (parse (second sexp))))]
       [(sub1) (op sub1 (list (parse (second sexp))))]
       [(with) (with (map (lambda (i) (binding (first i) (parse (second i)))) (second sexp)) (parse (third sexp)))]
       [(with*) (with* (map (lambda (i) (binding (first i) (parse (second i)))) (second sexp)) (parse (third sexp)))])]))