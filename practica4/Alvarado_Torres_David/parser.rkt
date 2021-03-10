#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (id sexp)]
    [(number? sexp) (num sexp)]
    [(list? sexp)
     (case (car sexp)
       [(if0) (if0 (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp)) )]
       [(+) (op + (map (lambda (i) (parse i)) (cdr sexp)))]
       [(-) (op - (map (lambda (i) (parse i)) (cdr sexp)))]
       [(*) (op * (map (lambda (i) (parse i)) (cdr sexp)))]
       [(/) (op / (map (lambda (i) (parse i)) (cdr sexp)))]
       [(modulo) (op modulo (map (lambda (i) (parse i)) (cdr sexp)))]
       [(expt) (op expt (map (lambda (i) (parse i)) (cdr sexp)))]
       [(add1) (op add1 (list (parse (second sexp))))]
       [(sub1) (op sub1 (list (parse (second sexp))))]
       [(with) (app (fun (for/list ([i (second sexp)]) (car i)) (parse (third sexp))) (for/list ([i (second sexp)]) (parse (second i))))]
       [(with*) (with* (map (lambda (i) (binding (first i) (parse (second i)))) (second sexp)) (parse (third sexp)))]
       [(fun) (let* ([args (second sexp)] [dup (check-duplicates args)])
                (if (equal? dup #f)
                    (fun args (parse (third sexp)))
                    (error (string-append "parser: parámetro definido dos veces: " (symbol->string dup)))))]
       [else (let ([args (for/list ([i (second sexp)]) (parse i))])
               (if (or (symbol? (car sexp)) (= (length (second (car sexp))) (length args)))
                  (app (parse (car sexp)) args)
                  (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función")))])]))
