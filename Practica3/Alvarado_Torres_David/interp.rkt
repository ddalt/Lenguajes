#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE

(define (subst expr sub-id value)
  (type-case WAE expr
    [id (i) (if (symbol=? i sub-id) value expr)]
    [num (n) expr]
    [op (f args) (op f (for/list ([i args]) (subst i sub-id value)))]
    [with (bindings body)
          (with (for/list ([i bindings]) (binding (binding-id i) (subst (binding-value i) sub-id value)))
                (if (esLigada sub-id bindings)
                    body
                    (subst body sub-id value)))]
    [with* (bindings body)
          (with* (for/list ([i bindings]) (binding (binding-id i) (subst (binding-value i) sub-id value)))
                (if (esLigada sub-id bindings)
                    body
                    (subst body sub-id value)))]))

;; Función auxiliar para la función subst
;; Dado un id y una lista de bindings nos dice si el id de algun binding es igual al id que la función recibe como parámetro
;; esLigada: id (listof? binding) -> boolean

(define (esLigada sub-id bindings)
  (for/or ([i bindings])
    (if (equal? (binding-id i) sub-id)
        #t
        #f)))



;; Toma un árbol de sintáxis abstraca del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number

(define (interp expr)
  (type-case WAE expr
    [id (i) (error "Error: variable libre")]
    [num (n) n]
    [op (f args)
        (let ([values (for/list ([i args]) (interp i))])
          (apply f values))]
    [with (bindings body)
          (if (empty? bindings)
              (interp body)
              (interp (with (cdr bindings) (subst body (binding-id (car bindings)) (binding-value (car bindings))))))]
    [with* (bindings body)
          (if (empty? bindings)
              (interp body)
              (interp (with* (for/list ([i (cdr bindings)]) (substBinding (car bindings) i))
                               (subst body (binding-id (car bindings)) (binding-value (car bindings))))))]))

;; Función auxiliar para la función interp
;; Dados dos bindings bind1 y bind2 sustituye al id de bind1 en el value de bind2 por el value de bind1
;; Es decirm si bind1 = (binding id1 expr1) y bind2 = (binding id2 expr2), sustituye id1 por expr1 en expr2
;; substBinding: binding binding -> binding

(define (substBinding bind1 bind2)
  (binding (binding-id bind2) (subst (binding-value bind2) (binding-id bind1) (binding-value bind1))))