#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr)
  (type-case SCFWBAE sexpr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    
    [iFS (c t e) (iF (desugar c) (desugar t) (desugar e))]
    
    [opS (f args) (op f (for/list ([i args]) (desugar i)))]
    
    [condS (cases) (let ([exp (car cases)])
                     (type-case Condition exp
                       [else-cond (e) (desugar e)]
                       [condition (test then)
                                  (iF (desugar test) (desugar then) (desugar (condS (cdr cases))))]))]
  
    [withS (bindings body)
         (app (fun (for/list ([i bindings]) (param (binding-id i) (binding-tipo i))) (desugar body)) (for/list ([i bindings]) (desugar (binding-value i))))]

    [withS* (bindings body)
           (if (= (length bindings) 1)
               (app (fun (list (param (binding-id (car bindings)) (binding-tipo (car bindings))))
                         (desugar body)) (list (desugar (binding-value (car bindings)))))
               (app (fun (list (param (binding-id (car bindings)) (binding-tipo (car bindings))))
                         (desugar (withS* (cdr bindings) body))) (list (desugar (binding-value (car bindings))))))]

    [funS (params  rtype body) (fun params (desugar body))]
    
    [appS (fun args) (app (desugar fun) (for/list ([i args]) (desugar i)))]))
