#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error (string-append "lookup: Hay un identificador libre: " (symbol->string name)))]
    [aSub (nombre value subE)
          (if (equal? name nombre)
              value
              (lookup name subE))]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> CFWAE-Value
(define (interp expr ds)
      (type-case CFWAE expr
        [id (i) (lookup i ds)]
        [num (n) (numV n)]
        [op (f args)
            (let ([values (for/list ([i args]) (numV-n (interp i ds)))])
              (numV (apply f values)))]
    
        [with* (bindings body)
               (if (empty? bindings)
                   (interp body ds)
                   (let ([bid (car bindings)])
                     (interp (with* (cdr bindings) body) (aSub (binding-id bid) (interp (binding-value bid) ds) ds))))]
    
        [fun (params body) (closure params body ds)]

        [if0 (cond then else)
             (with-handlers ([exn:fail:contract? (lambda (exn) (error "interp: Símbolo no esperado la condicional de if0, no es un número"))])
               (if (equal? (numV-n (interp cond ds)) 0)
                   (interp then ds)
                   (interp else ds)))]

        [app (func args)
             (let* ([clo (interp func ds)] [params (closure-param clo)] [body (closure-body clo)] [env (closure-env clo)])
               (for/list ([i params] [j args]) (set! env (aSub i (interp j ds) env)))
               (interp body env))]))
