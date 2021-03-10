#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE-Value
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error (string-append "lookup: Variable libre: " (symbol->string name)))]
    [aSub (nombre value subE)
          (if (equal? name nombre)
              value
              (lookup name subE))]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
   (type-case CFWBAE expr
        [id (i) (lookup i ds)]
        [num (n) (numV n)]
        [bool (b) (boolV b)]
        [op (f args)
            (case (get-type-fun f)
                  [(1) (let ([values (for/list ([i args]) (numV-n (interp i ds)))])
                         (boolV (apply f values)))]
                  [(2) (let ([values (for/list ([i args]) (boolV-b (interp i ds)))])
                         (boolV (f values)))]
                  [(3) (let ([values (for/list ([i args]) (numV-n (interp i ds)))])
                         (numV (apply f values)))])]
     
        [iF (cond then else)
                (with-handlers ([exn:fail:contract? (lambda (exn) (error "interp: Símbolo no esperado la condicional de if, no es un booleano"))])
                  (if (boolV-b (interp cond ds))
                      (interp then ds)
                      (interp else ds)))]
     
        [fun (params body) (closure params body ds)]

        [app (func args)
             (let* ([clo (interp func ds)] [params (closure-param clo)] [body (closure-body clo)] [env (closure-env clo)])
               (for/list ([i params] [j args]) (set! env (aSub i (interp j ds) env)))
               (interp body env))]))

;; Función auxiliar que nos dice qué tipo de función es el parámetro f:
;; Regresa 1 si es f es una comparación arimtética
;; 2 si f es un operador lógico
;; 3 en otro caso (i.e. f es un función arimética normal)
;; get-type-fun: procedure -> number
(define (get-type-fun f)
  (cond
    [(member f (list = < > >= <= zero?)) 1]
    [(member f (list anD oR noT)) 2]
    [else 3]))

