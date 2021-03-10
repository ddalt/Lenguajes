#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(if) (if (not (equal? (length sexp) 4))
                 (error "parser: Falta la else-expresion")
                 (iFS (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp))))]
       [(=) (opS = (map (lambda (i) (parse i)) (cdr sexp)))]
       [(<=) (opS <= (map (lambda (i) (parse i)) (cdr sexp)))]
       [(>=) (opS >= (map (lambda (i) (parse i)) (cdr sexp)))]
       [(<) (opS < (map (lambda (i) (parse i)) (cdr sexp)))]
       [(>) (opS > (map (lambda (i) (parse i)) (cdr sexp)))]
       [(+) (opS + (map (lambda (i) (parse i)) (cdr sexp)))]
       [(-) (opS - (map (lambda (i) (parse i)) (cdr sexp)))]
       [(*) (opS * (map (lambda (i) (parse i)) (cdr sexp)))]
       [(/) (opS / (map (lambda (i) (parse i)) (cdr sexp)))]
       [(not) (opS noT (map (lambda (i) (parse i)) (cdr sexp)))]
       [(and) (opS anD (map (lambda (i) (parse i)) (cdr sexp)))]
       [(or) (opS oR (map (lambda (i) (parse i)) (cdr sexp)))]
       [(zero?) (if (= 1 (length (cdr sexp)))
                    (opS zero? (list (parse (last sexp))))
                    (error "parser: la funcion 'zero?' recibe un solo argumento"))]
       [(modulo) (opS modulo (map (lambda (i) (parse i)) (cdr sexp)))]
       [(expt) (opS expt (map (lambda (i) (parse i)) (cdr sexp)))]
       [(add1) (opS add1 (list (parse (second sexp))))]
       [(sub1) (opS sub1 (list (parse (second sexp))))]
       
       [(cond) (if (not (equal? (car (last sexp)) 'else))
                   (error "parser: Falta la else-expresion")
                   (condS (for/list ([i (cdr sexp)]) (parse-cond i))))]
       
       [(with) (withS (map (lambda (i) (binding (first i) (parse-type (third i)) (parse (fourth i)))) (second sexp)) (parse (third sexp)))]
       
       [(with*) (withS* (map (lambda (i) (binding (first i) (parse-type (third i)) (parse (fourth i)))) (second sexp)) (parse (third sexp)))]
       
       [(fun) (let* ([args (second sexp)] [dup (duplicados-args args)])
                (if (equal? dup #f)
                    (funS (for/list ([i args]) (param (car i) (parse-type (third i)))) (parse-type (fourth sexp)) (parse (fifth sexp)))
                    (error (string-append "parser: parámetro definido dos veces: " (symbol->string dup)))))]

       [else (let ([args (for/list ([i (second sexp)]) (parse i))])
               (if (or (symbol? (car sexp)) (= (length (second (car sexp))) (length args)))
                  (appS (parse (car sexp)) args)
                  (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función")))])]))



;; Función auxiliar que nos dice si la lista de argumentos de una función contiene algun
;; elemento duplicado, en cuyo caso lo regresa para entonces lanzar un error.
;; Si no hay elementos duplicados, regresamos #f
;; duplicados-args: (listof? symbol) -> symbol
;; duplicados-args: (listof? symbol) -> boolean
(define (duplicados-args args)
  (let ([simbolos (for/list ([i args]) (car i))])
    (check-duplicates simbolos)))

;; Toma una lista de parejas de condiciones y genera la sintáxis abstracta
;; de una condicional en CFWBAE
;; parse-cond: A -> SCFWBAE
;; parse-cond: s-expression -> SCFWBAE
(define (parse-cond cond-expr)
  (case (car cond-expr)
    [(else) (else-cond (parse (second cond-expr)))]
    [else (condition (parse (car cond-expr)) (parse (second cond-expr)))]))

;; Funcion auxiliar para 'parsear' los tipos en las expresiones recibidas a instancias de la
;; gramática Type.
;; Por ejemplo (parse-type 'number') = (numberT)
;; parse-type: string -> Type
(define (parse-type tipo)
  (case tipo
    [(number) (numberT)]
    [(boolean) (booleanT)]
    [else (funT (for/list ([i (remove '-> tipo)]) (parse-type i)))]))

;; Funciones auxiliares para parse.
;; Recordemos que el tipo de dato 'opS' recibe como primer parámetro algo de tipo 'procedure'
;; pero los operadors lógicos 'and' y 'or' de Racket no son procedures, por lo que nosotros debemos
;; deifinir nuestra propia versión de dichas funciones.

;; Regresa el resultado de hacer and sobre una lista de booleanos
;; anD: (listof? boolean) -> boolean
(define (anD l)
  (andmap (lambda (x) x) l))

;; Regresa el resultado de hacer or sobre una lista de booleanos
;; oR: (listof? boolean) -> boolean
(define (oR l)
  (ormap (lambda (x) x) l))

;; Regresa el resultado de hacer not sobre el primer elemento de una lista de booleanos
;; noT: (listof? boolean) -> boolean
(define (noT l)
  (not (car l)))