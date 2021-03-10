#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE-Value
;; (define (lookup name ds)
(define (lookupType name context)
  (type-case Type-Context context
    [phi () (error (string-append "lookup: No hay un tipo asociado: " (symbol->string name)))]
    [gamma (id tipo rest)
          (if (equal? name id)
              tipo
              (lookupType name rest))]))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof CFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
    [idS (i) (lookupType i context)]
    [numS (n) (numberT)]
    [boolS (b) (booleanT)]
    
    [iFS (cond then else) (if (not (booleanT? (typeof cond context)))
                              (error (string-append "if: Type error\nConditional's test-expr type must be a boolean\nGiven: "
                                                    (~v (typeof cond context))))
                              (if (equal? (typeof then context) (typeof else context))
                                  (typeof then context)
                                  (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr")))]
    
    [condS (cases) (let* ([aux (remove (last cases) cases)]
                          [condts (for/list ([i aux]) (typeof (condition-test-expr i) context))]
                          [thens (cons (typeof (else-cond-else-expr (last cases)) context) (for/list ([i aux]) (typeof (condition-then-expr i) context)))]
                          [test-type (remove* (list (first condts)) condts)]
                          [then-type (remove* (list (first thens)) thens)])
                     (if (empty? test-type)
                         (if (empty? then-type)
                             (first thens)
                             (error "typeof: Type error\nconditionals must have same type in then-expr and else-expr"))
                         (error (string-append "typeof: Type error\nConditional's test-expr type must be a boolean\nGiven: "
                                               (~v (first test-type))))))]
    
    [opS (f args) (let ([args-type (for/list ([i args]) (typeof i context))] [inType (get-type-fun2 f)])
                    (begin (for/list ([i args]) (let ([tipoI (typeof i context)])
                                                  (if (equal? inType tipoI)
                                                    tipoI
                                                    (error (string-append "typeof: Error in parameter " (~v i)
                                                                          "\nExpected type: " (~v inType)
                                                                          "\nGiven type: " (~v tipoI))))))
                           inType))]
    
    [withS (binds body) (begin (for/list ([bid binds]) (let ([tipoI (typeof (binding-value bid) context)])
                                                         (if (equal? (binding-tipo bid) tipoI)
                                                             (set! context (gamma (binding-id bid) (binding-tipo bid) context))
                                                             (error (string-append "with: Error in binding " (~v (binding-id bid))
                                                                                   "\nExpected type: " (~v (binding-tipo bid))
                                                                                   "\nGiven type: " (~v tipoI))))))
                               (typeof body context))]
  
    [withS* (binds body) (begin (for/list ([bid binds]) (let ([tipoI (typeof (binding-value bid) context)])
                                                         (if (equal? (binding-tipo bid) tipoI)
                                                             (set! context (gamma (binding-id bid) (binding-tipo bid) context))
                                                             (error (string-append "with*: Error in binding " (~v (binding-id bid))
                                                                                   "\nExpected type: " (~v (binding-tipo bid))
                                                                                   "\nGiven type: " (~v tipoI))))))
                               (typeof body context))]
    
    [funS (params rType body) (begin
                                (for/list ([p params]) (set! context (gamma (param-param p) (param-tipo p) context)))
                                (if (equal? (last (funT-params rType)) (typeof body context))
                                    rType
                                    (error  "error en funS")))]
    
    [appS (fun args) (let ([tipos (funT-params (typeof fun context))])
                       (begin (for/vector ([i (drop-right tipos 1)] [j args]) (if (equal? i (typeof j context))
                                                            'null
                                                            (error (string-append "app: Type error:\nParameter's type doesn't match expected types\nGiven: "
                                                                                  (~v (typeof j context))
                                                                                  "\nExpected: " (~v i)))))
                              (last tipos)))]))

;; Función auxiliar que nos dice qué tipo de datos recibe la función f:
;; f recibe (numberT) si es una comparación arimtética
;; (booleanT) si f es un operador lógico
;; (numberT) en otro caso (i.e. f es un función arimética normal)
;; get-type-fun2: procedure -> Type
(define (get-type-fun2 f)
  (cond
    [(member f (list = < > >= <= zero?)) (numberT)]
    [(member f (list anD oR noT)) (booleanT)]
    [else (numberT)]))

(define (prueba exp)
  (typeof (parse exp) (phi)))