#lang plai

;;BP IN YOU AREAAA

;; Ejercicio 1
;; Predicado que recibe un número natural n y devuelve verdadero si n es par, falso en otro caso.
;; esPar? : numner -> boolean
(define (esPar? x)
  (if (equal? (modulo x 2) 0)
      #t
      #f))

;; Ejercicio 2
;; Función que recibe un número natural n y devuelve una lista en orden ascendente,
;; con los números menores o iguales a n.
;; menores : number -> (listof number)
(define (menores n)
  (if (< n 0)
      '()
      (for/list ([i (+ n 1)]) i)))

;; Ejercicio 3
;; Función que recibe un número natural n y devuelve una lista en orden ascendente,
;; con los números pares desde 0 hasta n.
;; pares: number -> (listof number)
(define (pares n)
  (filter even? (for/list ([i (+ n 1)]) i)))

;; Ejercicio 4
;; Función que recibe un número n y calcula la suma de los primeros n números naturales al cuadrado.
;; Esta función utiliza a fórmula conocida para esta cuenta.
;; suma-cuadrados: number -> number
(define (suma-cuadrados n)
  (if (< n 0)
      0
      (/ (* n (* (+ n 1) (+ (* 2 n) 1))) 6)))

;; Ejercicio 5
;; Función recursiva, que calcula la suma de los primeros n números naturales al cuadrado.
;; Esta función no utiliza la fórmula conocida, ni directa ni indirectamente.
;; suma-cuadrados: number -> number
(define (suma-cuadradosR n)
  (if (< n 0)
      0
      (if (= n 1)
          1
          (+ (* n n) (suma-cuadradosR (- n 1))))))

;; Función auxiliar: nos da el discriminante de un polinomio de grado 2
;; disc : number number number -> number
(define (disc a b c)
  (- (* b b) (* 4 a c)))

;; Ejercicio 6
;; Función que recibe los términos a, b y c,  de una expresión cuadrática y decide si la expresión tiene
;; raíces reales. La función verifica que el discriminante sea mayor o igual a cero.
;; raicesReales? : number number number -> boolean
(define (raicesReales? a b c)
  (if (< (disc a b c) 0)
      #f
      #t))

;; Ejercicio 7
;; Función que recibe tres números a, b y c y devuelve la primer raíz de la fórmula general (sumando la raíz cuadrada)
;; general1: number number number -> number
(define (general1 a b c)
  (if (equal? (raicesReales? a b c) #f)
      (error "La ecuación no tiene solución real")
      (/ (+ (- b) (sqrt (disc a b c))) (* 2 a))))

;; Ejercicio 8
;; Función que recibe tres números a, b y c y devuelve la segunda raíz de la fórmula general (restando la raíz cuadrada)
;; general2: number number number -> number
(define (general2 a b c)
  (if (equal? (raicesReales? a b c) #f)
      (error "La ecuación no tiene solución real")
      (/ (- (- b) (sqrt (disc a b c))) (* 2 a))))

;; Ejercicio 9
;; Función que nos da una lista invertida de la lista pasada como parámetro
;; reversa-lista: (listof a) -> (listof a)
(define (reversa-lista cadena)
  (if (equal? cadena '())
      cadena
      (append (reversa-lista (cdr cadena)) (list (car cadena)))))

;; Ejercicio 10
;; Predicado que nos dice si una lista contiene elementos que forman un palíndromo
;; palindromo-lista?: (listof a) -> Boolean
(define (palindromo? lista)
  (equal? lista (reversa-lista lista)))