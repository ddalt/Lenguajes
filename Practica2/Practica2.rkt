#lang plai
;; Ejercicio 1
;; Función que recibe n, r y devuelve el conjunto con los primeros r múltiplos de n.
;; multiplos: number number -> (listof number)
(define (multiplos n r)
  (cond
    [(= r 1) (list n)]
    [(> r 1) (append (multiplos n (- r 1)) (list (* n r)))]
    [else null]))

;; Ejercicio 2
;; Predicado que nos dice si un número m es divisor de otro número n.
;; Si el parámetro recibido es cero, se devuelve un error.
;; divisor?: number number -> number
(define (divisor? m n)
  (if (> m 0)
      (zero? (remainder n m))
      (error 'mes "el cero no es divisor de nadie")))

;; Ejercicio 3
;; Función que nos da el una lista de divisores de un número pasado como parámetro
;; divisores: number -> (listof number)
(define (divisores n)
    (filter (λ (x) (divisor? x n)) (menores n)))

;; Función auxiliar
;; Funcion que nos da una lista con con los números menores (sin el 0) de un número pasado como parámetro
;; menores: number -> (listof number)
(define (menores n)
  (if (= 0 n)
      '()
      (cond
        [(> n 0) (append  (menores (- n 1)) (list n))]
        [else (menores (- n))])))

;; Ejercicio 4
;; Función que recibe un elemento a, una lista l y decide si a pertenece a l.
;; pertenece: a (listof a) -> boolean
(define (pertenece? a l)
  (if(null? l)
     #f
     (if(equal? a (car l)) #t (pertenece? a (cdr l)))))

;; Ejercicio 5
;; Función que recibe una lista l con elementos. Devuelve una lista sin repeticiones con los elementos de l.
;; eliminaRep: (listof a) -> (listof a)
(define (eliminaRepetidos lista) 
     (define otraLista '()) 
      (begin (for-each 
         (lambda (x) 
          (if (pertenece? x otraLista) 
           #t 
          (set! otraLista (cons x otraLista)))) 
         lista) 
        (reverse otraLista)))

;; Estructura que nos permite modelar puntos en el plano.
;; Sirve para modelar figuras geométricas.
(struct Punto (x y) #:inspector #f)

;; Ejercicio 6
;; Funcion que nos permite calcular el punto equidistante entre dos puntos.
;; Si alguno de los dos parámetros recibidos no es un punto, lanza un error
;; punto-medio: Punto Punto -> Punto
(define (punto-medio p q)
  (if(or (not(Punto? p)) (not(Punto? q)))
     error"Alguno de los los dos no es un punto")
  (let ([x1 (Punto-x p)]
        [y1 (Punto-y p)]
        [x2 (Punto-x q)]
        [y2 (Punto-y q)])
  (Punto(/ (+ x1 x2) 2) (/ (+ y1 y2) 2))
  ))

;; Ejercicio 7
;; Funcion que nos permite calcular la distancia entre dos puntos.
;; Si alguno de los dos parámetros recibidos no es un punto, lanza un error
;; distancia: Punto Punto -> number
(define (distancia p q)
  (if(or (not(Punto? p)) (not(Punto? q)))
     error"Alguno de los los dos no es un punto")
  (let ([x1 (Punto-x p)]
        [y1 (Punto-y p)]
        [x2 (Punto-x q)]
        [y2 (Punto-y q)])
    (sqrt(+ (expt(- x2 x1) 2) (expt(- y2 y1) 2)))
    ))

;; Ejercicio 8
;; Definición del tipo abstracto de datos Figura
(define-type Figura
  [Circulo (centro Punto?) (radio number?)]
  [Triangulo (a Punto?) (b Punto?) (c Punto?)]
  [Cuadrado (esq Punto?) (side number?)]
  [Rectangulo (esq Punto?) (a number?) (b number?)])

;; Ejercicio 9
;; Función que recibe una figura y calcula su perímetro.
;; perimetro: Figura -> number
(define (perimetro fig)
  (type-case Figura fig
    [Circulo (centro radio) (* 2 pi radio)]
    [Triangulo (a b c) (+ (distancia a b) (distancia b c) (distancia a c))]
    [Cuadrado (esq side) (* 4 side)]
    [Rectangulo (esq a b) (* 2 (+ a b))]))

;; Ejercicio 10
;; Función que recibe una figura y calcula su área.
;; area: Figura -> number
(define (area fig)
   (type-case Figura fig
    [Circulo (centro radio) (* radio radio pi)]
    [Triangulo (a b c) (areaHeron (distancia a b) (distancia b c) (distancia a c))]
    [Cuadrado (esq side) (* side side)]
    [Rectangulo (esq a b) (* a b)]))

;; Función auxiliar para la función area
;; Dadas las longitudes de los tres lados de un triángulo, nos regresa el área de dicho triángulo, calculada usando la fórmula de Herón.
;; areaHeron : number number number -> number
(define (areaHeron a b c)
  (let ([s (/ (+ a b c) 2)])
    (sqrt (* s (- s a) (- s b) (- s c)))))

;; Punto extra
;; Función que nos da el elemento más repetido en una lista. 
;; Si hay dos o más elementos repetidos el mismo número de veces, devuelve el primero en aparecer de izquierda a derecha.
;; masRepetido (listof a) -> number
(define (masRepetido lista)
  (if (empty? lista)
      (error "la lista es vacía")
      (masRepetidoAux2 (masRepetidoAux lista '()) '())))

;; Función auxiliar para la función masRepetidos
;; Dada una lista L, nos devuelve una lista que contiene sublistas con todas las apariciones de un elemento en L.
;; masRepetidosAux : (listof a) (listof a) -> (listof (listof a))
(define (masRepetidoAux l aux)
  (if (empty? l)
      aux
      (let ([x (filter (lambda (n) (equal? n (car l))) l)])
        (masRepetidoAux (cdr l) (append aux (list x))))))

;; Función auxiliar para la función masRepetido
;; Dada una lista L conformada por listas, nos devuelve el primer elemento de la lista de mayor longitud en L
;; masRepetidoAux2 : (listof a) (listof a) -> a
(define (masRepetidoAux2 l1 l2)
  (if (empty? l1)
      (car l2)
      (if (> (length (car l1)) (length l2))
          (masRepetidoAux2 (cdr l1) (car l1))
          (masRepetidoAux2 (cdr l1) l2))))


(define a (Punto 2 2))
(define b (Punto 2 8))
(define c (Circulo (Punto 0 0) 1))


(test (perimetro c) 6.283185307179586)
(test (distancia a b) 6)
(test (punto-medio a b) (Punto 2 5))




  