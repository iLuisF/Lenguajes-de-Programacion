#lang plai

(require "practica2.rkt")

;perimetro :: Figura -> Real
(test (perimetro (triangulo 17 29 20)) 66)
(test (perimetro (triangulo 17 29 15)) 61)
(test (perimetro (cuadrado 2)) 8)
(test (perimetro (cuadrado 3)) 12)
(test (perimetro (rectangulo 2 4)) 12)
(test (perimetro (rectangulo 3 4)) 14)
(test (perimetro (rombo 5 8 3)) 20)
(test (perimetro (rombo 7 10 15)) 28)
(test (perimetro (paralelogramo 5 6 7)) 22)
(test (perimetro (paralelogramo 10 15 20)) 50)
(test (perimetro (circulo 10)) 31.41)
(test (perimetro (circulo 15)) 47.12)
(test (perimetro (elipse 7 12)) 61.72)
(test (perimetro (elipse 14 24)) 123.44)

;area :: Figura -> real
(test (area (triangulo 17 29 20)) 165.69)
(test (area (triangulo 17 29 15)) 97.84)
(test (area (cuadrado 2)) 4)
(test (area (cuadrado 3)) 9)
(test (area (rectangulo 2 4)) 8)
(test (area (rectangulo 3 4)) 12)
(test (area (rombo 5 8 3)) 12)
(test (area (rombo 7 10 15)) 75)
(test (area (paralelogramo 5 6 7)) 42)
(test (area (paralelogramo 10 15 20))300)
(test (area (circulo 10)) 78.53)
(test (area (circulo 15)) 176.71)
(test (area (elipse 7 12)) 263.89)
(test (area (elipse 14 24)) 1055.57)

;Ejercicio 2: Funciones simples.

(test (Funcion->string (mul (cte 2) (x))) "(2*x)")
(test (Funcion->string (sum (cte 7) (mul (cte 1) (x)))) "(7+(1*x))")
(test (evalua (mul (cte 2) (x)) 1729) (mul (cte 2) (cte 1729)))
(test (evalua (sum (cte 7) (mul (cte 1) (x))) 2) (sum (cte 7) (mul (cte 1) (cte 2))))
(test (deriva (mul (cte 2) (x))) (sum (mul (cte 2) (cte 1)) (mul (x) (cte 0))))
(test (deriva (sum (cte 7) (mul (cte 1) (x))))
      (sum (cte 0) (sum (mul (cte 1) (cte 1)) (mul (x) (cte 0)))))

;Ejercicio 3: Pilas y Colas.

(define p1 (pila (nodo 1 (nodo 2 (nodo 3 (vacio))))))
(define p2 (pila (nodo 10 (nodo 15 (nodo 20 (vacio))))))
(test (calc-p p1) (pila (nodo 1 (nodo 2 (nodo 3 (vacio))))))
(test (calc-p p2) (pila (nodo 10 (nodo 15 (nodo 20 (vacio))))))
(test (calc-p (mete-p 4 p1)) (pila (nodo 1 (nodo 2 (nodo 3 (nodo 4 (vacio)))))))
(test (calc-p (mete-p 4 p2)) (pila (nodo 10 (nodo 15 (nodo 20 (nodo 4 (vacio)))))))
(test (calc-p (saca-p p1)) (pila (nodo 1 (nodo 2 (vacio)))))
(test (calc-p (saca-p p2)) (pila (nodo 10 (nodo 15 (vacio)))))
(test (calc-p (mira-p p1)) 3)
(test (calc-p (mira-p p2)) 20)

(define c1 (cola (nodo 1 (nodo 2 (nodo 3 (vacio))))))
(define c2 (cola (nodo 10 (nodo 15 (nodo 20 (vacio))))))
(test (calc-c c1) (cola (nodo 1 (nodo 2 (nodo 3 (vacio))))))
(test (calc-c c2) (cola (nodo 10 (nodo 15 (nodo 20 (vacio))))))
(test (calc-c (mete-c 4 c1)) (cola (nodo 1 (nodo 2 (nodo 3 (nodo 4 (vacio)))))))
(test (calc-c (mete-c 4 c2)) (cola (nodo 10 (nodo 15 (nodo 20 (nodo 4 (vacio)))))))
(test (calc-c (saca-c c1)) (cola (nodo 2 (nodo 3 (vacio)))))
(test (calc-c (saca-c c2)) (cola (nodo 15 (nodo 20 (vacio)))))
(test (calc-c (mira-c c1)) 1)
(test (calc-c (mira-c c2)) 10)

(define a (conjunto '(1 7 2 9)))
(define b (conjunto '(1 2 3 4)))

;Ejercicio 4: Conjuntos.

(test (calc-cjto (conjunto '(1 1 7 2 9))) (conjunto '(1 7 2 9)))
(test (calc-cjto b) (conjunto '(1 2 3 4)))

;esvacio? :: Conjunto -> Boolean
(test (calc-cjto (esvacio? a)) #f)
(test (calc-cjto (esvacio? (conjunto '()))) #t)

;contiene? :: Conjunto -> Elem -> Boolean
(test (calc-cjto (contiene? a 1)) #t)
(test (calc-cjto (contiene? a 10)) #f)

;agrega :: Conjunto -> Elem -> List
(test (calc-cjto (agrega a 9)) (conjunto '(1 7 2 9)))
(test (calc-cjto (agrega a 5)) (conjunto '(1 7 2 9 5)))

;union :: Conjunto -> Conjunto -> Conjunto
(test (calc-cjto (union a b)) (conjunto '(1 7 2 9 3 4)))
(test (calc-cjto (union a (conjunto '()))) a)

;interseccion :: Conjunto -> Conjunto -> Conjunto
(test (calc-cjto (interseccion a b)) (conjunto '(1 2)))
(test (calc-cjto (interseccion b a)) (conjunto '(1 2)))

;diferencia :: List -> List -> Lista
(test (calc-cjto (diferencia a b)) (conjunto '(7 9)))
(test (calc-cjto (diferencia b a)) (conjunto '(3 4)))





