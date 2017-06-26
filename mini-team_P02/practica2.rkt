#lang plai

;Ejercicio 1: Figuras geométricas.

;Tipo de dato abstracto para trabajar con figuras.
(define-type Figura
  [triangulo (a real?) (b real?) (c real?)]
  [cuadrado (a real?)]
  [rectangulo (a real?) (b real?)]
  [rombo (a real?) (D real?) (d real?)]
  [paralelogramo (a real?) (b real?) (h real?)]
  [circulo (D real?)]
  [elipse (a real?) (b real?)])

;Calcula el perimetro de una figurara dada.
;perimetro :: Figura -> real
(define (perimetro fig)
  (type-case Figura fig
    [triangulo (a b c) (+ a b c)]
    [cuadrado (a) (* 4 a)]
    [rectangulo (a b) (+ (* 2 a) (* 2 b))]
    [rombo (a D d) (* a 4)]
    [paralelogramo (a b h) (* 2 (+ a b))]
    [circulo (D) (* 2 pi (/ D 2))]
    [elipse (a b) (* 2 pi (sqrt(/ (+ (expt a 2) (expt b 2)) 2)))]))

;Calcula el área de una figura dada.
;area :: Figura -> real
(define (area fig)
  (type-case Figura fig
    [triangulo (a b c) (let ([S (/ (+ a b c) 2)])
                         (sqrt (* S (- S a) (- S b) (- S c))))]
    [cuadrado (a) (* a a)]
    [rectangulo (a b) (* a b)]
    [rombo (a D d) (/ (* D d) 2)]
    [paralelogramo (a b h) (* b h)]
    [circulo (D) (* pi (/ (* D D) 4))]
    [elipse (a b) (* pi a b)]))

;Ejercicio 2: Funciones simples.

;Tipo de dato abstracto para trabajar con funciones simples.
(define-type Funcion
  [x]
  [cte (a integer?)]
  [sum (f Funcion?) (g Funcion?)]
  [mul (f Funcion?) (g Funcion?)]
  [pot (f Funcion?) (n integer?)])

;Convirte una Funcion a su representacion de Cadena.
;Funcion->String :: Funcion -> String
(define (Funcion->string fun)
  (type-case Funcion fun
    [x () "x"]
    [cte (n) (if (zero? n) "0" (integer->string n))]
    [sum (f g) (string-append "(" (Funcion->string f) "+" (Funcion->string g) ")")]
    [mul (f g) (string-append "(" (Funcion->string f) "*" (Funcion->string g) ")")]
    [pot (f n) (string-append "(" (Funcion->string f) "^" (integer->string n) ")")]))

;Funcion auxiliar que transforma un entero a su representacion de cadena 1234 = "1234"
;integer->string :: Int -> String.
(define (integer->string n)
  (if (zero? n)
      ""
      (string-append(integer->string(quotient n 10)) (string(integer->char (+ 48 (remainder n 10)))))))

;Evalua la función evaluada en v, es decir, f(v).
;evalua :: Funcion -> Int
(define (evalua fun v)
   (type-case Funcion fun
     [x () (cte v)]
     [cte (n) (cte n)]
     [sum (f g) (sum (evalua f v) (evalua g v))]
     [mul (f g) (mul (evalua f v) (evalua g v))]
     [pot (f g) (pot (evalua f v) (evalua g v))]))
  
;Derivada de una función.
;derivada :: Funcion -> Funcion
(define (deriva fun)
  (type-case Funcion fun
     [x () (cte 1)]
     [cte (n) (cte 0)]
     [sum (f g) (sum (deriva f) (deriva g))]
     [mul (f g) (sum (mul f (deriva g)) (mul g (deriva f)))]
     [pot (f n) (mul (mul (cte n) (pot f (- n 1))) (deriva f))]))

;Ejercicio 3: Pilas y Colas.

(define (any? a) #t)

;Definición de una pila y cola.
(define-type Nodo
  [vacio]
  [nodo (elemento any?) (siguiente Nodo?)]
  [pila (tope Nodo?)]
  [cola (inicio Nodo?)]
  [mete-p (e any?) (p pila?)]
  [saca-p (p pila?)]
  [mira-p (p pila?)]
  [mete-c (e any?) (c cola?)]
  [saca-c (c cola?)]
  [mira-c (c cola?)])

;Evalua expresiones del tipo Pila.
(define (calc-p p)
  (type-case Nodo p
    [pila (tope) (pila tope)]
    [mete-p (e p) (agrega-final e p)]
    [saca-p (p) (elimina-ultimo p)]
    [mira-p (p) (primero p)]
    [else (error "funcion no definida")]))

;Evalua expresiones del tipo Cola.
(define (calc-c c)
  (type-case Nodo c
    [cola (inicio) (cola inicio)]
    [mete-c (e c) (agrega-final e c)]
    [saca-c (c) (elimina-primero c)]
    [mira-c (c) (primero c)]
    [else (error "funcion no definida")]))

;Auxiliar.
;Agrega un elemento al final de la lista.
(define (agrega-final x l )
  (type-case  Nodo l
    [vacio () (nodo  x (vacio))]
    [nodo (n m)(nodo n (agrega-final x m))]
    [pila (p) (pila (agrega-final x p))]
    [cola (c) (cola (agrega-final x c))]
    [else (error "funcion no definida")]))

;Auxiliar.
;Elimina el ultimo elemento de la lista.
(define (elimina-ultimo l)
  (type-case  Nodo l
    [vacio () (error "la lista esta vacia")]
    [nodo ( k m ) (if (vacio? m) (vacio) (nodo k (elimina-ultimo m)))]
    [pila (p) (if (vacio? l) (error "la pila esta vacia") (pila (elimina-ultimo p)))]
    [cola (c) (if (vacio? l) (error "la cola esta vacia") (cola (elimina-ultimo c)))]
    [else (error "funcion no definida")]))

;Auxiliar.
;Elimina el primer elemento de la lista 
(define (elimina-primero l)
  (type-case  Nodo l
    [vacio () (error "la lista esta vacia")]
    [nodo ( k m ) m]
    [pila (p) (if (vacio? l) (error "la pila esta vacia") (pila (elimina-ultimo p)))]
    [cola (c) (if (vacio? l) (error "la cola esta vacia") (cola (elimina-primero c)))]
    [else (error "funcion no definida")]))
  
;Auxiliar.
;Regresa el primer elemento de una lista
(define (primero l)
  (type-case Nodo l
    [vacio () (vacio)]
    [nodo ( k m ) k]
    [pila (p) (ultimo p)]
    [cola (c) (primero c)] 
    [else (error "funcion no definida")]))

;Auxiliar.
;Regresa el ultimo de la lista 
(define (ultimo l)
  (type-case Nodo l
    [vacio () (vacio)]
    [nodo ( k m ) (if (vacio? m) k (ultimo m))]
    [pila (p) (primero p)]
    [cola (c) (ultimo c)]
    [else (error "funcion no definida")]))

;Ejercicio 4: Conjuntos.

;Tipo de dato abstracto para trabajar con conjuntos.
(define-type Conjunto  
  [conjunto (l list?)]
  [esvacio? (c Conjunto?)]
  [contiene? (c Conjunto?) (e any?)]
  [agrega (c Conjunto?) (e any?)]
  [union (c1 Conjunto?) (c2 Conjunto?)]
  [interseccion (c1 Conjunto?) (c2 Conjunto?)]
  [diferencia (c1 Conjunto?) (c2 Conjunto?)])

;Evalua expreciones del tipo Conjunto.
(define (calc-cjto cjto)
  (type-case Conjunto cjto
  [conjunto (l) (conjunto (remove-duplicates l))]
  [esvacio? (c) (es-vacio-cjto c)] 
  [contiene? (c e) (contiene-cjto c e)]
  [agrega (c e) (agrega-cjto c e)]
  [union (c1 c2) (union-cjto c1 c2)]
  [interseccion (c1 c2) (interseccion-cjto c1 c2)]
  [diferencia (c1 c2) (diferencia-cjto c1 c2)]))

;Auxiliares para conjuntos.

;Verifica si un conjunto es vacio usando listas como estructura auxiliar.
;es-vacio-cjto :: Conjunto -> Boolean
(define (es-vacio-cjto c)
  (type-case Conjunto c
    [conjunto (l) (empty? l)]
    [else "Función no definida"]))

;Verifica si un elemento esta en el conjunto.
;contiene-cjto :: Conjunto -> Elem -> Boolean
(define (contiene-cjto c e)
  (type-case Conjunto c
    [conjunto (l) (contiene-aux l e)]
    [else "funcion no definida"]))

;Verifica si un elemento esta en una lista.
;contiene-aux :: List -> Elem -> Boolean
(define (contiene-aux  l e)
  (match l
    ['() #f]
    [(cons x xs) (or (equal? x e) (contiene-aux xs e))]))

;Agrega un elemento a un conjunto.
;agrega-cjto :: Conjunto -> Elem -> List
(define (agrega-cjto c e)
  (type-case Conjunto c
    [esvacio? (c) (conjunto (list e))]
    [conjunto (l) (if (contiene-aux l e) c (conjunto (append l (list e))))]    
    [else "funcion no definida"]))

;Union de dos conjuntos
;union-cjto :: Conjunto -> Conjunto -> Conjunto
(define (union-cjto c1 c2)
  (type-case Conjunto c1
    [conjunto (l) (type-case Conjunto c2
                    [conjunto (t) (conjunto (remove-duplicates (append l t)))]
                    [else "la funcion no esta definida"])]
    [else "funcion no definida"]))

;Intersección de dos conjuntos.
;interseccion-cjto :: Conjunto -> Conjunto -> Conjunto
(define (interseccion-cjto c1 c2)
  (type-case Conjunto c1
    [conjunto (l) (type-case Conjunto c2
                    [conjunto (t) (conjunto (interseccion-aux l t))]
                    [else "La funcion no esta definida."])]
              [else "La función no esta definida."]))

;Intersección de dos listas, es decir, elementos que comparten dos listas.
;interseccion-aux :: List -> List -> List
(define (interseccion-aux l1 l2)
  (match l1
    ['() '()]
    [(cons x xs)
     (if (contiene-aux l2 x)
         (cons x (interseccion-aux xs l2))
         (interseccion-aux xs l2))])
  )

;Diferencia de dos conjuntos, es decir, A - B respectivamente con los parametros.
;diferencia-cjto :: Conjunto -> Elem -> Boolean
(define (diferencia-cjto c1 c2)
   (type-case Conjunto c1
    [conjunto (l) (type-case Conjunto c2
                    [conjunto (t) (conjunto (diferencia-aux l t))]
                    [else "La funcion no esta definida."])]
              [else "La función no esta definida."]))

;Simulación de la operación diferencia de conjuntos en listas.
;diferencia-aux :: List -> List -> Lista
(define (diferencia-aux l1 l2)
  (match l1
    ['() '()]
    [(cons x xs)
     (if (contiene-aux l2 x)
         (diferencia-aux xs l2)
         (cons x (diferencia-aux xs l2)))])
  )