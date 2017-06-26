#lang plai

;ec-lin :: Int -> Int -> Int
;Resuelve la ecucion Ax + B = 0
(define (ec-lin a b)
  (/ (* -1 b) a))

;area-heron :: Int -> Int -> Int -> Int
;Realiza el area de heron como esta definida
(define (area-heron a b c)
  (let ([S (/ (+ a b c) 2)])
   (sqrt (* S (- S a) (- S b) (- S c)))))

; triangulo-rec? :: Int -> Int -> Int -> Bool
; Dice si un triangulo es rectangulo o no con
; la longitud de sus lados
(define (triangulo-rec? a b c)
  (let ([C (max a (max b c))])
    (cond
      [(= C a) (= (+ (expt c 2) (expt b 2)) (expt a 2))]
      [(= C b) (= (+ (expt c 2) (expt a 2)) (expt b 2))]
      [else (= (+ (expt a 2) (expt b 2)) (expt c 2))])))

; s-digito :: Int -> Int
; Ejemplo: sdigito (984) = sdigito (9+8+4) = sdigito (21) = sdigito (2+1) = 3
(define (s-digito n)
  (if (equal? (num-digitos n) 1)
      n
      (s-digito(suma-digitos n))
   )
)

; Invierte :: Int -> Int
; Invierte un número usando la notación exponencial.
(define (invierte n)
  (if (< n 10)
      n
      (+
       (* (modulo n 10) (expt 10 (- (num-digitos n) 1)))
       (invierte (truncate (/ n 10)))
       )
   )
 )

; elimina-dup :: [a] -> [a]
; dada una lista l elimina los elementos
; duplicados adyacentes de una lista dejando únicamente una aparición de cada elemento
(define (elimina-dup l)
  (if (empty? l)
      '{}
      (cons (car l) (elimina-dup(elimina-aux (car l) (cdr l))))))

;binarios :: [Int] -> [String]
;regresa una lista con la representacion binaria de los elementos en [Int]
(define (binarios n)
  (mapea cambia-cadena (mapea cambia-binario n)))

;primos :: [Int]x -> [Int]
; regresa una lista con los numeros primos dentro de x
(define (primos l)
  (cond
    [(empty? l) '()]
    [(primo? (car l)) (cons (car l) (primos (cdr l)))]
    [else (primos (cdr l))]))

;reversar :: [a] -> [a]
; regresar la reversa de la lista usando la funcion foldr 
(define (reversar l)
  (foldr (lambda (v l)
           (concatena l (list v) )) '() l))

;reversal :: [a] -> [a]
; regresal la reversa de la lista usando la funcion foldl
(define (reversal l )
  (foldl cons '() l))

;concatena :: lista -> lista -> lista
;une nos listas de la forma l3 = l1 ++ l2
(define (concatena l1 l2)
  (if (empty? l1)
      l2
      (cons (car l1) (concatena (cdr l1) l2))))

;mapea :: funcion f -> [a] ->[b]
;aplica la funcion f a todos los elementos de la lista y regresa una lista con
;los resultados
(define (mapea f l1)
  (if (null? l1)
      '()
      (cons (f (car l1)) (mapea f (cdr l1))
   )
  )
 )

;filtra :: predicado pred -> [a] -> [pred(a)]
; regrea la lista de los elementos de [a] que cumplen el predicado pred
(define (filtra p l)
  (cond
    [(empty? l) l]
    [(p (car l)) (cons (car l) (filtra p (cdr l)))]
    [else (filtra p (cdr l))]))

;toma :: [a] -> n 
;toma los primeros n elementos de la lista 
(define (toma l n )
  (cond
    [(zero? n) '{}]
    [(empty? l) l]
    [else (cons (car l) (toma (cdr l) (- n 1)))]))

;quita :: [a] -> n 
;elimina los primeros n elementos de la lista 
(define (quita l n )
  (cond
   [(zero? n) l]
    [(empty? l) l]
    [else (quita (cdr l) (- n 1))]))

;Número mayor de dos números.
(letrec ([numero-mayor (lambda (x y)
                         (if (> x y) x y))])
  (numero-mayor 1834 1729))

;Calcula la suma de los primeros 100 naturales.
(letrec ([suma-naturales (lambda (n)
                           (if (zero? n)
                               0 (+ n (suma-naturales (- n 1)))))])
  (suma-naturales 100))

;Funciones auxiliares.

; max :: Int -> Int -> Int
;Auxiliar para triangulo-rec? regresa el maximo
;de entre dos números 
(define (max x y)
  (if (> x y)
      x
      y))

; num-digitos :: Int -> Int
;Auxiliar: Número de digitos que componen un número.
;Ejemplo: num-digitos(984) = 3
(define (num-digitos n)
  (if (< n 10)
      1
      (+ 1 (num-digitos (/ n 10))))
)

; suma-digitos :: Int -> Int
;Auxiliar: Suma total de los digitos que componen un número.
;Ejemplo: suma-digitos(984) = 21
(define (suma-digitos n)
  (if (zero? n)
      0
      (+ (modulo n 10) (suma-digitos(truncate (/ n 10)))
   )))

;cambia-binario :: Int -> [Int]
;funcion auxiliarr que da una lista con los digitos de un numero en su forma binaria
(define (cambia-binario n)
  (cond
    [(= n 0) '[0]]
    [(= n 1) '[1]]
    [else (append (cambia-binario (truncate (/ n 2))) (list (modulo n 2)))]))

;cabia-cadena :: [Int] -> String
; funcion auciliar que regresa la representacion en cadena de la lista l
(define (cambia-cadena x)
  (cond
      [(empty? x) ""]
      [(= (car x ) 1) (string-append "1" (cambia-cadena (cdr x)))]
      [(= (car x ) 0) (string-append "0" (cambia-cadena (cdr x)))]))

;factorial Int -> Int
;regresa el factorial de un numero n 
(define (factorial n)
  (cond
    [(= n 0) 1]
    [else ( * n (factorial (sub1 n)))]))

; primo? :: Int -> Bool
; Auxiliar: determina si un numero es primo
(define (primo? n)
  (if (= (modulo (+ (factorial (- n 1)) 1) n) 0)
      (if (= n 1 )
          #f
          #t)
      #f))

; Auxiliar que elimina todas las repeticiones adyacentes de un numero en una lista
(define (elimina-aux x l)
  (cond
    [(empty? l) '{}]
    [(eq? x (car l)) (elimina-aux x (cdr l))]
    [else l]))
     

