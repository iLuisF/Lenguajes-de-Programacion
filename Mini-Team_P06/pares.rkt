#lang racket

;Regresa el ultimo elemento de una lista.
;ultimo :: lista -> elemento
(define (ultimo l)
  (if (= (longitud l) 1)
      (car l)
      (ultimo (cdr l))))

;Regresa la longitud de una lista
;longitud :: lista -> Int
(define (longitud l)
  (if (vacia l)
      0
      (+ 1 (longitud (cdr l)))))

;Si la lista es vacia regresa #t, en otro caso #f
;vacia :: lista -> Bool
(define (vacia l)
  (match l
    ['() #t]
    [(cons x xs) #f]))

;Regresa una lista sin el ultimo elemento.
;elimina-ultimo :: lista -> lista
(define (elimina-ultimo l)
  (match l
    ['() (error "lista vacia")]
    [(cons x '()) '{}]
    [(cons x xs) (cons x (elimina-ultimo xs))]))

;Regresa la concatenación de dos listas.
;concatena :: lista -> lista
(define (concatena l1 l2)
  (if (vacia l1)
      l2
      (cons (car l1) (concatena (cdr l1) l2))))
      
;Dada una lista de la forma '(a0 a1 a2... an ao ap) regresa una lista
;pares cuyos elementos son (a0 ap) (a1 ao) (a2 an)...
;pares1 :: lista -> lista
(define (pares1 l)
  (cond
    [(vacia l) l]
    [(= (longitud l) 1) (concatena (list (list (car l))) '{})]
    [else (concatena (list (list (car l) (ultimo l))) (pares1 (cdr (elimina-ultimo l))))]))

;Parte de recurción de cola 

;Soluciona el mismo problema que (pares 1).
;pares2 :: lista -> lista
(define (pares2 l)
  (pares2-tail l '{}))

;Soluciona el mismo problema que (elimina-ultimo l).
;elimina-ultimo2 :: lista -> lista
(define (elimina-ultimo2 l)
  (elimina-ultimo2-tail l '{}))

;Soluciona el mismo problema que (elimina-ultimo l) pero con acumulador.
;elmina-ultimo2-tail :: lista -> lista -> lista
(define (elimina-ultimo2-tail l acc)
  (if (= (longitud2 l) 1)
      (reversa acc) 
      (elimina-ultimo2-tail (cdr l) (cons (car l) acc)))) 

;Soluciona el mismo problema que (longitud l) pero con acumulador.
;longitud2-tail :: lista -> Int -> Int
(define (longitud2-tail l acc)
  (if (vacia l)
      acc
      (longitud2-tail (cdr l) (+ acc 1))))

;Soluciona el mismo problema que (longitud l).
;longitud2 :: lista -> Int
(define (longitud2 l)
  (longitud2-tail l 0))

;Soluciona el mismo problema que (reversa l) pero con acumulador.
;reversa-tail :: lista -> lista -> lista
(define (reversa-tail l acc)
  (if (vacia l)
      acc
      (reversa-tail (cdr l) (cons (car l) acc))))

;Regresa una lista en el orden contrario.
;reversa :: lista -> lista
(define (reversa l)
  (reversa-tail l '{}))

;Soluciona el mismo problema que (concatena l1 l2) pero con acumulador.
;concatena2-tail :: lista -> lista > lista -> lista
(define (concatena2-tail l1 l2 acc)
  (cond
    [(and (vacia l1) (vacia l2)) (reversa acc)]
    [(and (vacia l1) (not (vacia l2))) (concatena2-tail l1 (cdr l2) (cons (car l2) acc))]
    [else (concatena2-tail (cdr l1) l2 (cons (car l1) acc))]))

;Soluciona el mismo problema que (concatena l1 l2).
;concatena2 :: lista -> lista > lista
(define (concatena2 l1 l2)
  (concatena2-tail l1 l2 '{}))

;Soluciona el mismo problema que (pares 1) pero con acumulador.
;pares2-tail :: lista -> lista -> lista
(define (pares2-tail l acc)
  (cond
    [(vacia l) acc]
    [(= (longitud2 l) 1) (concatena2 acc (list (list (car l))))]
    [else (pares2-tail (cdr (elimina-ultimo2 l)) (concatena2 acc (list (list (car l) (ultimo l)))))]))

;Parte de CPS

;Soluciona el mismo problema que (pares 1).
;pares3 :: lista -> lista
(define (pares3 l)
  (pares3-cps l (lambda (x) x)))

;Soluciona el mismo problema que (longitud l).
;longitud3 :: lista -> Int
(define (longitud3 l)
  (longitud3-cps l (lambda (x) x)))

;Soluciona el mismo problema que (longitud l) pero con cps.
;longitud3-cps :: lista -> fun -> Int
(define (longitud3-cps l k)
  (if (vacia l)
      (k 0)
      (longitud3-cps (cdr l) (lambda (v) (k (+ 1 v))))))

;Soluciona el mismo problema que (concatena l1 l2).
;concatena3 :: lista -> lista > lista
(define (concatena3 l1 l2)
  (concatena3-cps l1 l2 (lambda (x) x)))

;Soluciona el mismo problema que (concatena l1 l2) pero con cps.
;concatena3-cps :: lista -> lista > fun -> lista
(define (concatena3-cps l1 l2 k)
  (if (vacia l1)
      (k l2)
      (concatena3-cps (cdr l1) l2 (lambda(v) (k (cons (car l1) v))))))

;Soluciona el mismo problema que (elimina-ultimo l).
;elimina-ultimo3 :: lista -> lista
(define (elimina-ultimo3 l)
  (elimina-ultimo3-cps l (lambda (x) x)))

;Soluciona el mismo problema que (elimina-ultimo l) pero con cps.
;elmina-ultimo3-cps :: lista -> fun -> lista
(define (elimina-ultimo3-cps l k)
  (if (= (longitud3 l) 1)
      (k '{})
      (elimina-ultimo3-cps (cdr l) (lambda (v) (k (cons (car l) v))))))

;Soluciona el mismo problema que (pares 1) pero con cps.
;pares3-cps :: lista -> fun -> lista
(define (pares3-cps l k)
 (cond
   [(vacia l) (k l)]
   [(= (longitud3 l) 1) (k (list (list (car l))))]
   [else (pares3-cps (cdr (elimina-ultimo3 l)) (lambda (v) (k (concatena3 (list (list (car l) (ultimo l))) v))))]))

          








