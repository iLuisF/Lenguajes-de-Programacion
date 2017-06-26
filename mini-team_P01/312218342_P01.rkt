#lang plai

;Todas las sublistas de una lista.
; sublistas :: [a] -> [a]
(define (sublistas l)
	{if (empty? l)
		'(())
		(append
		(sublistas (cdr l))
		(aux-sublistas (car l) (sublistas (cdr l))))})

; Calcula la mayor de las sumas de las sublistas de una lista..
; max-sums :: [Integer] -> Integer
(define (max-sumas l)
  (suma-lista (car (reverse (sublistas l)))))

;Auxiliares

;aux-sublistas :: a -> [a]
(define (aux-sublistas n l)
	(if (empty? l)
	'()
	(append 
	(list (append (list n) (car l))) 
	(aux-sublistas n (cdr l)))))

;Suma todos los numeros de una lista.
;suma-lista :: [Integer] -> Integer
(define (suma-lista l)
  (if (empty? l)
      0
      (+ (car l) (max-sumas (cdr l)))))
