#lang plai

(require "streams.rkt")

;; Procedimiento que genera una sucesión con los números naturales desde el 
;; número n.
;; genera-naturales: number -> Stream
(define (genera-naturales n)
   (scons n (λ () (genera-naturales (+ n 1)))))

;; Procedimiento que calcula el factorial de un número.
;; fact : number -> number
(define (fact n)
   (match n
      [0 1]
      [n (* n (fact (- n 1)))]))

;; Procedimiento que genera una sucesión de los factoriales desde el número n.
;; genera-factoriales: number -> Stream
(define (genera-factoriales n)
   (scons (fact n) (λ () (genera-factoriales (+ n 1)))))

;; Procedimiento que calcula el fibonacci de un número.
;; fibo : number -> number
(define (fibo n)
   (match n
      [0 1]
      [1 1]
      [n (+ (fibo (- n 1)) (fibo(- n 2)))]))

;; Procedimiento que genera una sucesión de fibonacci desde el número n.
;; genera-fibonaccis: number -> Stream
(define (genera-fibonaccies n)
   (scons (fibo n) (λ () (genera-fibonaccies (+ n 1)))))

;; Procedimiento que calcula el n-ésimo número triangular.
;; triangular: number -> number
(define (triangular n)
   (truncate (/ (* n (+ 1 n)) 2)))       
;; Procedimiento que genera una sucesión de números triangulares desde el número
;; n.
;; genera-triangulares: number -> Stream
(define (genera-triangulares n)
   (scons (triangular n) (λ () (genera-triangulares (+ n 1)))))

;; Definimos la sucesión de los naturales desde 0
(define naturales (genera-naturales 0))
;; Definimos la sucesión de los factoriales desde 0
(define factoriales (genera-factoriales 0))
;; Definimos la sucesión de los fibonaccies desde 0
(define fibonaccies (genera-fibonaccies 0))
;; Definimos la sucesión de los triangulares desde 1
(define triangulares (genera-triangulares 1))

;; Muestra los primeros 29 naturales:
(stake naturales 29)
;; Muestra los primeros 14 factoriales:
(stake factoriales 14)
;; Muestra los primeros 29 fibonaccis:
(stake fibonaccies 21)
;; Muestra los primeros 100 trianguales:
(stake triangulares 100)
