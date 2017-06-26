#lang plai

(require "FWAE.rkt")

;; Análisis sintáctico del lenguaje. En analizador sintáctico se encarga de
;; construir el árbol de sintaxis abstracta.

;; Función elige auxiliar.
;; Permite elegir el operador correspondiente en Racket para las operaciones
;; binarias.
;; elige: symbol -> procedure
(define (elige s)
   (case s
      [(+) +]
      [(-) -]
      [(*) *]
      [(/) /]
      [(%) modulo]
      [(max) max]
      [(min) min]
      [(pow) expt]))

;; Analizador sintáctico para FWAE.
;; Dada una expresión en sintaxis concreta, regresa el árbol de sintaxis
;; abstractca correspondiente. Es decir, construye expresiones del tipo de
;; dato abstracto definido anteriormente.
;; parse: symbol -> FWAE
(define (parse sexp)
   (cond
      [(symbol? sexp) (id sexp)] ; para identificadores
      [(number? sexp) (num sexp)] ; para números
      [(list? sexp)
         (case (car sexp)
            [(+ - * - / % max min pow) ; para operaciones binarias
               (binop
                  (elige (car sexp))
                  (parse (cadr sexp))
                  (parse (caddr sexp)))]
            [(with) ; para asignaciones locales 
               (with
                  (bin (car (cdr sexp)))
                  (parse (car (cdr (cdr sexp)))))]
            [(fun) ; para lambdas MODIFICAR ESTE CASO
               (fun
                  (param  (car (cdr sexp)))
                  (parse (caddr sexp)))]
            [else ; para aplicación de funciones
               (app
                  (parse (car sexp))
                  (param1 (cdr sexp)))])]))

;bin : list -> list
; funcion auxiliar que regresa el parse de los parametros del with
(define (bin l)
  (if (empty? l)
      '{}
      (let ([l1 (car l)])
        (if (symbol? (car l1))
            (append (list (binding (car l1) (parse (car (cdr l1))))) (bin (cdr l)))
            (error "funcion no definida")))))
;funcion auxiliar que regresa el parse de los parametros de la funcion
;bin : list -> list
(define (param l)
  (cond
    [(empty? l) '{}]
    [(symbol? (car l)) (cons (car l) (param (cdr l)))]
    [else (error "funcion no definida")]))

;funcion auxiliar que regresa el parse de los argumentos de la aplicacion de funcion
;bin : list -> list 
(define (param1 l)
  (if (empty? l)
     '{}
    (cons (parse (car l)) (param1 (cdr l)))))
