#lang plai

(require "grammars.rkt")

;; Análisis sintáctico del lenguaje. En analizador sintáctico se encarga de
;; construir el árbol de sintaxis abstracta.

;; Analizador sintáctico para FWBAE.
;; Dada una expresión en sintaxis concreta, regresa el árbol de sintaxis
;; abstractca correspondiente. Es decir, construye expresiones del tipo de
;; dato abstracto definido en el archivo grammars.rkt
;; parse: symbol -> FWBAE
(define (parse sexp)
   (cond
     [(number? sexp) (numS sexp)]
     [(boolean? sexp) (boolS sexp)]
     [(equal? sexp 'false) (boolS #f)]
     [(equal? sexp 'true) (boolS #t)]
     [(symbol? sexp) (idS sexp)]
     [(list? sexp)
      (case (car sexp)
        [(+ - / * % min max pow and or  not < > <= >= = !=)
         (opS (elige (car sexp)) (parse-list (cdr sexp)))]
         [(with) ; para asignaciones locales 
           (withS
           (bin (car (cdr sexp)))
           (parse (car (cdr (cdr sexp)))))]
        [(fun) ; para lambdas
         (funS
           (param  (car (cdr sexp)))
           (parse (caddr sexp)))]
        [else ; para aplicación de funciones
               (appS
                  (parse (car sexp))
                  (param1 (cdr sexp)))])]))

;funcion auxiliar que regresa el parse de los argumentos de la aplicacion de funcion
;bin : list -> list 
(define (param1 l)
  (if (empty? l)
     '{}
    (cons (parse (car l)) (param1 (cdr l)))))

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
        
      
     
;funcion que dependiendo su simbolo ne sintaxis abstracte regresa su procedimiento en racket en sitaxis concreta
;elige :: symbol -> procedure
(define (elige s)
   (case s
      [(+) +]
      [(-) -]
      [(*) *]
      [(/) /]
      [(%) modulo]
      [(max) max]
      [(min) min]
      [(pow) expt]
      [(and) mand]
      [(or) mor]
      [(not) not]
      [(<) <]
      [(>) >]
      [(<=) <=]
      [(>=) >=]
      [(=) =]
      [(!=) nequal?]))

;funcion que haca el arbol de sintasis abstracata para cada elemento en una lista
; parse-list :: listof symbol -> listof FWBAE
(define (parse-list l)
  (if (empty? l)
      '{}
      (cons (parse (car l)) (parse-list (cdr l)))))
;procedimiento que redefinio la rutina de and (hace lo mismo que and)
(define (mand . args)
   (land args))

; aplica el and a una lista 
(define (land list)
   (match list
      ['() #t]
      [(cons x xs) (and x (land xs))]))
;procedimiento que redefinio la rutina de or (hace lo mismo que or)
(define (mor . args)
   (lor args))

;hace un or a todos los elementos de una lista
(define (lor list)
   (match list
      ['() #f]
      [(cons x xs) (or x (lor xs))]))
;procedimiento que redefine a la rutina de != (hace lo mismo que !=)
(define (nequal? . args)
   (lnequal? args))

; no dice si la cabeza es diferente a todos los elementos en la cola de la lista
(define (lnequal? list)
   (match list
      ['() #t]
      [(cons x xs)
         (cond
            [(empty? xs) #t]
            [(equal? x (car xs)) #f]
            [else (lnequal? xs)])]))
  
;; Función que toma una expresión en FWBAE y le quita la azúcar sintáctica.
;; desugar: FWBAE -> FBAE
(define (desugar sexp)
   (match sexp
     [(numS n) (num n)]
     [(boolS b) (bool b)]
     [(idS x) (id x)]
     [(opS proc args) (op proc (desugar-l args))]
     [(funS params body) (fun params (desugar body))]
     [(appS expr param) (app (desugar expr) (desugar-l param))]
     [(withS bindings body) (app (fun (saca-param bindings) (desugar body)) (saca-valores bindings))]))

; Función que le quita el azucar sintactica a cada elemento en una lista
; desugar-l :: listof FWBAE -> listof FBAE
(define (desugar-l l)
  (if (empty? l)
      '{}
      (cons (desugar (car l)) (desugar-l (cdr l)))))

;Funcion auxiliar que de un with saca solo los id's de sus parametros en orden (el primer parametro que aparece en
; la lista de parametros del with es el primer simbolo de la lista resultante)
;saca-param :: (withS bindings body) -> lisof symbol
(define (saca-param l)
 (if (empty? l)
     '{}
     (match (car l)
       [(binding simbolo expr) (cons simbolo (saca-param (cdr l)))])))

;Funcion auxiliar que de un with saca solo los valores de sus parametros  en orden (el primer valor del parametro que aparece en
; la lista de parametros del with es el primer valor de la lista resultante)
;saca-param :: (withS bindings body) -> lisof symbol
(define (saca-valores l)
  (if (empty? l)
      '{}
      (match (car l)
        [(binding simbolo expr) (cons (desugar expr) (saca-valores (cdr l)))])))
