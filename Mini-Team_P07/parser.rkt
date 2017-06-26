#lang plai

(require "grammars.rkt")

;; Analizador sintáctico para RCFBWAEL
;; Dada una expresión en sintaxis concreta, regresa el árbol de sintaxis
;; abstractca correspondiente. Es decir, construye expresiones del tipo de
;; dato abstracto definido en el archivo grammars.rkt
;; parse: symbol -> RCFBWAEL
(define (parse sexp)
  (cond
     [(number? sexp) (numS sexp)]
     [(boolean? sexp) (boolS sexp)]
     [(equal? sexp 'false) (boolS #f)]
     [(equal? sexp 'true) (boolS #t)]
     [(equal? sexp 'empty) (MemptyS)]
     [(symbol? sexp) (idS sexp)]
     [(list? sexp)
      (case (car sexp)
        [(+ - / * % min max pow and or  not < > <= >= = != head tail zero? list? empty?)
         (opS (elige (car sexp)) (parse-list (cdr sexp)))]
        [(with) ; para asignaciones locales 
           (withS
           (bin (car (cdr sexp)))
           (parse (car (cdr (cdr sexp)))))]
        [(cons) (MconsS (parse (cadr sexp)) (parse (caddr sexp)))]
        [(if) (ifS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
        [(fun) ; para lambdas
          (let ([c (cdr sexp)])
            (funS
              (param (car c))
              (tipo (caddr c))
              (parse (cadddr c))))]
        [(rec) (recS (bin (cadr sexp)) (parse (caddr sexp)))]
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
            (append (list (bindingS (car l1) (tipo (caddr l1)) (parse (cadddr l1)))) (bin (cdr l)))
            (error "funcion no definida")))))

; tipo list -> Tipo
; Funcion que abstrae el dato de la sintaxis 
(define (tipo l)
  (cond 
    [(equal? l 'number) (tnumber)]
    [(equal? l 'boolean) (tboolean)]
    [(list? l) (cond 
                  [(= (length l) 2) (tlistof (tipo (cadr l)))]
                  [(= (length l) 3) (tarrow (tipo (car l)) (tipo (caddr l)))]
                  [(error "mala especificación de tipos")])]
    [else (error "mala especificación de tipos")]))

;funcion auxiliar que regresa el parse de los parametros de la funcion
;bin : list -> listof bindingfS
(define (param l)
  (cond
    [(empty? l) '{}]
    [(equal? (length  (car l)) 3) (let ([elem (car l)])
                                    (cons (bindingfS (car elem) (tipo (caddr elem))) (param (cdr l))))]
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
      [(!=) nequal?]
      [(head) car]
      [(tail) cdr]
      [(empty?) empty?]
      [(list?) list?]
      [(zero?) zero?]
      [(number?) number?]))

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

