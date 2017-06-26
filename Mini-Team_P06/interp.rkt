#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Análisis semántico del lenguaje. El analizador semántico se encarga de dar
;; un significado al árbol de sintaxis abstracta.

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por
;; el parser. El intérprete requiere cargar un ambiente en esta versión.
;; interp: RCFBAEL Env -> RCFBAEL-Value
(define (interp exp ds)
    (match exp
     [(num n) (numV n)]
     [(bool b) (boolV b)]
     [(id x) (lookup x ds)]
     [(Mempty) (emptyV)]
     [(Mcons h t) (listV (interp h ds) (interp t ds))]
     [(iF c t e) (if (valor (interp c ds)) (interp t ds) (interp e ds))] 
     [(op f params) (cond
                      [(or (equal? f +)
                           (equal? f *)
                           (equal? f -)
                           (equal? f /)
                           (equal? f modulo)
                           (equal? f min)
                           (equal? f max)
                           (equal? f expt)) (numV (aplica f '{} (interp-l params ds)))]
                      
                      [(or (equal? f mand)
                           (equal? f mor)
                           (equal? f not)
                           (equal? f <)
                           (equal? f >)
                           (equal? f >=)
                           (equal? f <=)
                           (equal? f =)
                           (equal? f nequal?)
                           (equal? f zero?))  (boolV (aplica f '{} (interp-l params ds)))]

                      [(or (equal? f car)
                           (equal? f cdr)
                           (equal? f list?)
                           (equal? f empty?)) (aplical f (interp-l params ds))]
                      
                      [else (error "funcion no definida")])]
     [(fun params fun-body) (closureV params fun-body ds)]
     [(rec funs-rec body) (interp-rec funs-rec body ds)]
     [(app fun-expr params) (match fun-expr
                              [(id x) (interp-rec1 (lookup x ds) (interp-l params ds) ds)]
                              [else (interpapp-l (saca-params fun-expr) (saca-body fun-expr) params ds)])]))

;funcion que mete al ambientetodas las funciones recursivas y despues interpreta el cuerpo
;interp-rec :: (rec funs body) -> RCFBAEL-Value
(define (interp-rec funs body ds)
  (if (empty? funs)
      (interp body ds)
      (interp-rec (cdr funs) body (crea-ambiente (car funs) ds))))

;Funcion auxiliar para interpretar las funciones recursivas
;interp-rec1 :: box -> listof RCFBAEL-Value -> env -> RCFBAEL-Value
(define (interp-rec1 caja params ds)
  (let ([fun (unbox caja)])
    (interp-rec2 (saca-params fun) (saca-body fun) params ds)))

;Funcion auxiliar que mete las variables con sus valores al ambiente
;interp-rec2 :: listof symbol -> RCFBAEL -> listof RCFBAEL-Value -> DefrdSub -> RCFBAEL-Value
(define (interp-rec2 param fun arg ds)
  (if (or (empty? param) (empty? arg))
      (interp fun ds)
      (interp-rec2 (cdr param) fun (cdr arg) (aSub (car param) (car arg) ds))))
  
;Funciones que crea un ambiente recursivo
; crea-ambiente RCFBAEL-> DefrdSub -> DefrdSub
(define (crea-ambiente expr ds)
  [match expr
    [(binding simbolo expr1) (let [(v (box 1729))]
                                (begin
                                  (set-box! v (interp expr1 ds))
                                  (aRecSub simbolo v ds)))]])

;Funcion que nos regresa a lo que se evalua la condicíon de un if
;valor (boolV) -> boolean
(define (valor c)
  (match c
    [(boolV p) p]))
;Funcion que aplica las operaciones con lista suponemos que todas las operaciones con listas tienen solo un argumneto
;aplical :: procedure -> list (con un solo elemento) -> RCFBAEL-Value
(define (aplical f l)
  (match (car l)
    [(listV h t) (cond
                   [(equal? f car) h]
                   [(equal? f cdr) t]
                   [(equal? f list?) (boolV #t)]
                   [(equal? f empty?) (boolV #f)])]
    [(emptyV) (cond
                   [(equal? f car) (error "lista vacia")]
                   [(equal? f cdr) (error "lista vacia")]
                   [(equal? f list?) (boolV #t)]
                   [(equal? f empty?) (boolV #t)])]))
    
;Función que busca el valor de nu simbolo en un ambiente
;lookup :: symbol ->  DefrdSub -> FBAE-value
(define (lookup simbolo ambiente)
  (match ambiente
    [(mtSub) (error "variable no encontrada en el ambinete")]
    [(aSub nombre valor ambiente-anterior) (if (equal? simbolo nombre) valor (lookup simbolo ambiente-anterior))]
    [(aRecSub nombre caja ambiente-anterior) (if (equal? simbolo nombre) caja (lookup simbolo ambiente-anterior))]))

;Función que saca el cuerpo de una funcion
;saca-body :: (fun params body) -> body
(define (saca-body f)
  (match f
    [(fun params body) body]
    [(closureV p c env) c]))

;Función que saca los parametros formales de una funcion
;saca-body :: (fun params body) -> params
(define (saca-params f)
  (match f
    [(fun params body) params]
    [(closureV p c env) p]))

;Funcion auxiliar que mete todas las variales con su valor al ambiente  de una aplicacion de funcion
;interpapp-l listof symbol -> RCFBAEL -> listof RCFBAEL-Value -> DefrdSub -> RCFBAEL-Value
(define (interpapp-l param-form cuerpo params ds)
  ; si alguna de las listas (parametros formales o parametros reales) esta vacia dejamos de meter variables con sus respectvos valores al ambiente
  ; e interpretamos el cuerpo de esa funcion con el ambiente resultante
  (if (or (empty? param-form) (empty? params))                               
      (interp cuerpo ds)
      ; metemos al ambiente el simbolo con su valor y mandamos llamar recursivamente con la cola de los parametros formales y la cola de los
      ; parametros reales
      (interpapp-l (cdr param-form) cuerpo (cdr params) (aSub (car param-form) (interp (car params) ds) ds))))  
        
; Funcion auxiliar que regresa la interpretacion de cada uno de los elementos de una lista
; interp-l listof RCFBAEL -> listof RCFBAEL-Value
(define (interp-l l ds)
  (if (empty? l)
      '{}
      (cons (interp (car l) ds) (interp-l (cdr l) ds))))
;Funcion que aplica el procedimiento f a los numeros en sintaxis concreta
; Estamos suponiendo que a puede ser boolean o number y siempre la lista es del mismo tipo nunca se combinan
;aplica :: proicedure -> (listof a) -> a 
(define (aplica f numeros params)
  (if (empty? params)
      (apply f numeros)
      (match (car params)
        [(numV n ) (aplica f (append numeros (list n)) (cdr params))]
        [(boolV b) (aplica f (append numeros (list b)) (cdr params))]
        [else (error "funcion no definida")])))


;; Función encargada de ejecutar el interprete para que el usuario interactúe
;; con el lenguaje. Para diferenciar el prompt de Racket del nuestro, usamos
;; "(λ)". Aprovechamos los aspectos imperativos del lenguaje.
(define (ejecuta)
   (begin
      (display "(λ) ")
      (define x (read))
      (if (equal? x '{exit})
         (display "")
         (begin
            (let ([result (interp (desugar (parse x)) (mtSub))])
               (cond
                  [(numV? result) (display (numV-n result))]
                  [(boolV? result)
                     (if (boolV-b result)
                        (display "true")
                        (display "false"))]
                  [else (display "#<function>")])
            (display "\n")
            (ejecuta))))))

;; Llamada a función encargada de iniciar la ejecución del interprete
(display "Bienvenido a RCFBWAEL v4.0.\n")
(ejecuta)
