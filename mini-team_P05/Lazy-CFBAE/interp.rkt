#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Análisis semántico del lenguaje. El analizador semántico se encarga de dar
;; un significado al árbol de sintaxis abstracta.

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por
;; el parser. El intérprete requiere cargar un ambiente en esta versión.
;; interp: CFBAE/L Env -> CFBAE/L-Value
(define (interp exp ds)
   (match exp
     [(num n) (numV n)]
     [(bool b) (boolV b)]
     [(id x) (lookup x ds)]
     [(iF cond then else) (if (strict (exprV cond ds)) (interp then ds) (interp else ds))] 
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
                           (equal? f nequal?))  (boolV (aplica f '{} (interp-l params ds)))]
                      
                      [else (error "funcion no definida")])]
     [(fun params fun-body) (closureV params fun-body ds)] 
     [(app fun-expr params) (interpapp-l (saca-params fun-expr) (saca-body fun-expr) params ds)]))

;Función que fuerza la evaluacion en los puntos extrictos
;strict ::  (CFBAE/L U (exprV  CFBAE/L  Env)) -> CFBAE/L-Value
(define (strict e)
  (match e
    [(exprV expr env) (strict (interp expr env))]
    [else e]))

; Funcion auxiliar que regresa la interpretacion de cada uno de los elementos de una lista
; interp-l listof FBAE -> listof FBAE-value
(define (interp-l l ds)
  (if (empty? l)
      '{}
      (cons (strict (interp (car l) ds)) (interp-l (cdr l) ds))))

;Función que busca el valor de nu simbolo en un ambiente
;lookup :: symbol ->  DefrdSub -> FBAE-value
(define (lookup simbolo ambiente)
  (match ambiente
    [(mtSub) (error "variable no encontrada en el ambinete")]
    [(aSub nombre valor ambiente-anterior) (if (equal? simbolo nombre) valor (lookup simbolo ambiente-anterior))]))

;Función que saca el cuerpo de una funcion
;saca-body :: (fun params body) -> body
(define (saca-body f)
  (match f
    [(fun params body) body]))

;Función que saca los parametros formales de una funcion
;saca-body :: (fun params body) -> params
(define (saca-params f)
  (match f
    [(fun params body) params]))

;Funcion auxiliar que mete todas las variales con su valor al ambiente  de una aplicacion de funcion
;interpapp-l listof symbol -> FBAE -> listof FBAE-value -> DefrdSub -> FBAE-value
(define (interpapp-l param-form cuerpo params ds)
  ; si alguna de las listas (parametros formales o parametros reales) esta vacia dejamos de meter variables con sus respectvos valores al ambiente
  ; e interpretamos el cuerpo de esa funcion con el ambiente resultante
  (if (or (empty? param-form) (empty? params))                               
      (interp cuerpo ds)
      ; metemos al ambiente el simbolo con su valor (en forma de expración no evaluamos) y mandamos llamar recursivamente con la cola de los parametros formales y la cola de los
      ; parametros reales
      (interpapp-l (cdr param-form) cuerpo (cdr params) (aSub (car param-form) (exprV (car params) ds) ds))))  
        

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
(display "Bienvenido a CFWBAE/L v3.0.\n")
(ejecuta)
