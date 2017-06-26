#lang plai

(require "FWAE.rkt")
(require "parser.rkt")

;; Análisis semántico del lenguaje. El analizador semántico se encarga de dar
;; un significado al árbol de sintaxis abstracta.


;; Función que implementa el algoritmo de sustitución.
;; subst: FWAE symbol FWAE -> FWAE
(define (subst expr sub-id val)
   (match expr
      #| Para números simplemente devolvemos su valor pues no hay nada que
         sustituir. |#
      [(num n) expr]
      #| Para operaciones binarias regresamos una nueva operación binaria con
         la sustitución del lado izquierdo y el lado derecho. |#
      [(binop f l r) (binop f (subst l sub-id val) (subst r sub-id val))]
      #| Para with tenemos que considerar dos casos |#
      [(with bound-ids bound-body)
        (subst1 '() bound-ids bound-body sub-id val)]
     
      #| Para fun tenemos que considerar dos casos. |#
      [(fun bound-ids bound-body)
       (if (esta-simbolo bound-ids sub-id)
            #| Si el parámetro de la función es igual al del valor a 
               sustituir no hacemos nada para evitar modificar el 
               alcance. |#
           (fun bound-ids bound-body)
            #| Si el parámetro de la función es distinto al del valor a
               sustituir sólo sustituimos en el cuerpo. |#
           (fun bound-ids (subst bound-body sub-id val)))]
      #| Para la aplicación de funciones simplemente sustituimos 
         recursivamente el valor en la funció y los argumentos. |#
      [(app fun-expr arg-exprs)
         (app 
            (subst fun-expr sub-id val)
            (subst3 arg-exprs sub-id val))]
      #| Para identificadores, sustituimos la expresión si es igual al valor
         a sustituir, en caso contrario, no hacemos nada. |#
      [(id v)
         (if (symbol=? v sub-id)
            val
            expr)]))
;funcion auxiliar que regresa que regresa la substitucion si aparece en los parametros del with el sub-id
;subst1 : list -> list -> FWAE -> symbol -> FWAE -> FWAE
(define (subst1  l1 l expr sub-id val)
  (cond
    [(empty? l) (subst2 '() l1 expr sub-id val)]
    [else (match (car l)
      [(binding n expr1)
       #| Si el identificador de la expresión with es igual al del 
          valor a sustituir, simplemente sustituimos en la expresión
          asociada al identificador, pues el alcance del cuerpo del 
          with hace referencia al identificador y no podemos cambiar 
          dicho valor. |#
        (if (symbol=? n sub-id)
           (with (append l1 (list (binding n (subst expr1 sub-id val))) (cdr l)) expr)
           ;si no esta verificamos en los parametros siguietnes 
           (subst1 (append l1 (list (car l))) (cdr l) expr sub-id val))])]))

#|funcion auxiliar que regresa Si el identificador de la expresión with es distinto al de alguno de sus valores a
 sustituir, sustituimos la expresión asociada al indentificador y al cuerpo, pues esto no afecta el alcance. |#
;subst2 : list -> list -> FWAE -> symbol -> FWAE -> FWAE
(define (subst2 l1 l expr sub-id val)
  (if (empty? l )
      (with l1 (subst expr sub-id val))
      (match (car l)
        [(binding n expr1)
         (subst2 (append l1 (list (binding n (subst expr1 sub-id val)))) (cdr l) expr sub-id val)])))
;funcon auxiliar que regresa verdadero si un simbolo esta en una lsita
;esta-simbolo list -> symbol -> boolean
(define (esta-simbolo l sim)
  (cond
    [(empty? l) #f]
    [(symbol=? (car l) sim) #t]
    [else (esta-simbolo (cdr l ) sim)]))

;funcion auxiliar que subtituye en todos los parametros de la aplicacion de funcion el sub-id y el valor asociado a ese idenificador
;subst3 list -> symbol -> FWAE -> FWAE
(define (subst3 l sub-id val)
  (if (empty? l)
      '{}
      (cons (subst (car l) sub-id val) (subst3 (cdr l) sub-id val))))
  
       
;; Función encargada de interpretar el árbol de sintaxis abstracta generador por
;; el parser.
;; interp: AE -> any
(define (interp exp)
   (match exp
      #| Un número se evalúa a un número |#
      [(num n) n]
      #| Aplicamos la función f a la interpretación del lado izquierdo y 
         derecho. |#
      [(binop f l r) (f (interp l) (interp r))]
      #| Interpretamos el cuerpo con la sustitución del identificador 
         correspondiente. |#
      [(with bound-ids bound-body)
       (interp-with bound-ids bound-body)]
      #| En esta versión del intérprete, no regresamos ningún resultado al
         interpretar una función. Una función es una función y punto. |#
    
      [(fun bound-id bound-body) "#<function>"]
      #| Para aplicar la función debemos sustituir el parámetro de la función
         por por el argumento en el cuerpo de la función y luego 
         interpretarla. |#
      
      [(app fun-expr arg-exprs)
       (interp-app fun-expr arg-exprs)]
      #| El identificador no tiene valor asociado, por lo que decimos ques es
         una variable libre. |#
      [(id v) "free identifier"]))

; regresa el cuerpo de una funcion
; fun-body : FWAE -> FWAE
(define (fun-body f)
  (match f
    [(fun params body) body]
    [else f]))

; regresa los parametros de una funcion
; fun-body : FWAE -> listof symbol
(define (fun-params f)
  (match f
    [(fun params body) params]
    [else '{}]))
; regresa el interp de una aplicacion de funcion toma el primera parametro y sustituye su valor
; despues se va coin el que sigue hasta terminar con la lista de parametros
; estamos suponiendo que los parametos que recibe la funcion y que los argumentos que toma la aplicacion son de la misma longitud
; interp-app FWAE -> listof FWAE -> FWAE
(define (interp-app fun-expr arg-exprs)
  (let [(params (fun-params fun-expr))]
    (if (empty? params)
        (interp (fun-body fun-expr))
        (interp-app (fun (cdr params) (subst (fun-body fun-expr) (car params) (num (interp (car arg-exprs))))) (cdr arg-exprs))))) 
  
; regresa el inerp del with metemos todos los parametros con su valor en los demas parametros por si algunos estan escritos en forma de otros parametros
; y tambien en la expresion del with eso para cada parametro en la lista de parametros
; interp-wiht : listof binding -> FWAE -> FWAE
(define (interp-with params expr)
  (if (empty? params)
      (interp expr)
      (let [(l (car params))]
        (match l
            [(binding sim expr1)
             (match expr1
               [(fun bound-ids bound-body)
                (interp-with (subst-params (cdr params) sim (fun bound-ids bound-body)) (subst expr sim (fun bound-ids bound-body)))]
               [else
                (let [(expr2 (num (interp expr1)))]
               (interp-with (subst-params (cdr params) sim expr2) (subst expr sim expr2)))])]))))

; funcion que sustituye en una lista de bindigs un simbolo y una expresion
; subst-params listof binding -> symbol -> listof binding
(define (subst-params l  sim expr)
  (if (empty? l)
      l
     (match (car l)
       [(binding sim1 expr1)
        (cons (binding sim1 (subst expr1 sim expr)) (subst-params (cdr l) sim expr))])))
      

;; Función encargada de ejecutar el interprete para que el usuario interactúe
;; con el lenguaje. Para diferenciar el prompt de Racket del nuestro, usamos
;; "(λ)". Aprovechamos los aspectos imperativos del lenguaje.
(define (ejecuta)
   (begin
      (display "(λ) ")  ; imprimimos el prompt
      (define x (read)) ; pedimos la expresión
      (if (equal? x '{exit}) ; si nos piden salir del intérprete
         (display "") ; no hacemos nada
         (begin ; en otro caso realizamos la interpretación
            (display (interp (parse x))) ; interpretamos el árbol de sintaxis abstracta
            (display "\n") ; da un salto de línea
            (ejecuta))))) ; repite el procesos hasta que se lea {exit}.

;; Llamada a función encargada de iniciar la ejecución del interprete
(display "Bienvenido a Mini_Team FWAE v1.0.\n") ; bienvenida al usuario
(ejecuta) ; llama a ejecución al intérprete