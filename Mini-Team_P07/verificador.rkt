  #lang plai
(require "grammars.rkt")
(require "parser.rkt")
;Funcion que nos regresa el tipo de un arbol sintactico 
(define (type-of exp)
  (match exp
    [(numS n) (tnumber)]
    [(boolS b) (tboolean)]
    [(opS f arg) (cond
                    [(or
                      (equal? f +)
                      (equal? f *)
                      (equal? f -)
                      (equal? f /)
                      (equal? f modulo)
                      (equal? f min)
                      (equal? f max)
                      (equal? f expt)) (verifica-ln f arg)]

                    [(or 
                      (equal? f <)
                      (equal? f >)
                      (equal? f >=)
                      (equal? f <=)
                      (equal? f =)
                      (equal? f nequal?)) (verifica-lb2 f arg)]

                    [(or
                      (equal? f car)
                      (equal? f cdr)) (type-of (car arg))]
                    
                    [(or
                      (equal? f mand)
                      (equal? f mor)
                      (equal? f not)) (verifica-lb f arg)]

                    [(or 
                      (equal? f zero?)
                      (equal? f list?)
                      (equal? f empty?)
                      (equal? f number?)) (tboolean)])]
    [(ifS c t e) (verifica-if c t e)]
    [(MemptyS) (tnumber)]
    [(MconsS h t) (verifica-ll t (type-of h))]
    [(funS param tipo body) (verifica-f param tipo)]
    [(recS params body) (verifica-rec params body)]
    [(appS fun param) (match (type-of fun)
                        [(tarrow a b) (verifica-app param (tarrow a b))]
                        [else (format "Error : la primera exprecion de la aplicacion de funcion debe de ser te tipo a -> b")])]))

; Funcion auxiliar que nos regresa el tipo para recursion
(define (verifica-rec params body)
  (match body
    [(appS i p) (verifica-app p (buscat i params))]))

(define (buscat i params)
  (match i
    [(idS x) (buscat_aux x params)]))

(define (buscat_aux x params)
  (if (empty? params)
    (format " Error variable no entcontrada")
    (match (car params)
      [(bindingS a t e) (if (equal? a x) t (buscat_aux x (cdr params)))])))


; Funcion auxiliar que nos regresa el tipo para operadores con numeros
(define (verifica-ln f l )
  (if (empty? l)
      (tnumber)
      (match (car l)
        [(tnumber) (verifica-ln f (cdr l))]
        [(numS n) (verifica-ln f (cdr l))]
        [(opS x t) (if (equal? x cdr)
                       (format "Error: en ~a se esperaban argumentos de tipo number." (elige f))
                       (match (type-of (opS x t))
                         [(tnumber) (verifica-ln f (cdr l))]
                         [(tlistof number) (verifica-ln f (cdr l))]
                         [else (format "Error: en ~a se esperaban argumentos de tipo number." (elige f))]))]
        [else (format "Error: en ~a se esperaban argumentos de tipo number." (elige f))])))

; Funcion auxiliar que nos regresa el tipo para aplicacion de funcion
(define (verifica-app param t)
  (if (empty? param)
    t
    (match t
      [(tarrow a b) (if (equal? a (type-of (car param))) (verifica-app (cdr param) b) (format "Error se esperaba un argumento de tipo ~a y se recivio uno de tipo ~s" (eligeTipo b) (eligeTipo (type-of (car param)))))]
      [else ("Error : se esperaba el mismo numero de parametros que de argumentos")])))

; Funcion auxiliar que nos regresa el tipo para funciones
(define (verifica-f p t)
  (if (empty? p)
    t
    (match (car p)
      [(bindingfS a t1) (tarrow t1 (verifica-f (cdr p) t))])))

; Funcion auxiliar que nos regresa el tipo para if
(define (verifica-if condicion then else)
  (if (equal? (type-of condicion) (tboolean))
    (let ([t1 (type-of then)])
      (if (equal? t1 (type-of else))
        t1
        (format "Error : se esperaba que ambas funciones furan del tipo  ~a." (eligeTipo (t1)))))
    (format "Error : en el if se esperaba que la condicion fuera de tipo boolean")))

; Funcion auxiliar que nos regresa el tipo para listas
(define (verifica-ll t tipo)
  (match t
    [(MemptyS) (tlistof tipo)]
    [(MconsS h tail) (if (equal? (type-of h) tipo) (verifica-ll tail tipo) (format "Error: en la lista se esperaban argumentos de tipo ~a." (eligeTipo tipo)))]
    [else (format "error lista mal construida")]))

; Funcion auxiliar que nos regresa el tipo para operadores boleanos con numeros (< = <= etc)
(define (verifica-lb2 f l)
  (if (empty? l)
      (tboolean)
      (match (car l)
        [(tnumber) (verifica-lb2 f (cdr l))]
        [(numS n) (verifica-lb2 f (cdr l))]
        [(opS x t) (if (equal? x cdr)
                       (format "Error: en ~a se esperaban argumentos de tipo number." (elige f))
                       (match (type-of (opS x t))
                         [(tnumber) (verifica-lb2 f (cdr l))]
                         [(tlistof number) (verifica-lb2 (cdr l))]
                         [else (format "Error: en ~a se esperaban argumentos de tipo number." (elige f))]))]
        [else (format "Error: en ~a se esperaban argumentos de tipo number." (elige f))])))

; Funcion paraa desplegar los tipos que conocemos a partir de los tipos defindos en la gramatica
(define (eligeTipo t)
  (match t
    [(tnumber) 'number]
    [(tboolean) 'boolean]
    [(tlistof a) (format "lista de ~a" (eligeTipo a))]
    [(tarrow a b) (format "~a -> ~s" (eligeTipo a) (eligeTipo b))]))

; funcion auxiliar que nos dice que operacion estamos realizando para poder escribirla
(define (elige f)
  (cond
    [(equal? f +) '+]
    [(equal? f *) '*]
    [(equal? f -) '-]
    [(equal? f /) '/]
    [(equal? f modulo) 'modulo]
    [(equal? f min) 'min]
    [(equal? f max) 'max]
    [(equal? f expt) 'expt]
    [(equal? f <) '<]
    [(equal? f >) '>]
    [(equal? f >=) '>=]
    [(equal? f <=) '<=]
    [(equal? f =) '=]
    [(equal? f nequal?) '!=]
    [(equal? f zero?) 'zero?]
    [(equal? f mand) 'and]
    [(equal? f mor) 'or]
    [(equal? f not) 'not]
    [(equal? f number?) 'number?]))

; Funcion auxiliar que nos regresa el tipo para operadores booleanos
(define (verifica-lb f l)
  (if (empty? l)
      (tboolean)
      (match (car l)
        [(tboolean) (verifica-lb f (cdr l))]
        [(boolS b) (verifica-lb f (cdr l))]
        [(opS x s) (if (equal? x cdr)
                       (format "Error: en ~a se esperaban argumentos de tipo boolean." (elige f))
                       (match (type-of (opS x s))
                         [(tboolean) (verifica-lb f (cdr l))]
                         [(tlistof boolean) (verifica-lb (cdr l))]
                         [else (format "Error: en ~a se esperaban argumentos de tipo boolean." (elige f))]))]
        [else (format "Error: en ~a se esperaban argumentos de tipo boolean." (elige f))])))


