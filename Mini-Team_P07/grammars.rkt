#lang plai

;; Tipo de dato abstracto RCFBWAEL para representar el árbol de sintaxis 
;; abstracta. El lenguaje RCFBWAEL reconoce expresiones numéricas, expresiones 
;; booleanas, operaciones n-arias, asignaciones locales, identificadores, 
;; funciones anónimas (lambdas), aplicación de funciones, el condicional if,
;; asignaciones locales recursivas y listas.
(define-type RCFBWAEL
   [idS (name symbol?)]
   [numS (n number?)]
   [boolS (b boolean?)]
   [MemptyS]
   [MconsS (head RCFBWAEL?) (tail RCFBWAEL?)]
   [opS (f procedure?) (args (listof RCFBWAEL?))]
   [ifS (expr RCFBWAEL?) (then-expr RCFBWAEL?) (else-expr RCFBWAEL?)]
   [withS (bindings (listof bindingS?)) (body RCFBWAEL?)]
   [recS (bindings (listof bindingS?)) (body RCFBWAEL?)]
   [funS (params (listof bindingfS?)) (tipo Tipo?) (body RCFBWAEL?)]
   [appS (fun-expr RCFBWAEL?) (args (listof RCFBWAEL?))])

;; Tipo de dato abstracto para representar a los identificadores con su valor.
(define-type BindingS
   [bindingS (name symbol?) (tipo Tipo?) (value RCFBWAEL?)])
; Tipo de dato para las funciones para guardar los tipos de cada simbolo
(define-type BindingfS
   [bindingfS (name symbol?) (tipo Tipo?)])

;; Tipo de dato abstracto RCFBAEL para representar el árbol de sintaxis 
;; abstracta. El lenguaje RCFBAEL reconoce expresiones numéricas, expresiones 
;; booleanas, operaciones n-arias, identificadores, funciones anónimas 
;; (lambdas), aplicación de funcioneS, condicionales, asignaciones locales 
;; recursivas y listas. Es la versión sin azúcar sintáctica de LCFWBAE.
(define-type RCFBAEL
   [id (name symbol?)]
   [num (n number?)]
   [bool (b boolean?)]  
   [Mempty]
   [Mcons (head RCFBAEL?) (tail RCFBAEL?)]
   [op (f procedure?) (args (listof RCFBAEL?))]
   [iF (expr RCFBAEL?) (then-expr RCFBAEL?) (else-expr RCFBAEL?)]
   [rec (bindings (listof binding?)) (body RCFBAEL?)]
   [fun (params (listof symbol?)) (body RCFBAEL?)]
   [app (fun-expr RCFBAEL?) (args (listof RCFBAEL?))])

;; Tipo de dato para representar a los identificadores con su valor. Versión sin
;; azúcar sintáctica.
(define-type Binding
   [binding (name symbol?) (value RCFBAEL?)])

;; Tipo de dato abstracto para representar el ambiente.
(define-type Env
   [mtSub]
   [aSub (name symbol?) (value RCFBAEL-Value?) (ds Env?)]
   [aRecSub (name symbol?) (value boxed-RCFBAEL-Value?) (env Env?)])

;; Indica si la caja almacena un dato de tipo RCFBAEL-Value.
;; boxed-RCFBAEL-Value?: box -> boolean
(define (boxed-RCFBAEL-Value? v)
   (and (box? v) (RCFBAEL-Value? (unbox v))))

;; Tipo de dato abstracto para representar los resultados devueltos por el 
;; intérprete. El intérprete devuelve números, booleanos, listas y funciones.
(define-type RCFBAEL-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [emptyV]
   [listV (head RCFBAEL-Value?) (tail RCFBAEL-Value?)]
   [closureV (params (listof symbol?)) (body RCFBAEL?) (ds Env?)])

; Tipo de dato abstracto para representar los tipos
(define-type Tipo
   [tnumber]
   [tboolean]
   [tlistof (t Tipo?)]
   [tarrow (a Tipo?) (b Tipo?)])