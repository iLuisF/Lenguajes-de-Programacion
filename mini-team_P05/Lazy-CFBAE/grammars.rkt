#lang plai

;; Tipo de dato abstracto CFWBAE/L para representar el árbol de sintaxis 
;; abstracta. El lenguaje CFWBAE/L reconoce expresiones numéricas, expresiones 
;; booleanas, operaciones n-arias, asignaciones locales, identificadores, 
;; funciones anónimas (lambdas), aplicación de funciones y el condicional if.
(define-type CFWBAE/L
   [numS (n number?)]
   [boolS (b boolean?)]
   [idS (name symbol?)]
   [opS (f procedure?) (args (listof CFWBAE/L?))]
   [ifS (expr CFWBAE/L?) (then-expr CFWBAE/L?) (else-expr CFWBAE/L?)]
   [withS (bindings (listof binding?)) (body CFWBAE/L?)]
   [funS (params (listof symbol?)) (body CFWBAE/L?)]
   [appS (fun-expr CFWBAE/L?) (args (listof CFWBAE/L?))])

;; Tipo de dato abstracto para representar a los identificadores con su valor.
(define-type Binding
   [binding (name symbol?) (value CFWBAE/L?)])

;; Tipo de dato abstracto CFBAE/L para representar el árbol de sintaxis 
;; abstracta. El lenguaje CFBAE/L reconoce expresiones numéricas, expresiones 
;; booleanas, operaciones n-arias, identificadores, funciones anónimas (lambdas) 
;; y aplicación de funciones. Es la versión sin azúcar sintáctica de CFWBAE/L.
(define-type CFBAE/L
   [num (n number?)]
   [bool (b boolean?)]
   [id (name symbol?)]
   [op (f procedure?) (args (listof CFBAE/L?))]
   [iF (expr CFBAE/L?) (then-expr CFBAE/L?) (else-expr CFBAE/L?)]
   [fun (params (listof symbol?)) (body CFBAE/L?)]
   [app (fun-expr CFBAE/L?) (args (listof CFBAE/L?))])

;; Tipo de dato abstracto para representar el ambiente.
(define-type Env
   [mtSub]
   [aSub (name symbol?) (value CFBAE/L-Value?) (ds Env?)])

;; Tipo de dato abstracto para representar los resultados devueltos por el 
;; intérprete. El intérprete únicamente devuelve números y funciones. Se usa el
;; valor exprV para postergar la evaluación y lograr evaluación perezosa.
(define-type CFBAE/L-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [closureV (params (listof symbol?)) (body CFBAE/L?) (ds Env?)]
   [exprV (expr CFBAE/L?) (env Env?)])
