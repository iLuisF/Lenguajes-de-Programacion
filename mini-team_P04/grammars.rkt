#lang plai

;; Tipo de dato abstracto FWBAE para representar el árbol de sintaxis abstracta.
;; El lenguaje FWBAE reconoce expresiones numéricas, expresiones booleanas, 
;; operaciones n-arias, asignaciones locales, identificadores, funciones 
;; anónimas (lambdas) y aplicación de funciones.
(define-type FWBAE
   [numS (n number?)]
   [boolS (b boolean?)]
   [idS (name symbol?)]
   [opS (f procedure?) (args (listof FWBAE?))]
   [withS (bindings (listof binding?)) (body FWBAE?)]
   [funS (params (listof symbol?)) (body FWBAE?)]
   [appS (fun-expr FWBAE?) (args (listof FWBAE?))])

;; Tipo de dato abstracto para representar a los identificadores con su valor.
(define-type Binding
	[binding (name symbol?) (value FWBAE?)])

;; Tipo de dato abstracto FBAE para representar el árbol de sintaxis abstracta.
;; El lenguaje FBAE reconoce expresiones numéricas, expresiones booleanas, 
;; operaciones n-arias, identificadores, funciones anónimas (lambdas) y 
;; aplicación de funciones. Es la versión sin azúcar sintáctica de FWAE.
(define-type FBAE
	[num (n number?)]
	[bool (b boolean?)]
	[id (name symbol?)]
	[op (f procedure?) (args (listof FBAE?))]
	[fun (params (listof symbol?)) (body FBAE?)]
   	[app (fun-expr FBAE?) (args (listof FBAE?))])

;; Tipo de dato abstracto para representar el ambiente.
(define-type DefrdSub
	[mtSub]
	[aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;; Tipo de dato abstracto para representar los resultados devueltos por el 
;; intérprete. El intérprete únicamente devuelve números y funciones.
(define-type FAE-Value
	[numV (n number?)]
	[boolV (b boolean?)]
	[closureV (param (listof symbol?)) (body FBAE?) (ds DefrdSub?)])
