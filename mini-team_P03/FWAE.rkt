#lang plai

;; Tipo de dato abstracto FWAE para representar el árbol de sintaxis abstracta.
;; El lenguaje FWAE reconoce expresiones numéricas, operaciones binarias,
;; asignaciones locales, identificadores, funciones anónimas (lambdas) y 
;; aplicación de funciones.
(define-type FWAE
   [num (n number?)]
   [binop (f procedure?) (l FWAE?) (r FWAE?)]
   [with (bindings (listof Biding?)) (body FWAE?)]
   [id (name symbol?)]
   [fun (params (listof symbol?)) (body FWAE?)]
   [app (fun-expr FWAE?) (arg-exprs (listof FWAE?))])

(define-type  Biding
  [binding (name symbol?) (exp FWAE?)])
