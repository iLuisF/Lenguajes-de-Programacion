#lang plai
(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

;; Expresiones a probar
(define expr1  '{with {{x 10} {y 5}} 
	              {with {{x 1} {y 2}}
	                 {+ {/ y x} {* x y}}}})
(define expr2  '{with {{x 5} {y 10}} 
	              {* x {with {{y 5} {x 1}} {+ y x}}}})
(define expr3  '{with {{x 5} {y 10}} 
	              {{fun {x y} {/ x y}} 10 5}})
(define expr4  '{and true {= 50 {/ 100 2}}})
(define expr5  '{{fun {x y z} {+ {pow x 2}  {pow y z}}} 2 3 4})
(define expr6  '{with {{a 4} {b 5} {c 10}} 
	              {+ a {/ b c}}})
(define expr7  '{with {{y {- {+ 5 5} 3}} {x {pow 2 3}}} 
	               {* y x}})
(define expr8  '{{fun {p q} {and {or {not p} q} {or {not q} p}}} true true})
(define expr9  '{with {{x {+ 1 2}} {y 2} {z 5}}
	               {+ {pow x y} 5}})
(define expr10 '{{fun {x y} {+ x y}} {pow 2 4} {+ 2 {+ 8 {+ 5 6}}}})
(define expr11 '{with {{x {+ 2 3}} {y {+ 3 3}}}
	               {+ x y}})
(define expr12 '{with {{x 5}} 
	               {+ x x}})
(define expr13 '{with {{x 1}}
	               {with {{y 3}}
	                   {+ y {pow x y}}}})
(define expr14 '{{fun {x y} {with {{z 2}} {+ z {+ x y}}}} 2 3})
(define expr15 '{with {{x 5}}
	               {+ x {with {{y 3}} x}}})
(define expr16 '{with {{x 3}}
	               {fun {y} {+ x y}}})
(define expr17 '{with {{a 2} {b {{fun {x y} {pow x y}} 2 3}}}
	               {+ a b}})
(define expr18 '{and {> 5 6} false})
(define expr19 '{with {{x 5} {y 10}}
	               {min x y}})
(define expr20 '{{fun {x y} {pow x y}} 2 3})
(define expr21 '{{fun {x y z} {+ {pow x y} z}} 5 2 7})
(define expr22 '{{fun {x y} {< {with {{a x} {b y}} {+ a b}} {pow x y}}} 4 5})
(define expr23 '{+ 2 4 7})
(define expr24 '{with {{a 2} {b 3}}
	               {+ a b 4 5}})
(define expr25 '{and {or true false true} true true})

;; Pruebas unitarias
(test (interp (desugar (parse expr1 )) (mtSub)) (numV 4))
(test (interp (desugar (parse expr2 )) (mtSub)) (numV 30))
(test (interp (desugar (parse expr3 )) (mtSub)) (numV 2))
(test (interp (desugar (parse expr4 )) (mtSub)) (boolV #t))
(test (interp (desugar (parse expr5 )) (mtSub)) (numV 85))
(test (interp (desugar (parse expr6 )) (mtSub)) (numV 9/2))
(test (interp (desugar (parse expr7 )) (mtSub)) (numV 56))
(test (interp (desugar (parse expr8 )) (mtSub)) (boolV #t))
(test (interp (desugar (parse expr9 )) (mtSub)) (numV 14))
(test (interp (desugar (parse expr10)) (mtSub)) (numV 37))
(test (interp (desugar (parse expr11)) (mtSub)) (numV 11))
(test (interp (desugar (parse expr12)) (mtSub)) (numV 10))
(test (interp (desugar (parse expr13)) (mtSub)) (numV 4))
(test (interp (desugar (parse expr14)) (mtSub)) (numV 7))
(test (interp (desugar (parse expr15)) (mtSub)) (numV 10))
(test (interp (desugar (parse expr16)) (mtSub)) (closureV '(y) (op + (list (id 'x) (id 'y))) (aSub 'x (numV 3) (mtSub))))
(test (interp (desugar (parse expr17)) (mtSub)) (numV 10))
(test (interp (desugar (parse expr18)) (mtSub)) (boolV #f))
(test (interp (desugar (parse expr19)) (mtSub)) (numV 5))
(test (interp (desugar (parse expr20)) (mtSub)) (numV 8))
(test (interp (desugar (parse expr21)) (mtSub)) (numV 32))
(test (interp (desugar (parse expr22)) (mtSub)) (boolV true))
(test (interp (desugar (parse expr23)) (mtSub)) (numV 13))
(test (interp (desugar (parse expr24)) (mtSub)) (numV 14))
(test (interp (desugar (parse expr25)) (mtSub)) (boolV #t))
