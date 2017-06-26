#lang plai

(require "interp.rkt")
(require "parser.rkt")

;; interp: AE -> any
(test (interp (parse 1)) 1)
(test (interp (parse 1000)) 1000)
(test (interp (parse -8)) -8)
(test (interp (parse 30)) 30)
(test (interp (parse 15)) 15)

;; interp: AE -> any
(test (interp (parse '(+ 1 3))) 4)
(test (interp (parse '(- 10 5))) 5)
(test (interp (parse '(* 10 5))) 50)
(test (interp (parse '(/ 6 2))) 3)
(test (interp (parse '(% 10 3))) 1)
(test (interp (parse '(min 6 2))) 2)
(test (interp (parse '(max 6 2))) 6)
(test (interp (parse '(pow 2 3))) 8)

;; interp: AE -> any
(test (interp (parse '(with {{a 2} {b 3}} {+ a b}))) 5)
(test (interp (parse '(with {{a 2} {b ({fun {x y} {pow x y}} 2 3)}} {+ a b}))) 10)
(test (interp (parse '(with {{a 2} {b (+ 2 3)}} {+ a b}))) 7)
(test (interp (parse '(with {{a 2} {b (min 6 2)}} {+ a b}))) 4)
(test (interp (parse '(with {{a 2} {b (max 6 2)}} {+ a b}))) 8)

;; interp: AE -> any
(test (interp (parse '({fun {x y} {pow x y}} 2 3))) 8)
(test (interp (parse '({fun {x y} {min x y}} 2 3))) 2)
(test (interp (parse '({fun {x} {+ x x}} 2))) 4)
(test (interp (parse '({fun {x} {% x x}} 3))) 0)
(test (interp (parse '({fun {x} {x}} 100))) 100)
