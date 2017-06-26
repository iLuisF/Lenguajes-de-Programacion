#lang plai

(require "./streams/streams.rkt")
(require "./streams/streams-ejem.rkt")
(require "./Lazy-CFBAE/parser.rkt")
(require "./Lazy-CFBAE/grammars.rkt")
(require "./Lazy-CFBAE/interp.rkt")

;; smap: procedure Stream -> Stream
(test (stake (smap (lambda (number) (+ 1 number)) naturales) 10)
      '(1 2 3 4 5 6 7 8 9 10))
(test (stake (smap (lambda (number) (+ 1 number)) factoriales) 10)
      '(2 2 3 7 25 121 721 5041 40321 362881))
(test (stake (smap (lambda (number) (+ 0 number)) fibonaccies) 10)
      '(1 1 2 3 5 8 13 21 34 55))

;; sfilter: procedure Stream -> Stream
(test (stake (sfilter (lambda (number) (> number 10)) naturales) 10)
      '(11 12 13 14 15 16 17 18 19 20))
(test (stake (sfilter (lambda (number) (> number 1)) factoriales) 3)
      '(2 6 24))
(test (stake (sfilter (lambda (number) (> number 2)) naturales) 5)
      '(3 4 5 6 7))

;; triangular: number -> number
(test (triangular 5) 15)
(test (triangular 2) 3)
(test (triangular 101) 5151)

;; genera-triangulares: number -> Stream
(test (stake (genera-triangulares 1) 10) '(1 3 6 10 15 21 28 36 45 55))
(test (stake (genera-triangulares 2) 5) '(3 6 10 15 21))
(test (stake (genera-triangulares 1) 2) '(1 3))

;;Expresiones para evaluar más adelante con parse y desugar.
(define expr2  '{with {{x 5} {y 10}} {x}})
(define expr4  '{with {{x 3}} {pow x 2}})
(define expr16 '{with {{x 3}} {fun {y} {+ x y}}})
(define expr18 '{and {> 5 6} false})

;; parse: symbol -> CFWBAE/L
(test (parse expr2) (withS (list (binding 'x (numS 5)) (binding 'y (numS 10))) (appS (idS 'x) '())))
(test (parse expr4) (withS (list (binding 'x (numS 3))) (opS expt (list (idS 'x) (numS 2)))))
(test (parse expr16) (withS (list (binding 'x (numS 3))) (funS '(y) (opS + (list (idS 'x) (idS 'y))))))
(test (parse expr18) (opS mand (list (opS > (list (numS 5) (numS 6))) (boolS #f))))

;; desugar :: CFWBAE/L -> CFBAE/L
(test (desugar (parse expr2)) (app (fun '(x y) (app (id 'x) '())) (list (num 5) (num 10))))
(test (desugar (parse expr4)) (app (fun '(x) (op expt (list (id 'x) (num 2)))) (list (num 3))))
(test (desugar (parse expr16)) (app (fun '(x) (fun '(y) (op + (list (id 'x) (id 'y))))) (list (num 3))))
(test (desugar (parse expr18)) (op mand (list (op > (list (num 5) (num 6))) (bool #f))))

;; interp: CFBAE/L Env -> CFBAE/L-Value
(test (interp (desugar (parse expr4)) (mtSub)) (numV 9))
(test (interp (desugar (parse expr16)) (mtSub)) (closureV '(y) (op + (list (id 'x) (id 'y)))
 (aSub 'x (exprV (num 3) (mtSub)) (mtSub))))
(test (interp (desugar (parse expr18)) (mtSub)) (boolV #f))

