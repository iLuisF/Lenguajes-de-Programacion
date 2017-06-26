#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Análisis semántico del lenguaje. El analizador semántico se encarga de dar
;; un significado al árbol de sintaxis abstracta.

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por
;; el parser. El intérprete requiere cargar un ambiente en esta versión.
;; interp: CFBAE/L Env -> CFBAE/L-Value
(define (interp exp ds)
   (error "No implementado"))

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
