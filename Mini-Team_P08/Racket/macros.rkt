#lang plai

(define-syntax for
  (syntax-rules (do)
    [(for i c a do body ...)
     (local ([define loop (lambda (id)
               (if (not c)
                   (newline)
                   (begin
                     a
                     body ...
                     (loop i))))])
       (loop i))]))