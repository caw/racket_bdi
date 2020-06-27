#lang racket
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ body)
     #`(let #,(map (lambda (x)
                     (list x
                           (char->integer (car (string->list (symbol->string x))))))
                   '(a b))
         body)]))

(test (+ a b))