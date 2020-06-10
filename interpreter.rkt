#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/include)
(include "parser.rkt")
(include "env_code.rkt")
(include "value-of.rkt")




(define (interperet in)
  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this simple-math-lexer in))
  (with-handlers (
                  [(lambda (v) (and (list? v) (equal? (first v) 'return))) (lambda (v) (list-ref v 1))])
    (value-of-command (let ((parser-res (simple-math-parser my-lexer))) parser-res) (empty-env))
  )
)





(interperet (open-input-string "a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]; return a[1][1]"))



      
                   