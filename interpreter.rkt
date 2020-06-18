#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/include)
(include "parser.rkt")
(include "env_code.rkt")
(include "value-of.rkt")




(define (interperet in)
  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this simple-math-lexer (open-input-string (file->string in #:mode 'text))))
  (with-handlers (
                  [(lambda (v)  (list? v)) (lambda (v) (list-ref v 1))])
    (value-of-command (let ((parser-res (simple-math-parser my-lexer))) parser-res) (empty-env))
  )
)








(interperet "prog.txt")




      
                   