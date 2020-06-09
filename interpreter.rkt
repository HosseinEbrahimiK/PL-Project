#lang racket

(require (lib "eopl.ss" "eopl"))
(require racket/include)
(include "parser.rkt")



(define (value-of-lst l)
  (cases lst l
    (lst-empty() (list))
    (lst-non-empty (listvals) (value-of-listvalues listvals))
   ))

(define (value-of-listvalues listvals)
  (cases listvalues listvals
    (list-values-single (expr) (list (value-of-exp expr)))
    (list-values-multi (expr vals2) (cons (value-of-exp expr) (value-of-listvalues vals2)))
    )
  )

(define (value-of-listmem mem)
  (cases listmem mem
    (listmem-single (index) (list (value-of-exp index)))
    (listmem-multi (index rindex) (cons (value-of-exp index) (value-of-listmem rindex)))
    ))



(let ((parser-res (simple-math-parser my-lexer))) parser-res)