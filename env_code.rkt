#lang racket
(require (lib "eopl.ss" "eopl"))
(include "parser.rkt")
(include "value-of.rkt")
(define-datatype enviroment enviroment?
  
  (empty-env)
  
  (extend-env
   (var symbol?)
   (val exp?)
   (env enviroment?))

  (extend-env-rec
   (p-name symbol?)
   (b-var symbol?)
   (body command?)
   (env enviroment?)))

(define apply-env
  (lambda (env search-var)
   (cases enviroment env
     (empty-env () (display "report-no-binding-found"))
     (extend-env (saved-var saved-val saved-env)
                 (if (eqv? search-var saved-var)
                     saved-val 
                     (apply-env saved-env search-var)))
     
     (extend-env-rec (p-name b-var p-body saved-env)
                     (if (eqv? search-var p-name)
                          (value-of-func (func-exp b-var p-body env))
                          (apply-env saved-env search-var))))))
