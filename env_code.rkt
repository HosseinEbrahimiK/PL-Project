;#lang racket
;(require (lib "eopl.ss" "eopl"))
;(include "parser.rkt")
;(include "value-of.rkt")

(define-datatype proc proc?
  (procedure
   (var vars?)
   (body command?)
   (env enviroment?))
)
(define (any? obj)
  true)
(define-datatype enviroment enviroment?
  
  (empty-env)
  
  (extend-env
   (var symbol?)
   (val any?)
   (env enviroment?))

  (extend-env-func
   (name symbol?)
   (func function?)
   (env enviroment?)))



(define apply-env
  (lambda (env search-var)
   (begin
     ;(display search-var)
     ;(display " in:\n")
     ;(display env)
     ;(display "\n\n")
     (cases enviroment env
     (empty-env () (display "report-no-binding-found"))
     (extend-env (saved-var saved-val saved-env)
                 (if (eqv? search-var saved-var)
                     saved-val 
                     (apply-env saved-env search-var)))
     
     (extend-env-func (p-name func saved-env)
                     (if (eqv? search-var p-name)
                         (cases function func
                           (func-def (vars body) (procedure vars body env)))
                          (apply-env saved-env search-var)))))))



