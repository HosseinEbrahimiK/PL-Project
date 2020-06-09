#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/include)
(include "parser.rkt")

(define (comp List) (cond
                   [(null? List) '()]
                   [(number? (car List)) (append (list(- (car List))) (comp (cdr List)))]
                   [(boolean? (car List)) (append (list(not (car List))) (comp (cdr List)))]
                   [(list? (car List)) (append (list(comp (car List))) (comp (cdr List)))]
                   [else (begin (display (car List)) (display " is the element of array: can not be negative."))]
                  ))

(define value-of-cexp
  (lambda (expression env)
    (cases cexp expression
      
      (cexp-comp (expr)
                 (let ([val (value-of-cexp expr env)])
                    (cond
                     [(number? val) (- val)]
                     [(boolean? val) (not val)]
                     [(list? val) (comp val)]
                     [else (display "This kind of data can not be negative.")])
                    ))

      (cexp-par (expr) (value-of-exp expr))
                
      (cexp-num (num-exp) num-exp)

      (cexp-null (null-exp) null-exp)

      (cexp-var (var) var)

      (cexp-bool (bool-exp) bool-exp)

      (cexp-string (str) str))))


(define (value-of-lst l)
  (cases lst l
      (lst-empty () (list))
      (lst-non-empty (lv) (value-of-listvalues lv))
   )
  )

(define (value-of-listvalues lv)
  (cases listvalues lv
    (listvalues-single (expr) (list (valur-of-exp expr)))
    (listvalues-multi (expr lvr) (cons (value-of-expr expr) (value-of-exp expr)))
    ))

(define (value-of-listmem lm)
  (cases listmem lm
   (listmem-single (index) (list (value-of-exp index)))
   (listmem-multi (index rindex) (cons (value-of-exp index) (value-of-listmem rindex)))))
      
                   