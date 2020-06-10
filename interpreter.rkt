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

(define (operation List num f) (cond
                   [(null? List) '()]
                   [(number? (car List)) (append (list(f (car List) num)) (operation (cdr List) num f))]
                   [(list? (car List)) (append (list(operation (car List) num f)) (operation (cdr List) num f))]
                   [else (begin (display (car List)) (display " is not a type of operation dude."))]
                  ))

(define (bool-mult List bool-val) (cond
                                     [(null? List) '()]
                                     [(boolean? (car List)) (append (list(and (car List) bool-val)) (bool-mult (cdr List) bool-val))]
                                     [(list? (car List)) (append (list(bool-mult (car List) bool-val)) (bool-mult (cdr List) bool-val))]
                                     [else (begin (display (car List)) (display " is not a boolean ,OMEGA LUL."))]
                                     ))

(define (bool-or List bool-val) (cond
                                     [(null? List) '()]
                                     [(boolean? (car List)) (append (list(or (car List) bool-val)) (bool-or (cdr List) bool-val))]
                                     [(list? (car List)) (append (list(bool-or (car List) bool-val)) (bool-or (cdr List) bool-val))]
                                     [else (begin (display (car List)) (display " is not a boolean bru."))]
                                     ))

(define (str-app List str) (cond
                   [(null? List) '()]
                   [(string? (car List)) (append (list(string-append (car List) str)) (str-app (cdr List) str))]
                   [(list? (car List)) (append (list(str-app (car List) str)) (str-app (cdr List) str))]
                   [else (begin (display (car List)) (display " is not a type of string WTF!!."))]
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


(define value-of-bexp
  (lambda (expression env)
    (cases bexp expression

      (bexp-cexp (expr) (value-of-cexp expr env))

      (bexp-mult (exp1 exp2)
                 (let ([operand1 (value-of-cexp exp1 env)]
                       [operand2 (value-of-bexp exp2 env)])
                   (cond
                     [(and (number? operand1) (number? operand2)) (* operand1 operand2)]
                     [(and (number? operand1) (list? operand2)) (operation operand2 operand1 *)]
                     [(and (list? operand1) (number? operand2)) (operation operand1 operand2 *)]
                     [(and (boolean? operand1) (boolean? operand2)) (and operand1 operand2)]
                     [(and (boolean? operand1) (list? operand2)) (bool-mult operand2 operand1)]
                     [(and (list? operand1) (boolean? operand2)) (bool-mult operand1 operand2)]
                     [else (display "ERROR")])
                     ))

      (bexp-div (exp1 exp2)
                (let ([operand1 (value-of-cexp exp1 env)]
                       [operand2 (value-of-bexp exp2 env)])
                   (cond
                     [(and (number? operand1) (number? operand2)) (/ operand1 operand2)]
                     [(and (number? operand1) (list? operand2)) (operation operand2 operand1 /)]
                     [(and (list? operand1) (number? operand2)) (operation operand1 operand2 /)]
                     [else (display "ERROR")])
                     ))
                )))


(define value-of-aexp
  (lambda (expression env)
   (cases aexp expression

     (aexp-bexp (expr) (value-of-bexp expr env))

     (aexp-minus (exp1 exp2)
                 (let ([operand1 (value-of-bexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
                   (cond
                     [(and (number? operand1) (number? operand2)) (- operand1 operand2)]
                     [(and (number? operand1) (list? operand2)) (operation operand2 operand1 -)]
                     [(and (list? operand1) (number? operand2)) (operation operand1 operand2 -)]
                     [else (display "ERROR")])
                     ))

     (aexp-plus (exp1 exp2)
                (let ([operand1 (value-of-bexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
                   (cond
                     [(and (number? operand1) (number? operand2)) (+ operand1 operand2)]
                     [(and (number? operand1) (list? operand2)) (operation operand2 operand1 +)]
                     [(and (list? operand1) (number? operand2)) (operation operand1 operand2 +)]
                     [(and (boolean? operand1) (boolean? operand2)) (or operand1 operand2)]
                     [(and (boolean? operand1) (list? operand2)) (bool-or operand2 operand1)]
                     [(and (list? operand1) (boolean operand2)) (bool-or operand1 operand2)]
                     [(and (string? operand1) (string? operand2)) (string-append operand1 operand2)]
                     [(and (string? operand1) (list? operand2)) (str-app operand2 operand1)]
                     [(and (list? operand1) (string? operand2)) (str-app operand1 operand2)]
                     [(and (list? operand1) (list? operand2)) (append operand1 operand2)]
                     [else (display "ERROR")])
                     ))
       )))


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
      
                   