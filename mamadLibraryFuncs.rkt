#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/include)
(include "value-of.rkt")

(define (pow a b) (if (= b 0) 1
                      (if (positive? b)
                      (* a (pow a (- b 1 )))
                      (/ 1 (pow a (- b)))
                      )))

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))
  )
)

(define (set L n c)
 (set! n (+ n 1))
  (let loop ((x 1)
             (em '()))
    (cond
      [(> x (length L))
       (reverse em)]
      [(= x n)
       (loop (add1 x) (cons c em))]
      [else
       (loop (add1 x) (cons (list-ref L (sub1 x)) em))])))


(define (merge ls1 ls2)
  (match* (ls1 ls2)
    [((list) ls2)  ls2]
    [(as (list))  ls1]
    [((list a ls1 ...) (list b ls2 ...))
     (if (< a b)
         (cons a (merge ls1 (cons b ls2)))
         (cons b (merge (cons a ls1) ls2)))]))

(define (merge_sort ls)
  (match ls
    [(list)  ls]
    [(list a)  ls]
    [_  (define-values (lvs rvs)
          (split-at ls (quotient (length ls) 2)))
        (merge (merge_sort lvs) (merge_sort rvs))]))


(define (make_list a b) (
                         cond
                          [(or (zero? a) (< a 0)) '()]
                          [else (make-list a b)]
                         ))

(define (reverse_all L) (cond
                   [(null? L) '()]
                   [else (append (reverse_all (cdr L)) (if (list? (car L)) (list (reverse_all (car L))) (list (car L))))]
                   ))

(define (eval str)
  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this simple-math-lexer (open-input-string str)))
  (with-handlers (
                  [(lambda (v) (and (list? v) (equal? (first v) 'return))) (lambda (v) (list-ref v 1))])
    (value-of-command (let ((parser-res (simple-math-parser my-lexer))) parser-res) (empty-env))
  )
)