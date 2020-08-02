#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/include)

(define (pow l) (if (= (list-ref l 1) 0) 1
                      (if (positive? (list-ref l 1))
                      (* (list-ref l 0) (pow (list (list-ref l 0) (- (list-ref l 1) 1 ))))
                      (/ 1 (list (pow (list-ref l 0) (- (list-ref l 1)))))
                      )))

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))
  )
)

(define (set ls)
 (let ([L (list-ref ls 0)]
        [n (list-ref ls 1)]
        [c (list-ref ls 2)])
 (set! n (+ n 1))
  (let loop ((x 1)
             (em '()))
    (cond
      [(> x (length L))
       (reverse em)]
      [(= x n)
       (loop (add1 x) (cons c em))]
      [else
       (loop (add1 x) (cons (list-ref L (sub1 x)) em))]))))



(define (merge ls)
  (let ([ls1 (list-ref ls 0)]
        [ls2 (list-ref ls 1)])
  (match* (ls1 ls2)
    [((list) ls2)  ls2]
    [(as (list))  ls1]
    [((list a ls1 ...) (list b ls2 ...))
     (if (< a b)
         (cons a (merge ls1 (cons b ls2)))
         (cons b (merge (cons a ls1) ls2)))])))


(define (mergeSort ls)
  (match ls
    [(list)  ls]
    [(list a)  ls]
    [_  (define-values (lvs rvs)
          (split-at ls (quotient (length ls) 2)))
        (merge (mergeSort lvs) (mergeSort rvs))]))


(define (makeList ls) (let ([a (list-ref ls 0)]
                             [b (list-ref ls 1)])
                         (cond
                          [(or (zero? a) (< a 0)) '()]
                          [else (makeList a b)]
                         )))

(define (reverseAll L) (cond
                   [(null? L) '()]
                   [else (append (reverseAll (cdr L)) (if (list? (car L)) (list (reverseAll (car L))) (list (car L))))]
                   ))




