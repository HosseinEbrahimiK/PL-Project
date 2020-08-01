#lang racket
(define (pow a b) (if (= b 0) 1
                      (if (positive? b)
                      (* a (pow a (- b 1 )))
                      (/ 1 (pow a (- b)))
                      )))
(pow 3 5)


(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))
  )
)

(reverse '(1 '(12 3) 3) )

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

(set '(a b c d e) 4 'd)

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

(merge '(1 4) '(3 5))
(merge_sort '(1 4 2 8 3))