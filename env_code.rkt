#lang racket

(require (lib "eopl.ss" "eopl"))

(define empty-env
    (lambda ()
      (lambda (search-var)
        (report-no-binding-found search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
    (lambda (env search-var)
      (env search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error "No binding for ~s" search-var)))