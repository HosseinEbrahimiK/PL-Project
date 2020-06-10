;(require (lib "eopl.ss" "eopl"))
(require racket/include)
;(include "parser.rkt")
;(include "env_code.rkt")
(require dyoo-while-loop)

(define (comp List) (cond
                   [(null? List) '()]
                   [(number? (car List)) (append (list(- (car List))) (comp (cdr List)))]
                   [(boolean? (car List)) (append (list(not (car List))) (comp (cdr List)))]
                   [(list? (car List)) (append (list(comp (car List))) (comp (cdr List)))]
                   [else (raise "invalid list entry")]
                  ))

(define (operation List num f) (cond
                   [(null? List) '()]
                   [(number? (car List)) (append (list(f (car List) num)) (operation (cdr List) num f))]
                   [(list? (car List)) (append (list(operation (car List) num f)) (operation (cdr List) num f))]
                   [else (raise "invalid operation dude.")]
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


(define (value-of-command cmd env)
  (cases command cmd
    (command-single(ucmd)
                   (value-of-unitcom ucmd env))
    (command-multi (cmd ucmd)

                   (value-of-unitcom ucmd (value-of-command cmd env)))))


(define (value-of-unitcom ucmd env)
  (cases unitcom ucmd
    (unitcom-while(wcmd)
                  (value-of-whilecom wcmd env))
    (unitcom-if(ifcmd)
               (value-of-ifcom ifcmd env))
    (unitcom-assign(asgn)
                   (value-of-assign asgn env))
    (unitcom-return(rucmd)
                   (value-of-return rucmd env))))



(define (value-of-whilecom wcmd env)
  (cases whilecom wcmd
    (while-cmd (exper cmd)
             (let ([r env])
             (while 
             (value-of-exp exper r)
             (set! r (value-of-command cmd env)))))))




(define value-of-ifcom
  (lambda (cmd env)
         (cases ifcom cmd
           (if-cmd (exp1 exp2 exp3)
                   (let ((val1 (value-of-exp exp1 env)))
                     (if val1
                         (value-of-command exp2 env)
                         (value-of-command exp3 env)))))))
                     
(define (value-of-assign asgn env)
  (cases assign asgn
    (assign-cmd (var expr)
    (extend-env var (value-of-exp expr env) env)))
)


           
(define (value-of-return ret env)
  (cases return ret
    (return-cmd (expr) (raise (list 'return (value-of-exp expr env)))))
  )



;exxxxxxxpppppppppp


 

(define value-of-exp
  (lambda (expression env)
  (cases exp expression
    (exp-aexp (expr) (value-of-aexp expr env))

    (exp-bigger (exp1 exp2)
                 (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
                   (cond
                      [(and (number? operand1) (number? operand2)) (> operand1 operand2)]
                      
                      [(and (string? operand1) (string? operand2)) (string<? operand1 operand2)]
                      [(and (string? operand1) (list? operand2)) (if (andmap string? operand2)
                                                                     (map (lambda (x) (string>? x operand1)) operand2)
                                                                     false)]
                      [(and (list? operand1) (string? operand2))(if (andmap string? operand1)
                                                                    (map (lambda (x) (string<? x operand2)) operand1)
                                                                    false)]
                      
                      [(and (number? operand1) (list? operand2)) ((if (andmap number? operand2)
                                                           (map (lambda (x) (> x operand1)) operand2)
                                                           false))]
                      
                      [(and (list? operand1) (number? operand2))((if (andmap number? operand1)
                                                           (map (lambda (x) (< x operand2)) operand1)
                                                           false))])))
                     
     (exp-smaller (exp1 exp2)
      (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
                   (cond
                      [(and (number? operand1) (number? operand2)) (< operand1 operand2)]
                      
                      [(and (string? operand1) (string? operand2)) ((string>? operand1 operand2))]
                      
                      [(and (string? operand1) (list? operand2)) (if (andmap string? operand2)
                                                                     (map (lambda (x) (string<? x operand1)) operand2)
                                                                     false)]
                      
                      [(and (list? operand1) (string? operand2)) (if (andmap string? operand1)
                                                                     (map (lambda (x) (string>? x operand2)) operand1)
                                                                     false)]
                      
                      [(and (number? operand1) (list? operand2)) ((if (andmap number? operand2)
                                                           (map (lambda (x) (< x operand1)) operand2)
                                                           false))]
                      
                      [(and (list? operand1) (number? operand2)) ((if (andmap number? operand1)
                                                           (map (lambda (x) (> x operand2)) operand1)
                                                           false))])))
      

      
      (exp-equal (exp1 exp2)
       (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
         (cond
           [(and (number? operand1) (number? operand2)) (eq? operand1 operand2)]
           [(and (string? operand1) (string? operand2)) (eq? operand1 operand2)]
           [(and (null? operand1) (null? operand2)) (true)]
           [(and (boolean? operand1) (boolean? operand2)) (eq? operand1 operand2)]
           [(and (list? operand1) (list? operand2)) (eq? operand1 operand2)]
           [(and (list? operand1) (number? operand2)) ((if (andmap number? operand1)
                                                           (map (lambda (x) (eq? x operand2)) operand1)
                                                           false))]
           [(and (number? operand1) (list? operand2)) ((if (andmap number? operand2)
                                                           (map (lambda (x) (eq? x operand1)) operand2)
                                                           false))]
           [(and (list? operand1) (string? operand2)) ((if (andmap string? operand1)
                                                           (map (lambda (x) (eq? x operand2)) operand1)
                                                           false))]
           [(and (string? operand1) (list? operand2)) ((if (andmap string? operand2)
                                                           (map (lambda (x) (eq? x operand1)) operand2)
                                                           false))]
           
           [(and (list? operand1) (boolean? operand2)) ((if (andmap boolean? operand1)
                                                           (map (lambda (x) (eq? x operand2)) operand1)
                                                           false))]
           [(and (boolean? operand1) (list? operand2)) ((if (andmap boolean? operand2)
                                                           (map (lambda (x) (eq? x operand1)) operand2)
                                                           false))]
           [(and (list? operand1) (null? operand2)) ((if (andmap null? operand1)
                                                           (map (lambda (x) (eq? x operand2)) operand1)
                                                           false))]
           [(and (null? operand1) (list? operand2)) ((if (andmap null? operand2)
                                                           (map (lambda (x) (eq? x operand1)) operand2)
                                                           false))]
           
)))
      

       (exp-not-equal (exp1 exp2)
        (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
         (cond
           [(and (number? operand1) (number? operand2)) (not (eq? operand1 operand2))]
           [(and (string? operand1) (string? operand2))(not (eq? operand1 operand2))]
           [(and (null? operand1) (null? operand2)) (false)]
           [(and (boolean? operand1) (boolean? operand2)) (not (eq? operand1 operand2))]
           [(and (list? operand1) (list? operand2)) (not (eq? operand1 operand2))]
           [(and (list? operand1) (number? operand2)) ((if (andmap number? operand1)
                                                           (map (lambda (x) (not(eq? x operand2))) operand1)
                                                           true))]
           [(and (number? operand1) (list? operand2)) ((if (andmap number? operand2)
                                                           (map (lambda (x) (not(eq? x operand1))) operand2)
                                                           true))]
           [(and (list? operand1) (string? operand2)) (if (andmap string? operand1)
                                                           (map (lambda (x) (not(eq? x operand2))) operand1)
                                                           true)]
           [(and (string? operand1) (number? operand2)) (if (andmap string? operand2)
                                                           (map (lambda (x) (not(eq? x operand1))) operand2)
                                                           true)]
           [(and (list? operand1) (boolean? operand2)) ((if (andmap boolean? operand1)
                                                           (map (lambda (x) (not(eq? x operand2))) operand1)
                                                           true))]
           [(and (boolean? operand1) (list? operand2)) ((if (andmap boolean? operand2)
                                                           (map (lambda (x) (not(eq? x operand1))) operand2)
                                                           true))]
           [(and (list? operand1) (null? operand2)) ((if (andmap null? operand1)
                                                           (map (lambda (x) (not(eq? x operand2))) operand1)
                                                           true))]
           [(and (null? operand1) (list? operand2)) ((if (andmap null? operand2)
                                                           (map (lambda (x) (not(eq? x operand1))) operand2)
                                                           true))]
           
)))
     
  )))
    

;eeeeeexxxxxxxxxxxpppppppp

(define value-of-cexp
  (lambda (expression env)
    (cases cexp expression
      
      (cexp-comp (expr)
                 (let ([val (value-of-cexp expr env)])
                    (cond
                     [(number? val) (- val)]
                     [(boolean? val) (not val)]
                     [(list? val) (comp val)]
                     [else (raise "This kind of data can not be negative.")])
                    ))

      (cexp-par (expr) (value-of-exp expr))
                
      (cexp-num (num-exp) num-exp)

      (cexp-null (null-exp) null-exp)

      (cexp-var (var) var)

      (cexp-bool (bool-exp) bool-exp)

      (cexp-string (str) str)
      (cexp-list (l) (value-of-lst l env))
      (cexp-listmem (id mem) (list-index (apply-env env id) (value-of-listmem mem env)))
      )))


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
                     [else (raise "ERROR")])
                     ))

      (bexp-div (exp1 exp2)
                (let ([operand1 (value-of-cexp exp1 env)]
                       [operand2 (value-of-bexp exp2 env)])
                   (cond
                     [(and (number? operand1) (number? operand2)) (/ operand1 operand2)]
                     [(and (number? operand1) (list? operand2)) (operation operand2 operand1 /)]
                     [(and (list? operand1) (number? operand2)) (operation operand1 operand2 /)]
                     [else (raise (list 'error "ERROR"))])
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
                     [else (raise (list 'error "ERROR"))])
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
                     [(and (list? operand1) (boolean? operand2)) (bool-or operand1 operand2)]
                     [(and (string? operand1) (string? operand2)) (string-append operand1 operand2)]
                     [(and (string? operand1) (list? operand2)) (str-app operand2 operand1)]
                     [(and (list? operand1) (string? operand2)) (str-app operand1 operand2)]
                     [(and (list? operand1) (list? operand2)) (append operand1 operand2)]
                     [else (raise (list 'error "ERROR"))])
                     ))
       )))



(define (value-of-lst l env)
  (cases lst l
      (lst-empty () (list))
      (lst-non-empty (lv) (value-of-listvalues lv env))
   )
  )

(define (value-of-listvalues lv env)
  (cases listvalues lv
    (listvalues-single (expr) (list (value-of-exp expr env)))
    (listvalues-multi (expr lvr) (cons (value-of-exp expr env) (value-of-listvalues lvr env)))
    ))

(define (value-of-listmem lm env)
  (cases listmem lm
   (listmem-single (index) (list (value-of-exp index env)))
   (listmem-multi (index rindex) (cons (value-of-exp index env) (value-of-listmem rindex env)))))




(define (list-index l index)
  (if (empty? index)
      l
      (list-index (list-ref l (first index)) (rest index)))
  )


      
                   