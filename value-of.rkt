(require (lib "eopl.ss" "eopl"))
(require racket/include)
(include "parser.rkt")
(require dyoo-while-loop)

;;;;;; AAaalooo


(define (pow l) (if (= (list-ref l 1) 0) 1
                      (if (positive? (list-ref l 1))
                      (* (list-ref l 0) (pow (list (list-ref l 0) (- (list-ref l 1) 1 ))))
                      (/ 1 (list (pow (list-ref l 0) (- (list-ref l 1)))))
                      )))

(define (Reverse ls)
  (let ([l (list-ref ls 0)])
  (if (null? l)
      '()
      (reverse l)
  )
))

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
         (cons a (merge (list ls1 (cons b ls2))))
         (cons b (merge (list (cons a ls1) ls2))))])))


(define (mergeSort l)
  (let ([ls (list-ref l 0)])
  (merge_sort ls)))


(define (merge_sort ls)
  (match ls
    [(list)  ls]
    [(list a)  ls]
    [_  (define-values (lvs rvs)
          (split-at ls (quotient (length ls) 2)))
        (merge2arg (merge_sort lvs) (merge_sort rvs))]))


(define (merge2arg ls1 ls2)
  (match* (ls1 ls2)
    [((list) ls2)  ls2]
    [(as (list))  ls1]
    [((list a ls1 ...) (list b ls2 ...))
     (if (< a b)
         (cons a (merge2arg ls1 (cons b ls2)))
         (cons b (merge2arg (cons a ls1) ls2)))]))


(define (makeList ls) (let ([a (list-ref ls 0)]
                             [b (list-ref ls 1)])
                         (cond
                          [(or (zero? a) (< a 0)) '()]
                          [else (make-list a b)]
                         )))

(define (reverseAll ls)
  (let ([Li (list-ref ls 0)])
   (deep-reverse)))

(define (deep-reverse l)
  (if (list? l)
      (reverse (map deep-reverse l))
      l))

(define (eval ls)
  (let ([str (list-ref ls 0)])
  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this simple-math-lexer (open-input-string str)))
  (with-handlers (
                  [(lambda (v) (and (list? v) (equal? (first v) 'return))) (lambda (v) (list-ref v 1))])
    (value-of-command (let ((parser-res (simple-math-parser my-lexer))) parser-res) (empty-env))
  )
))



(define lib-name (list 'pow 'makeList 'reverse 'reverseAll 'merge 'mergeSort 'set 'eval))
(define lib-proc (list pow makeList Reverse reverseAll merge mergeSort set eval))
(define (exist? v) (index-of lib-name v))






;;;;; ENV IS HERE
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

(define-datatype thunk thunk?
  (a-thunk
   (expr exp?)
   (saved-env enviroment?))
  (call-thunk
   (cl call?)
   (saved-env enviroment?))
  )


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
                     (if (thunk? saved-val) (cases thunk saved-val
                                              (a-thunk (expr saved-env) (value-of-exp expr saved-env))
                                              (call-thunk (cl saved-env) (value-of-call cl saved-env))) saved-val) 
                     (apply-env saved-env search-var)))
     
     (extend-env-func (p-name func saved-env)
                     (if (eqv? search-var p-name)
                         (cases function func
                           (func-def (vars body) (procedure vars body env)))
                          (apply-env saved-env search-var)))))))



;;;;;;;;;;;;; Value Of

(define (comp List) (cond
                   [(null? List) '()]
                   [(number? (car List)) (append (list(- (car List))) (comp (cdr List)))]
                   [(boolean? (car List)) (append (list(not (car List))) (comp (cdr List)))]
                   [(list? (car List)) (append (list(comp (car List))) (comp (cdr List)))]
                   [else (raise "invalid type entry for complement operation.")]
                  ))

(define (operation List num f) (cond
                   [(null? List) '()]
                   [(number? (car List)) (append (list(f (car List) num)) (operation (cdr List) num f))]
                   [(list? (car List)) (append (list(operation (car List) num f)) (operation (cdr List) num f))]
                   [else (raise "Not a number type for list entry due to multiply/divide/add/minus operation.")]
                  ))

(define (bool-mult List bool-val) (cond
                                     [(null? List) '()]
                                     [(boolean? (car List)) (append (list(and (car List) bool-val)) (bool-mult (cdr List) bool-val))]
                                     [(list? (car List)) (append (list(bool-mult (car List) bool-val)) (bool-mult (cdr List) bool-val))]
                                     [else (raise "Not a boolean type for list entry due to boolean AND operation.")]
                                     ))

(define (bool-or List bool-val) (cond
                                     [(null? List) '()]
                                     [(boolean? (car List)) (append (list(or (car List) bool-val)) (bool-or (cdr List) bool-val))]
                                     [(list? (car List)) (append (list(bool-or (car List) bool-val)) (bool-or (cdr List) bool-val))]
                                     [else (raise "Not boolean type for list entry due to boolean OR operation.")]
                                     ))

(define (str-app List str) (cond
                   [(null? List) '()]
                   [(string? (car List)) (append (list(string-append (car List) str)) (str-app (cdr List) str))]
                   [(list? (car List)) (append (list(str-app (car List) str)) (str-app (cdr List) str))]
                   [else (raise "Not a string type for list entry due to string appending operation.")]
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
 (let ([r env])
   (begin
   (cases whilecom wcmd
    (while-cmd (exper cmd)
             (while 
             (value-of-exp exper r)
             (begin
              (set! r (value-of-command cmd r))))))
   r
   )))




(define value-of-ifcom
  (lambda (cmd env)
         (cases ifcom cmd
           (if-cmd (exp1 exp2 exp3)
                   (let ((val1 (value-of-exp exp1 env)))
                     (if val1
                         (value-of-command exp2 env)
                         (value-of-command exp3 env)))))))
                     
(define (value-of-assign asgn env)
  (begin
    ;(display asgn)
    ;(display "\n\n")
  (cases assign asgn
    (assign-cmd-exp (var expr)
    (extend-env var (a-thunk expr env) env))
    (assign-cmd-func (var func)
                     (extend-env-func var func env))
    (assign-cmd-call (var cl)
                     (extend-env var (call-thunk cl env) env)
                     )))
)

(define (value-of-call cl env)
  (begin
    (cases call cl
  (call-func (proc-name arguments)
             
             (if (exist? proc-name)
                   ((list-ref lib-proc (exist? proc-name)) (get-val-arg arguments env '()))
                
             (let ([pr (apply-env env proc-name)])
               (cases proc pr
                 (procedure (var body saved-env)
                            (with-handlers (
                  [(lambda (v)  (list? v)) (lambda (v) (list-ref v 1))])
    (value-of-command body (extend-arg arguments var saved-env env))))))
                   )
             ))))

(define (get-val-arg arguments env l)
  (cases args arguments
    (single-arg (ar) (append l (list (value-of-exp ar env))))
    (multi-arg (first-arg rest-arg)
               (get-val-arg rest-arg env (append l (list (value-of-exp first-arg env))))
                            )))

(define (extend-arg ar va saved-env env)
  (cases args ar
    (single-arg (a)
                (cases vars va
                  (single-var (v) (extend-env v (a-thunk a env) saved-env))
                  (multi-var (first-var rest-var) (raise "More Args Needed!"))))
    (multi-arg (first-arg rest-arg)
               (cases vars va
                  (single-var (v) (raise "Unexpected Arg!"))
                  (multi-var (first-var rest-var) (extend-arg rest-arg rest-var (extend-env first-var (a-thunk first-arg env) saved-env) env))))
    ))


           
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
                   (begin

                    (cond
                      [(and (number? operand1) (number? operand2)) (> operand1 operand2)]
                      
                      [(and (string? operand1) (string? operand2)) (string<? operand1 operand2)]
                      [(and (string? operand1) (list? operand2)) (if (andmap string? operand2)
                                                                     (map (lambda (x) (string>? x operand1)) operand2)
                                                                     false)]
                      [(and (list? operand1) (string? operand2))(if (andmap string? operand1)
                                                                    (map (lambda (x) (string<? x operand2)) operand1)
                                                                    false)]
                      
                      [(and (number? operand1) (list? operand2)) (if (andmap number? operand2)
                                                           (map (lambda (x) (> x operand1)) operand2)
                                                           false)]
                      
                      [(and (list? operand1) (number? operand2))(if (andmap number? operand1)
                                                           (map (lambda (x) (> x operand2)) operand1)
                                                           false)]
                       [else  (raise (list 'error "NOT valid types of data for bigger comparison"))]
                      ))))
                     
     (exp-smaller (exp1 exp2)
      (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
                   (cond
                      [(and (number? operand1) (number? operand2)) (< operand1 operand2)]
                      
                      [(and (string? operand1) (string? operand2)) (string>? operand1 operand2)]
                      
                      [(and (string? operand1) (list? operand2)) (if (andmap string? operand2)
                                                                     (map (lambda (x) (string<? x operand1)) operand2)
                                                                     false)]
                      
                      [(and (list? operand1) (string? operand2)) (if (andmap string? operand1)
                                                                     (map (lambda (x) (string>? x operand2)) operand1)
                                                                     false)]
                      
                      [(and (number? operand1) (list? operand2)) (if (andmap number? operand2)
                                                           (map (lambda (x) (< x operand1)) operand2)
                                                           false)]
                      
                      [(and (list? operand1) (number? operand2)) (if (andmap number? operand1)
                                                           (map (lambda (x) (< x operand2)) operand1)
                                                           false)]
                      [else (raise "NOT valid types of data for smaller comparison")]
                      )))
      

      
      (exp-equal (exp1 exp2)
       
         (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
         (begin
         
         
         (cond
           [(and (number? operand1) (number? operand2)) (equal? operand1 operand2)]
           [(and (string? operand1) (string? operand2)) (equal? operand1 operand2)]
           [(and (null? operand1) (null? operand2)) true]
           [(and (boolean? operand1) (boolean? operand2)) (equal? operand1 operand2)]
           [(and (list? operand1) (list? operand2)) (equal? operand1 operand2)]
           [(and (list? operand1) (number? operand2)) (if (andmap number? operand1)
                                                           (map (lambda (x) (equal? x operand2)) operand1)
                                                           false)]
           [(and (number? operand1) (list? operand2)) (if (andmap number? operand2)
                                                           (map (lambda (x) (equal? x operand1)) operand2)
                                                           false)]
           [(and (list? operand1) (string? operand2)) (if (andmap string? operand1)
                                                           (map (lambda (x) (equal? x operand2)) operand1)
                                                           false)]
           [(and (string? operand1) (list? operand2)) (if (andmap string? operand2)
                                                           (map (lambda (x) (equal? x operand1)) operand2)
                                                           false)]
           
           [(and (list? operand1) (boolean? operand2)) (if (andmap boolean? operand1)
                                                           (map (lambda (x) (equal? x operand2)) operand1)
                                                           false)]
           [(and (boolean? operand1) (list? operand2)) (if (andmap boolean? operand2)
                                                           (map (lambda (x) (equal? x operand1)) operand2)
                                                           false)]
           [(and (list? operand1) (null? operand2)) (if (andmap null? operand1)
                                                           (map (lambda (x) (equal? x operand2)) operand1)
                                                           false)]
           [(and (null? operand1) (list? operand2)) (if (andmap null? operand2)
                                                           (map (lambda (x) (equal? x operand1)) operand2)
                                                           false)]
           [else (raise "NOT valid types of data for equality comparison")]
           
))))
      

       (exp-not-equal (exp1 exp2)
        (let ([operand1 (value-of-aexp exp1 env)]
                       [operand2 (value-of-aexp exp2 env)])
         (cond
           [(and (number? operand1) (number? operand2)) (not (equal? operand1 operand2))]
           [(and (string? operand1) (string? operand2))(not (equal? operand1 operand2))]
           [(and (null? operand1) (null? operand2)) false]
           [(and (boolean? operand1) (boolean? operand2)) (not (equal? operand1 operand2))]
           [(and (list? operand1) (list? operand2)) (not (equal? operand1 operand2))]
           [(and (list? operand1) (number? operand2)) (if (andmap number? operand1)
                                                           (map (lambda (x) (not(equal? x operand2))) operand1)
                                                           true)]
           [(and (number? operand1) (list? operand2)) (if (andmap number? operand2)
                                                           (map (lambda (x) (not(equal? x operand1))) operand2)
                                                           true)]
           [(and (list? operand1) (string? operand2)) (if (andmap string? operand1)
                                                           (map (lambda (x) (not(equal? x operand2))) operand1)
                                                           true)]
           [(and (string? operand1) (number? operand2)) (if (andmap string? operand2)
                                                           (map (lambda (x) (not(equal? x operand1))) operand2)
                                                           true)]
           [(and (list? operand1) (boolean? operand2)) (if (andmap boolean? operand1)
                                                           (map (lambda (x) (not(equal? x operand2))) operand1)
                                                           true)]
           [(and (boolean? operand1) (list? operand2)) (if (andmap boolean? operand2)
                                                           (map (lambda (x) (not(equal? x operand1))) operand2)
                                                           true)]
           [(and (list? operand1) (null? operand2)) (if (andmap null? operand1)
                                                           (map (lambda (x) (not(equal? x operand2))) operand1)
                                                           true)]
           [(and (null? operand1) (list? operand2)) (if (andmap null? operand2)
                                                           (map (lambda (x) (not(equal? x operand1))) operand2)
                                                           true)]
           [else (raise "NOT valid types of data for not-equal comparison")]
           
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
                     [else (raise "Input data type can not be complement.")])
                    ))

      (cexp-par (expr) (value-of-exp expr env))
                
      (cexp-num (num-exp) num-exp)

      (cexp-null (null-exp) null-exp)

      (cexp-var (var) (apply-env env var))

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
                 (let ([operand1 (value-of-cexp exp1 env)])
                   (cond
                     [(number? operand1) (if (zero? operand1) 0 (* operand1 (value-of-bexp exp2 env)))]
                     [(boolean? operand1) (if (false? operand1) #f (and operand1 (value-of-bexp exp2 env)))]
                     
                     [(and (number? operand1) (list? (value-of-bexp exp2 env))) (operation (value-of-bexp exp2 env) operand1 *)]
                     [(and (list? operand1) (number? (value-of-bexp exp2 env))) (operation operand1 (value-of-bexp exp2 env) *)]     
                     [(and (boolean? operand1) (list? (value-of-bexp exp2 env))) (bool-mult (value-of-bexp exp2 env) operand1)]
                     [(and (list? operand1) (boolean? (value-of-bexp exp2 env))) (bool-mult operand1 (value-of-bexp exp2 env))]
                     
                     [else (raise "Invalid operand type for multiply operation.")])
                     ))

      (bexp-div (exp1 exp2)
                (let ([operand1 (value-of-cexp exp1 env)]
                       [operand2 (value-of-bexp exp2 env)])
                   (cond
                     [(and (number? operand1) (number? operand2)) (/ operand1 operand2)]
                     [(and (number? operand1) (list? operand2)) (operation operand2 operand1 /)]
                     [(and (list? operand1) (number? operand2)) (operation operand1 operand2 /)]
                     [else (raise  "Invalid operand type for divide operation.")])
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
                     [else (raise "Invalid operand type for minus operation.")])
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
                     [else (raise "Invalid operand type for sum operation.")])
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
   (listmem-single (index) (list (expmem->val index env)))
   (listmem-multi (index rindex) (cons (expmem->val index env) (value-of-listmem rindex env)))))

(define (expmem->val expr env)
  (let
      ([val (value-of-exp expr env)])
    (cond
      [(not (integer? val)) (list 'error "list index must be a number")]
      [(negative? val) (list 'error "list index can not be negative")]
      [else val])
    )
)


(define (list-index l index)
  (if (empty? index)
      l
      (if (>= (first index) (length l))
          (list 'error "list index out of range dude!!!")
          (list-index (list-ref l (first index)) (rest index)))
      )
  )


      
                   