#lang racket

(define (value-of-command cmd,env)
  (cases command cmd
    (command-single(ucmd)
                   (value-of-unitcom ucmd env))
    (command-multi (cmd ucmd)

                   (value-of-unitcom ucmd (value-of-command cmd env)))))


(define (value-of-unitcom ucmd, env)
  (cases unitcom ucmd
    (unitcom-while(wcmd)
                  (value-of-whilecom wcmd env))
    (unitcom-if(ifcmd)
               (value-of-ifcom ifcmd env))
    (unitcom-assign(asgn)
                   (value-of-assign asgn env))
    (unitcom-return(rucmd)
                   (value-of-return rucmd env))))



(define (value-of-whilecom wcmd, env)
  (cases whilecom wcmd
    (whilecmd(exper cmd)
             (value-of-exp exper env)
             (value-of-command cmd env))))




(define value-of-ifcom
  (lamda (exp env)
         (cases expression exp
           (if-cmd (exp1 exp2 exp3)
                   (let ((val1 (value-of-exp exp1 env)))
                     (if val1
                         (value-of-command exp2 env)
                         (value-of-command exp3 env)))))))
                     
 (define value-of-assign
   (lambda (exp env)
     (cases expression exp
       (assign-cmd (var exp1 env)
                   (let ((val1 (value-of-exp exp1 env)))
                     (value-of-exp (extend-env var val1 env)))))))   ; in khato motmaen nistam
           


;RETURN MOONDE





(define value-of-exp
  (lambda expression env)
  (cases exp expression
    (exp-aexp (expr) (value-of-aexp expr env)

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
      
-
        


       
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
    
