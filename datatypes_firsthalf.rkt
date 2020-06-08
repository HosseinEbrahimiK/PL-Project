#lang eopl

(define-datatype command command?
  (command-unitcom
   (unitcom-exp unitcom?))
  (multi-command
   (cmd command?)
   (multi-unitcom unitcom?)))


(define-datatype unitcom unitcom?
  (whilecom-unit
   (whilecom-exp whilecom?))
  (ifcom-unit
   (ifcom-exp ifcom?))
  (assign-unit
   (assign-exp assign?))
  (return-unit
   (return-exp return?)))


(define-datatype whilecom whilecom?
  (whilecom-ex
   (exper exp?)
   (cmd command?)))


(define-datatype ifcom ifcom?
  (if-exp
   (exper exp?)
   (do-cmd command?)
   (else-cmd command?)))


(define-datatype return return?
  (return-ex
   (return-exp exp?)))


(define-datatype assign assign?
  (assign-ex
   (var-exp var?)
   (exper exp?)))
 
   
 
(define-datatype exp exp?
  (aexp-ex
   (aexp-exp aexp?))

  (aexp-bigger
   (aexp1 aexp?)
   (aexp2 aexp?))

   (aexp-smaller
   (aexp1 aexp?)
   (aexp2 aexp?))
  
  (aexp-equal
   (aexp1 aexp?)
   (aexp2 aexp?))

  (aexp-notequal
   (aexp1 aexp?)
   (aexp2 aexp?)))
     
   