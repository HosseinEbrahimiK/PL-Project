(require (lib "eopl.ss" "eopl"))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define null 'null)
(define (null? n) (equal? n 'null))

(define-datatype command command?
  (command-single
   (ucmd unitcom?))
  (command-multi
   (cmd command?)
   (ucmd unitcom?)))

(define-datatype unitcom unitcom?
  (unitcom-while
   (wcmd whilecom?))
  (unitcom-if
   (ifcmd ifcom?))
  (unitcom-assign
   (asgn assign?))
  (unitcom-return
   (rucmd return?)))


(define-datatype whilecom whilecom?
  (while-cmd
   (exper exp?)
   (cmd command?)))


(define-datatype ifcom ifcom?
  (if-cmd
   (exper exp?)
   (do-cmd command?)
   (else-cmd command?)))


(define-datatype return return?
  (return-cmd
   (return-exp exp?)))


(define-datatype assign assign?
  (assign-cmd
   (var-exp symbol?)
   (exper exp?)))
 
  
(define-datatype exp exp?
  (exp-aexp
   (aexp-exp aexp?))

  (exp-bigger
   (aexp1 aexp?)
   (aexp2 aexp?))

   (exp-smaller
   (aexp1 aexp?)
   (aexp2 aexp?))
  
  (exp-equal
   (aexp1 aexp?)
   (aexp2 aexp?))

  (exp-not-equal
   (aexp1 aexp?)
   (aexp2 aexp?)))


(define-datatype aexp aexp?
  (aexp-bexp
    (bexpression bexp?))
  (aexp-minus
    (exp1 bexp?)
    (exp2 aexp?))
  (aexp-plus
    (exp1 bexp?)
    (exp2 aexp?)))


(define-datatype bexp bexp?
  (bexp-cexp
    (cexpression cexp?))
  (bexp-mult
    (exp1 cexp?)
    (exp2 bexp?))
  (bexp-div
    (exp1 cexp?)
    (exp2 bexp?)))

(define non-negative? (lambda (v) (or (zero? v) (positive? v))))

(define-datatype cexp cexp?
  (cexp-comp
    (expr cexp?))
  (cexp-par
    (expr exp?))
  (cexp-num
    (pos-num non-negative?))
  (cexp-null
    (null-identifier null?))
  (cexp-var
    (var symbol?))
  (cexp-bool
    (bool-identifier boolean?))
  (cexp-string
    (str string?))
  (cexp-list
    (list-exp lst?))
  (cexp-listmem
    (id symbol?)
    (mem listmem?)))


(define-datatype lst lst?
  (lst-empty
    )
  (lst-non-empty
    (list-vals listvalues?)))


(define-datatype listvalues listvalues?
  (listvalues-single
    (expr exp?))
  (listvalues-multi
    (expr exp?)
    (list-val listvalues?)))


(define-datatype listmem listmem?
  (listmem-single
    (expr exp?))
  (listmem-multi
    (expr exp?)
    (list-mem listmem?)))



(define simple-math-lexer
           (lexer
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUM (string->number lexeme)))
            ("+" (token-plus))
            (";" (token-semi))
            ("while" (token-while))
            ("do" (token-do))
            ("end" (token-end))
            ("=" (token-asgn))
            ("if" (token-if))
            ("then" (token-then))
            ("else" (token-else))
            ("endif" (token-endif))
            ("return" (token-rtn))
            (">" (token-greater))
            ("<" (token-less))
            ("==" (token-eq))
            ("!=" (token-noteq))
            ("-" (token-min))
            ("*" (token-mult))
            ("/" (token-div))
            ("(" (token-openp))
            (")" (token-closep))
            ("[" (token-openb))
            ("]" (token-closeb))
            ("," (token-and))
            ("true" (token-true))
            ("false" (token-false))
            ("null" (token-null))
            ((:: "\"" (:* (:~ "\"")) "\"") (token-string (substring lexeme 1 (- (string-length lexeme) 1))))
            ((:+ alphabetic) (token-id (string->symbol lexeme)))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (NUM string id))
(define-empty-tokens b (EOF plus semi while do end asgn if then else endif rtn greater less eq noteq min mult div openp closep openb closeb and true false null))


(define simple-math-parser
           (parser
            (start command)
            (end EOF)
            (error void)
            (tokens a b)
            (grammar
             (command ((unitcom) (command-single $1)) ((command semi unitcom) (command-multi $1 $3)))
             (unitcom ((whilecom) (unitcom-while $1)) ((ifcom) (unitcom-if $1)) ((assign) (unitcom-assign $1)) ((return) (unitcom-return $1)))
             (whilecom ((while exp do command end) (while-cmd $2 $4)))
             (assign ((id asgn exp) (assign-cmd $1 $3)))
             (ifcom ((if exp then command else command endif) (if-cmd $2 $4 $6)))
             (return ((rtn exp) (return-cmd $2)))
             (exp ((aexp) (exp-aexp $1)) ((aexp greater aexp) (exp-bigger $1 $3)) ((aexp less aexp) (exp-smaller $1 $3)) ((aexp eq aexp) (exp-equal $1 $3)) ((aexp noteq aexp) (exp-not-equal $1 $3)))
             (aexp ((bexp) (aexp-bexp $1)) ((bexp min aexp) (aexp-minus $1 $3)) ((bexp plus aexp) (aexp-plus $1 $3)))
             (bexp ((cexp) (bexp-cexp $1)) ((cexp mult bexp) (bexp-mult $1 $3)) ((cexp div bexp) (bexp-div $1 $3)))
             (cexp ((min cexp) (cexp-comp $2)) ((openp exp closep) (cexp-par $2)) ((NUM) (cexp-num  $1)) ((null) (cexp-null null)) ((id) (cexp-var  $1)) ((true) (cexp-bool #t)) ((false) (cexp-bool #f)) ((string) (cexp-string $1)) ((lst) (cexp-list $1)) ((id listmem) (cexp-listmem  $1 $2)))
             (lst ((openb listvalues closeb) (lst-non-empty $2)) ((openb closeb) (lst-empty)))
             (listvalues ((exp) (listvalues-single $1)) ((exp and listvalues) (listvalues-multi $1 $3)))
             (listmem ((openb exp closeb) (listmem-single $2)) ((openb exp closeb listmem) (listmem-multi $2 $4)))
             )))



