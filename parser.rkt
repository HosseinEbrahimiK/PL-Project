#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(define program "a=2;")

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
            ("!=" '(token-noteq))
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
            ((:: "\"" any-string "\"") (token-string lexeme))
            ((:+ alphabetic) (token-id lexeme))
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
             (command ((unitcom) (list 'unitcom $1)) ((command semi unitcom) (list 'multicom $1 $3)))
             (unitcom ((whilecom) (list 'whilecom $1)) ((ifcom) (list 'ifcom $1)) ((assign) (list 'assigncom $1)) ((return) (list 'returncom $1)))
             (whilecom ((while exp do command end) (list 'while $2 $4)))
             (assign ((id asgn exp) (list 'assign $1 $3)))
             (ifcom ((if exp then command else command endif) (list 'if $2 $4 $6)))
             (return ((rtn exp) (list 'return $2)))
             (exp ((aexp) (list 'aexp $1)) ((aexp greater aexp) (list 'more? $1 $3)) ((aexp less aexp) (list 'less? $1 $3)) ((aexp eq aexp) (list 'equal? $1 $3)) ((aexp noteq aexp) (list 'not-equal? $1 $3)))
             (aexp ((bexp) (list 'bexp $1)) ((bexp min aexp) (list 'minus $1 $3)) ((bexp plus aexp) (list 'plus $1 $3)))
             (bexp ((cexp) (list 'cexp $1)) ((cexp mult bexp) (list 'mult $1 $3)) ((cexp div bexp) (list 'divide $1 $3)))
             (cexp ((min cexp) (list 'comp $2)) ((openp exp closep) (list 'par $2)) ((NUM) (list 'number $1)) ((null) (list 'null)) ((id) (list 'var $1)) ((true) (list 'true)) ((false) (list 'false)) ((string) (list 'string $1)) ((lst) (list 'lst '$1)) ((id listmem) (list 'listmem $1 $2)))
             (lst ((openb listvalues closeb) (list 'listvals $2)) ((openb closeb) (list 'null-list)))
             (listvalues ((exp) (list 'single-val $1)) ((exp and listvalues) (list 'multival $1 $3)))
             (listmem ((openb exp closeb) (list 'single-mem $2)) ((openb exp closeb listmem) (list 'multimem $2 $4)))
             )))


(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "while a < 5 do a = a - 1 end")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)