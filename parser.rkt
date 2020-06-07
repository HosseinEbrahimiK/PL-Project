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
             (unitcom ((assign) (list 'assigncom $1)) ((return) (list 'returncom $1)))
             (assign ((id asgn NUM) (list 'assign $1 $3)))
             )))


(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "while 2 do return 1; end")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)
