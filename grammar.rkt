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
)






