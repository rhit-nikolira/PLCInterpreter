;Author Ryan Nikolic and Sam Stiebith

;1
(define-datatype expression expression?
	[set!-exp
		(first symbol?)
		(second expression?)]
	[define-exp 
		(name symbol?)
		(body expression?)]
	[var-exp        
		(id symbol?)]
	[lit-exp
		(datum lit?)]
	[lambda-exp 
		(id (lambda (x) (or (symbol? x) (null? x) (pair? x))))
		(body (list-of expression?))]
	[if-exp
		(test expression?)
		(true-body expression?)]
	[if-else-exp
		(test expression?)
		(true-body expression?)
		(false-body expression?)]
	[let-exp
		(ids (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))]
	[letrec-exp
		(proc-names (list-of symbol?))
		(idss (list-of (list-of symbol?)))
		(bodies (list-of expression?))
		(letrec-bodies (list-of expression?))]
	[let*-exp
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))]
	[namelet-exp
		(name symbol?)
		(vars (list-of symbol?))
		(vals (list-of expression?))
		(bodies (list-of expression?))]
	[quoted-exp
		(data scheme-value?)]
	[app-exp
		(rator expression?)
		(rands (list-of expression?))]
	[cond-exp
		(test (list-of expression?))
		(body (list-of expression?))]
	[cases-exp
		(expr expression?)
		(keys (list-of symbol?))
		(bodies (list-of expression?))]
	[while-exp
		(test expression?)
		(bodies (list-of expression?))]
	[begin-exp
		(bodies (list-of expression?))]
	[or-exp
		(bodies (list-of expression?))]
	[and-exp
		(bodies (list-of expression?))]
	[case-exp
		(expr expression?)
		(keys (list-of expression?))
		(bodies (list-of expression?))]
)

(define lit?
	(lambda (x)
		(ormap (lambda (pred) (pred x)) (list number? vector? boolean? symbol? string? null?))))

(define scheme-value?
  (lambda (x) #t))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
	[prim-proc
		(name symbol?)]
   	[closure
		(params (lambda (x) (or (list-of symbol? x) (symbol? x) (contains? x))))
		(body (list-of expression?))
		(env environment?)])
		; (vars (list-of symbol?))
		; (bodies (list-of expression?))
		; (env environment?)])

(define contains?
  (lambda (syms)
    (if (null? syms)
	#t
	(if (pair? (cdr syms))
	    (if (symbol? (car syms))
		(contains-syms? (cdr syms))
		#f)
	    (if (symbol? (car syms))
		(symbol? (cdr syms))
		#f)))))	 
	 
(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(sym (lambda (x) (or (symbol? x) (null? x) (pair? x))))
		(vals (list-of scheme-value?))
		(env environment?)]
   [recursively-extended-env-record
		(proc-names (list-of symbol?))
		(ids (list-of (list-of symbol?)))
		(bodies (list-of expression?))
		(env environment?)])