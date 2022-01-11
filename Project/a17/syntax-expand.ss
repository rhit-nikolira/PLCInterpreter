(define syntax-expand
  (lambda (expr)
		(cases expression expr
			[set!-exp (id value)
				(set!-exp id (syntax-expand value))]
			[lit-exp (datum) expr]
			[var-exp (id) expr]
			[lambda-exp (id bodies)
				(lambda-exp id (map syntax-expand bodies))]
			[if-exp (test true-body)
				(if-exp (syntax-expand test) (syntax-expand true-body))]
			[if-else-exp (test true-body false-body)
				(if-else-exp (syntax-expand test) (syntax-expand true-body) (syntax-expand false-body))]
			[let-exp (vars vals bodies)
				(app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand vals))]
			[quoted-exp (data) expr]
			[app-exp (rator rands)
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[and-exp (bodies)
				(let loop ((bodies bodies))
					(if (null? bodies)
						(lit-exp #t)
						(if (null? (cdr bodies))
							(syntax-expand (car bodies))
							(if-else-exp (syntax-expand (car bodies)) (loop (cdr bodies)) (lit-exp #f)))))]
			[or-exp (bodies)
				(let loop ((bodies bodies))
					(if (null? bodies)
						(lit-exp #f)
						(if (null? (cdr bodies))
							(syntax-expand (car bodies))
							(syntax-expand (let-exp (list 'TylerJ) (list (syntax-expand (car bodies)))
								(list (if-else-exp (var-exp 'TylerJ) (var-exp 'TylerJ) (loop (cdr bodies)))))))))]
			[begin-exp (bodies) (app-exp (lambda-exp '() (map syntax-expand bodies)) '())]
			[let*-exp (vars vals bodies) 
				(car (let loop ([vars vars] [vals vals])
					(if (null? vars)
						(map syntax-expand bodies)
						(list (app-exp (lambda-exp (list (car vars)) (loop (cdr vars) (cdr vals)))  
					(list(syntax-expand (car vals))))))))]
			[letrec-exp (proc-names idss bodies letrec-bodies)
				(letrec-exp proc-names idss (map syntax-expand bodies) (map syntax-expand letrec-bodies))]
			[namelet-exp (name vars vals bodies)
				(syntax-expand (letrec-exp (list name) (list vars) bodies (list (app-exp (var-exp name) vals))))]
			[while-exp (test bodies)
				(while-exp (syntax-expand test) (map syntax-expand bodies))]
			[cond-exp (tests bodies)
				(let loop ([tests tests] [bodies bodies])
					(if (null? tests) 
						(app-exp (var-exp 'void) '())
						(if (eqv? (cadr (car tests)) 'else)
							(syntax-expand (car bodies))
							(if-else-exp (syntax-expand (car tests)) (syntax-expand (car bodies)) (loop (cdr tests) (cdr bodies))))))]
			[case-exp (expr keys bodies)
				(syntax-expand (cond-exp (case-expand-keys expr keys) (case-expand-bodies keys bodies)))]
			[define-exp (var dexpr)
				(define-exp var (syntax-expand dexpr))]
			[else (eopl:error 'syntax-expand "Bad abstract syntax: ~a" expr)])))
		

(define case-expand-keys
	(lambda (expr keys)
		(apply append
			(map (lambda (ls) 
				(if (list? (cadr ls)) 
					(map (lambda (key) (app-exp (var-exp 'eq?) (list (parse-exp key) expr))) (cadr ls))
					(list ls)))
			keys))))
		
(define case-expand-bodies
	(lambda (keys bodies)
		(apply append
			(map (lambda (key-list body) 
				(if (list? (cadr key-list)) 
					(map (lambda (x) body) (cadr key-list))
					(list body)))
			keys bodies))))