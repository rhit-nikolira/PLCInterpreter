; Homework 17
; Authors: Sam Stieby and Ryan Nikolic

; Problem 1
(define top-level-eval
  (lambda (form)
    (eval-exp form (empty-env))))

(define eval-exp
  (lambda (expr env)
    (cases expression expr
		[set!-exp (id value)
			(set-box! (apply-env-r env id 
				(lambda (x) x)
				(lambda () (apply-env global-env id (lambda (x) x) 
					(lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id))))) 
			(eval-exp value env))]
		[lit-exp (datum) datum]
		[var-exp (id) (apply-env env id (lambda (x) x) 
			(lambda () (apply-env global-env id (lambda (x) x) (lambda () 
				(eopl:error 'apply-env "variable not found in environment: ~s" id)))))]
		[letrec-exp (proc ids bodies letrecBodies) 
			(let ([newEnv (makeRecursively proc ids bodies env)])
				(let loop ([bodies letrecBodies]) 
				(if (null? (cdr bodies)) 
					(eval-exp (1st bodies) newEnv)
					(begin (eval-exp (1st bodies) newEnv) (loop (cdr bodies))))))]
		[define-exp (var expr)
			(set! global-env (extend-env (list var) (list (box (top-level-eval expr))) global-env))]
		[lambda-exp (vars bodies)
			(closure vars bodies env)]
		[if-exp (test true-body)
			(if (eval-exp test env) (eval-exp true-body env))]
		[if-else-exp (test true-body false-body)
			(if (eval-exp test env)
				(eval-exp true-body env)
				(eval-exp false-body env))]
		[quoted-exp (data) data]
		[app-exp (rator rands)
			(let ([proc-value (eval-exp rator env)]
				[args (eval-rands rands env)])
			(apply-proc proc-value args))]
		[while-exp (test bodies)
			(let whileLoop ()
				(if (eval-exp test env) 
					(let innerLoop ([bodies bodies]) 
						(if (null? bodies)
							(whileLoop)	
							(begin (eval-exp (car bodies) env) (innerLoop (cdr bodies)))))))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" expr)])))

(define makeRecursively 
	(lambda (proc-names idss bodies old-env)
		(recursively-extended-env-record
			proc-names idss bodies old-env)))		 

(define eval-rands
  (lambda (rands env)
    (map (lambda (e) (eval-exp e env)) rands)))

;  Apply a procedure to its arguments.

(define apply-proc
  	(lambda (proc-value args)
    	(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[closure (params body env) (let ([args (get-syms-and-args params args)])
				(let ([extended-env (extend-env
					(if (null? args)
						'()
						(car args))
					(if (null? args)
						'()
						(cdr args)) 
					env)])
					(let loop ([bodies body])
						(if (null? (cdr bodies))
							(eval-exp (1st bodies) extended-env)
							(begin	(eval-exp (1st bodies) extended-env)
									(loop (cdr bodies)))))))]
			[else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)])))

(define get-syms-and-args
	(lambda (syms args)
		(cond
			[(null? syms) '()]
			[(list? syms)
				(cons syms args)]
			[(atom? syms)
				(cons (list syms) (list args))]
			[else
				(cons (get-syms syms) (get-args syms args))])))

(define get-syms
	(lambda (syms)
		(if (null? syms)
			'()
			(if (null? (cdr syms))
				syms
				(if (pair? (cdr syms))
					(cons (car syms) (get-syms (cdr syms)))
					(cons (car syms) (list (cdr syms))))))))

(define get-args
	(lambda (syms args)
		(if (null? args)
			'()
			(if (null? (cdr syms))
				args
				(if (pair? (cdr syms))
					(cons (car args) (get-args (cdr syms) (cdr args)))
					(cons (car args) (list (cdr args))))))))

(define *prim-proc-names*
	'(+ - * add1 sub1 cons = / < > <= >= zero? not car cdr list null? eq? equal? length
	list->vector list? pair? vector->list vector? number? symbol? set-car! set-cdr! 
	display newline caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cdaar cdddr 
	vector vector-ref vector-set! apply void map quotient negative? positive? procedure? 
	list-tail assq append))

(define init-env       	; for now, our initial global environment only contains
  (extend-env         	; procedure names.  Recall that an environment associates
     *prim-proc-names*	;  a value (not an expression) with an identifier.
     (map box (map prim-proc
          *prim-proc-names*))
		(empty-env)))
		
(define global-env init-env)

(define reset-global-env (lambda () (set! global-env init-env)))

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
	  [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
	  [(=) (= (1st args) (2nd args))]
	  [(<) (< (1st args) (2nd args))]
	  [(>) (> (1st args) (2nd args))]
	  [(<=) (<= (1st args) (2nd args))]
	  [(>=) (>= (1st args) (2nd args))]
	  [(zero?) (zero? (1st args))]
	  [(not) (not (1st args))]
      [(cons) (cons (1st args) (2nd args))]
	  [(car) (car (1st args))]
	  [(cdr) (cdr (1st args))]
	  [(list) args]
	  [(null?) (null? (1st args))]
	  [(assq) (assq (1st args) (2nd args))]
	  [(eq?) (eq? (1st args) (2nd args))]
	  [(eqv?) (eqv? (1st args) (2nd args))]
	  [(equal?) (equal? (1st args) (2nd args))]
	  [(atom?) (atom? (1st args))]
	  [(length) (length (1st args))]
	  [(list->vector) (list->vector (1st args))]
	  [(list-tail) (list-tail (1st args) (2nd args))]
	  [(list?) (list? (1st args))]
	  [(pair?) (pair? (1st args))]
	  [(procedure?) (proc-val? (1st args))]
	  [(vector->list) (vector->list (1st args))]
	  [(vector) (apply vector args)]
	  [(make-vector) (apply make-vector (args))]
	  [(vector-ref) (vector-ref (1st args) (2nd args))]
	  [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
	  [(vector?) (vector? (1st args))]
	  [(number?) (number? (1st args))]
	  [(symbol?) (symbol? (1st args))]
	  [(set-car!) (set-car! (1st args) (2nd args))]
	  [(set-cdr!) (set-cdr! (1st args) (2nd args))]
	  [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(caar) (caar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
	  [(quotient) (quotient (1st args) (2nd args))]
	  [(apply) (my-apply (1st args) (2nd args))]
	  [(map) (my-map (1st args) (2nd args))]
	  [(void) (void)]
	  [(display) (display (1st args))]
	  [(newline) (newline)]
	  [(append) (append (1st args) (2nd args))]

      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s"
            prim-proc)])))
			
(define my-apply
	(lambda (proc args)
		(cases proc-val proc
			[prim-proc (name) 
				(apply-prim-proc name args)]
			[closure (params procedure env) 
				(apply-proc proc args)]
			[else
				(error 'my-apply "D: ~s" proc)])))

(define my-map
	(lambda (proc args)
		(cases proc-val proc
			[prim-proc (name)
				(let loop ((args args))
					(if (null? args)
						'()
						(cons (apply-prim-proc name (list (1st args))) (loop (cdr args)))))]
			[closure (params body env)
				(let loop ((args args))
					(if (null? args)
						'()
						(cons (apply-proc proc (list (1st args))) (loop (cdr args)))))]
			[else
				(error 'my-map "D: ~s" proc)])))		
			
(define repeat
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (repeat))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
