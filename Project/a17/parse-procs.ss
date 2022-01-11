; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr) ; to make it even saner

(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(lit? datum) (lit-exp datum)]
     [(pair? datum)
      (if (not (list? datum))
		(eopl:error 'parse-exp "expression must be a proper list: ~s" datum)
		(cond 
			[(eqv? (1st datum) 'set!)	; SET!
	   		(cond
	    		((not (equal? (length datum) 3)) (eopl:error 'parse-exp "Set! must have two arguments ~s" datum))
	    		((not (symbol? (2nd datum))) (eopl:error 'parse-exp "First argument must be a symbol in set! ~s" datum))
	    		(else (set!-exp (2nd datum) (parse-exp (3rd datum)))))]
			
			[(eqv? (1st datum) 'define)	; DEFINE 
				(cond 
		 			((not (equal? (length datum) 3)) (eopl:error 'parse-exp "Define must have two arguments ~s" datum))
					(else (define-exp (2nd datum) (parse-exp (3rd datum)))))]
			
			[(eqv? (1st datum) 'lambda)	; LAMBDA
				(if (not (null? (cddr datum)))
					(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
 					(eopl:error 'parse-exp "too small lambda expression length: ~s" datum))]
			
      		[(eqv? (1st datum) 'if)		; IF
				(cond 
         		 	((or (null? (cdr datum)) (null? (cddr datum))) (eopl:error 'parse-exp "too small if expression length: ~s" datum))
					((null? (cdddr datum)) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum))))
					((null? (cddddr datum)) (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum))))
					(else (eopl:error 'parse-exp "too big if expression length: ~s" datum)))]

			[(eqv? (1st datum) 'let)	; LET
				(if (symbol? (2nd datum)) 
					(namelet-exp (2nd datum) (map car (3rd datum)) (map parse-exp (map cadr (3rd datum))) (map parse-exp (cdddr datum)))
					(let-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum))))]
			
			[(eqv? (1st datum) 'let*)	; LET*
				(let*-exp (map car (2nd datum)) (map parse-exp (map cadr (2nd datum))) (map parse-exp (cddr datum)))]
			
			[(eqv? 'letrec (1st datum)) ; LETREC (letrec ((id (body)) (id2 (body2))...) (letrecBody))
			  	(letrec-exp (map car (2nd datum)) (map 2nd (map 2nd (2nd datum))) 
				  	(map parse-exp (map 3rd (map 2nd (2nd datum)))) (map parse-exp (cddr datum)))]
		 
      		[(eqv? (1st datum) 'quote)	; QUOTE
				(if (or (not (null? (cdr datum))) (null? (cddr datum)))
					(quoted-exp (2nd datum))
					(eopl:error 'parse-exp "Invalid arguments for quoted-exp: ~s" datum))]
			
			[(eqv? (1st datum) 'while)	; WHILE
				(while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]

			[(eqv? (1st datum) 'begin)	; BEGIN
				(begin-exp (map parse-exp (cdr datum)))]

			[(eqv? (1st datum) 'and)	; AND
				(and-exp (map parse-exp (cdr datum)))]

			[(eqv? (1st datum) 'or)		; OR
				(or-exp (map parse-exp (cdr datum)))]

			[(eqv? (1st datum) 'cond)	; COND
				(cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum))))]
			
			[(eqv? (1st datum) 'case)	; CASES
				(case-exp (parse-exp (2nd datum)) (map quoted-exp (map car (cddr datum))) (map parse-exp (map cadr (cddr datum))))]
			
			[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]))]
		[else (eopl:error 'parse-exp "terrible no good bad disgusting ugly unbearable expression: ~s" datum)]
	 	)
	)
)










