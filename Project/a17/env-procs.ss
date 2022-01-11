; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  	(lambda ()
    	(empty-env-record)))

(define extend-env
  	(lambda (syms vals env)
    	(extended-env-record syms (map box vals) env)))

(define list-find-position
  	(lambda (sym los)
    	(let loop ([los los] [pos 0])
      	(cond 
        	[(null? los) #f]
	    	[(eq? sym (car los)) pos]
	    	[else (loop (cdr los) (add1 pos))]))))
		 	 
(define apply-env
	(lambda (env var succeed fail)
		(unbox (apply-env-r env var succeed fail))))

(define apply-env-r
  	(lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    	(cases environment env
			[empty-env-record () (fail)]
			[extended-env-record (syms vals env)
				(let [(pos (list-find-position sym syms))]
					(if (number? pos)
						(succeed (list-ref vals pos)) 
						(apply-env-r env sym succeed fail)))]
			[recursively-extended-env-record (procnames idss bodies old-env)
				(let ([pos (list-find-position sym procnames)])
					(if (number? pos)
						(box (closure (list-ref idss pos) (list (list-ref bodies pos)) env))
						(apply-env-r old-env sym succeed fail)))])))
					
					

