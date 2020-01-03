#lang racket/base

(require (except-in racket + -)
         (rename-in racket [+ rack+] [- rack-]))

(provide (rename-out [stack-module-begin #%module-begin])
				 #%top
				 #%app
				 #%datum
				 number
				 quotation
				 plus
				 identifier
				 unq)

(define (stack-compose . fs)
	(foldl compose identity fs))

(define (push x) (λ (s) (cons x s)))

(define plus (λ (s) (cons (rack+ (first s) (second s))
													(drop s 2))))

(define primops (hash 'plus +))

(define-syntax-rule (stack-module-begin body ...)
  (#%plain-module-begin
	 	(display ((stack-compose body ...) empty))))

(define-syntax (number stx)
	(syntax-case stx ()
		[(_ n) #'(λ (s) (cons n s))]))

(define-syntax (identifier stx)
	(syntax-case stx ()
		[(_ id) #'(λ (s) (id s))]))

(define-syntax (quotation stx)
	(syntax-case stx ()
		[(quotation #s(quoted es) ...) #'(push (stack-compose es ...))]))

(define unq
	(λ (s)
		 ((car s) (drop s 1))))
