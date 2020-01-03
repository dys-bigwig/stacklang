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
				 unq
				 dot
				 cur)

(define (stack-compose . fs)
	(define (to-fn x)
		(match x
			[(? procedure?) x]
			[_ (push x)]))
	(foldl compose identity (map to-fn fs)))

(define (push x) (λ (s) (cons x s)))

(define plus (λ (s) (cons (rack+ (first s) (second s))
													(drop s 2))))

(define-syntax-rule (stack-module-begin body ...)
  (#%plain-module-begin
	 	((stack-compose body ...) empty)))

(define-syntax (number stx)
	(syntax-case stx ()
		[(_ n) #'n]))

(define-syntax (identifier stx)
	(syntax-case stx ()
		[(_ id) #'id]))

(define-syntax (quotation stx)
	(syntax-case stx ()
		[(quotation es ...)
		 #'(push (stack-compose es ...))]))

(define unq
	(λ (s) ((car s) (drop s 1))))

(define dot
	(λ (s) (begin (display (car s)))
		 		 (cdr s)))

(define cur
	(λ (s)
		 (define qfs (car s))
		 (define v (cadr s))
		 (define nq (stack-compose v qfs))
		 (cons nq (drop s 2))))
