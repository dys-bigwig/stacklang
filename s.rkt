#lang racket

(define (add s)
	(match-define-values (`(,x ,y . ,zs)) (values s))
	`(,(+ x y) . ,zs))

(define (stack-compose fs)
	(foldl compose identity fs))

(define (num n)
	(λ (s) (cons n s)))

(define (push x)
	(curry cons x))

(define (quot . es)
	(push (stack-compose es)))

(define (i s)
	(define q (car s))
	(q (cdr s)))

(define (go . fs)
	((stack-compose fs) '()))

(go (num 1)
		(quot (num 1) (quot add))
		i
		i)
