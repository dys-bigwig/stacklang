#lang s-exp syntax/module-reader
stack/language

#:read stack-read
#:read-syntax stack-read-syntax
#:whole-body-readers? #t

(require "parser.rkt"
				 "lexer.rkt"
				 megaparsack)

(define (stack-read-syntax module-name in)
	(define src-datums (parse-result! (stack-parse in)))
	(datum->syntax #f `(compose ,@(reverse src-datums))))

(define (stack-read in)
  (syntax->datum (stack-read-syntax #f in)))
