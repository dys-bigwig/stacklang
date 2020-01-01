#lang s-exp syntax/module-reader
stack/language

#:read stack-read
#:read-syntax stack-read-syntax
#:whole-body-readers? #t

(require "parser.rkt"
				 "lexer.rkt"
				 megaparsack
				 racket)

(define (stack-read-syntax module-name in)
	(parse-result! (stack-parse in)))

(define (stack-read in)
  (syntax->datum (stack-read-syntax in #f)))
