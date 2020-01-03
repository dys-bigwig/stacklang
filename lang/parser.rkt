#lang racket

(require megaparsack
				 megaparsack/parser-tools/lex
				 data/applicative
				 data/maybe
				 data/monad
				 "lexer.rkt")

(provide stack-parse)

(struct quoted (es) #:prefab)

(define identifier/p
  (do [id <- ((pure string->symbol) (token/p 'IDENTIFIER))]
      (pure `(identifier ,id))))

(define number/p
  (do [n <- ((pure string->number) (token/p 'NUMBER))]
      (pure `(number ,n))))

(define eof/p
	(token/p 'EOF))

(define quotation/p
	(do
		(token/p 'OPEN-BRACE)
		[exprs <- (many/p expr/p)]
		(token/p 'CLOSE-BRACE)
		(pure `(quotation ,@(map quoted exprs)))))

(define expr/p
	(syntax/p (or/p quotation/p identifier/p number/p)))

(define program/p
	(do
		[exprs <- (many+/p (syntax/p expr/p))]
		eof/p
		(pure exprs)))

(define (stack-parse in [source-name (object-name in)])
	(parse-tokens program/p (stack-lex in) source-name))

(define (test str)
	(parse-result! (stack-parse (open-input-string str))))
