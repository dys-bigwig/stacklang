#lang racket

(require megaparsack
				 megaparsack/parser-tools/lex
				 data/applicative
				 data/maybe
				 data/monad
				 "lexer.rkt")

(provide stack-parse)

(define (wrap name e)
	(cond
		[(list? e) `(,(string->symbol name) ,@e)]
		[else `(,(string->symbol name) ,e)]))

(define identifier/p
  (do [id <- (token/p 'IDENTIFIER)]
      (pure (wrap "identifier" id))))

(define number/p
  (do [n <- ((pure string->number) (token/p 'NUMBER))]
      (pure (wrap "number" n))))

(define eof/p
	(token/p 'EOF))

(define quotation/p
	(do
		(token/p 'OPEN-BRACE)
		[exprs <- (many/p expr/p)]
		(token/p 'CLOSE-BRACE)
		(pure `(quotation ,@exprs))))

(define expr/p
	(syntax/p (or/p quotation/p identifier/p number/p)))

(define program/p
	(do
		[exprs <- (many+/p (syntax/p expr/p))]
		eof/p
		(pure exprs)))

(define (stack-parse in [source-name (object-name in)])
	(parse-tokens program/p (stack-lex in) source-name))
