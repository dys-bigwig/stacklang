#lang racket

(require parser-tools/lex
				 (prefix-in : parser-tools/lex-sre))

(provide stack-lex)

(define-tokens stack
	[IDENTIFIER NUMBER])
(define-empty-tokens stack*
	[OPEN-BRACE CLOSE-BRACE OPEN-PAREN CLOSE-PAREN WHITESPACE EOF ]) 

(define-lex-abbrevs
	[space (:or #\newline whitespace blank)]
	[identifier (:: alphabetic (:* (:or alphabetic numeric)))])

(define stack-lexer
	(lexer-src-pos
		[#\< (token-OPEN-BRACE)]
		[#\> (token-CLOSE-BRACE)]
		[#\( (token-OPEN-PAREN)]
		[#\) (token-CLOSE-PAREN)]
		[identifier (token-IDENTIFIER lexeme)]
		[numeric (token-NUMBER lexeme)]
		[(:+ space) (token-WHITESPACE)]
		[(eof) (token-EOF)]))

(define (flatten-token token)
  (cond
    [(position-token? token) (flatten-token (position-token-token token))]
		[else token]))

(define (stack-lex in)
	(for/list ([t (in-producer (λ () (stack-lexer in)))]
						 #:unless (eq? (token-name (flatten-token t)) 'WHITESPACE)
						 #:final (eq? (token-name (flatten-token t)) 'EOF))
		t))
