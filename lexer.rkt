#lang racket

(require parser-tools/lex
				 (prefix-in : parser-tools/lex-sre))

(provide stack-lex)

(define-tokens stack
	[IDENTIFIER NUMBER PRIMOP])
(define-empty-tokens stack*
	[OPEN-BRACE CLOSE-BRACE OPEN-PAREN CLOSE-PAREN EOF])

(define-lex-abbrevs
	[space (:& (:~ #\newline) (:or whitespace blank))]
	[identifier (:: alphabetic (:* (:or alphabetic numeric)))])

(define stack-lexer
	(lexer-src-pos
		[#\[ (token-OPEN-BRACE)]
		[#\] (token-CLOSE-BRACE)]
		[#\( (token-OPEN-PAREN)]
		[#\) (token-CLOSE-PAREN)]
		[identifier (token-IDENTIFIER lexeme)]
		[numeric (token-NUMBER lexeme)]
		[(:+ space) (void)]
		[(eof) (token-EOF)]))

(define (flatten-token token)
  (cond
    [(position-token? token) (flatten-token (position-token-token token))]
		[else token]))

(define (stack-lex in)
	(for/list ([t (in-producer (λ () (stack-lexer in)))]
						 #:unless (void? (position-token-token t))
						 #:final (equal? (token-name (flatten-token t)) 'EOF))
		t))
