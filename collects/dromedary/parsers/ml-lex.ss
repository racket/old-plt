#cs
(module ml-lex mzscheme
	(require (lib "lex.ss" "parser-tools")
		 (lib "readerr.ss" "syntax"))
	(provide Keywords Labels Ops ConstantConstructors Literals Errors Others Precedences ml-lex)

	   ;; Taken from the OCaml System release 3.04 Documentation and user's manual
	   ;; Also converted from the parsing subdirectory of the Ocaml 3.04 distribution
	   (define-lex-abbrevs [letter (: (- #\a #\z) (- #\A #\Z))]
                         [digit (- #\0 #\9)]
                         [lowercase (: (- #\a #\z) (- #\337 #\366) (- #\370 #\377) #\_ )]
			 [uppercase (: (- #\A #\Z) (- #\300 #\326) (- #\330 #\336))]


			 ;; From 6.1 Lexical conventions
			 [blank (: #\newline #\return #\tab #\page #\space )]
			 [comment (@ "(*" (* (^ "*)")) "*)")]
			 [identchar (: lowercase uppercase #\' digit )]
			 [symbolchar (: #\! #\$ #\% #\& #\* #\+ #\- #\. #\/ #\: #\< #\= #\> #\? #\@ #\^ #\| #\~ )]
			 [decimal_literal (+ digit)]
			 [hex_literal (@ #\0 (: #\x #\X) (+ (: digit (- #\A #\F) (- #\a #\f))) )]
			 [oct_literal (@ #\0 (: #\o #\O) (+ (- #\0 #\7)))]
			 [bin_literal (@ #\0 (: #\b #\B) (+ (- #\0 #\1)))]
			 [float_literal (@ (+ digit) (? (@ #\. (* digit))) (? (@ (: #\e #\E) (? (: #\+ #\-)) (+ digit))))]

			 [escape_sequence (: "\\\\" "\\\"" "\\n" "\\r" "\\t" "\\b" (@ #\\ digit digit digit))]
			 [newln (: #\newline #\return (@ #\return #\newline))]
)
 

; Should be empty
     (define-tokens Keywords
       (AND AS ASSERT ASR BEGIN CLASS
	    CLOSED CONSTRAINT DO DONE DOWNTO ELSE
	    END EXCEPTION EXTERNAL FOR FUN
	    FUNCTION FUNCTOR IF IN INCLUDE INHERIT
	    LAND LAZY LET LOR LSL LSR
	    LXOR MATCH METHOD MOD MODULE MUTABLE
	    NEW OF OPEN OR PARSER PRIVATE 
	    REC SIG STRUCT THEN TO
	    TRY TYPE VAL VIRTUAL WHEN WHILE
	    WITH ))
     
     

     (define-tokens Labels
       (LABEL LIDENT OPTLABEL UIDENT))
     
     (define-tokens Ops
       (PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4))
     
     ;; From 6.2.5
; Should be empty
     (define-tokens ConstantConstructors
       (TRUE FALSE UNIT EMPTY))
     
     ;; From 6.5
     (define-tokens Literals
       (CHAR FLOAT INT STRING))

; Should be empty
     (define-tokens Errors
       (UNPARSEABLE ERRORKEYWORDASLABEL))

; Should be empty
     (define-tokens Others
       (SHARP AMPERSAND AMPERAMPER BACKQUOTE QUOTE LPAREN RPAREN STAR COMMA 
	      MINUSGREATER DOT DOTDOT COLON COLONCOLON COLONEQUAL COLONGREATER
	      SEMI SEMISEMI LESS LESSMINUS EQUAL LBRACKET LBRACKETBAR 
	      LBRACKETLESS RBRACKET LBRACE LBRACELESS BAR BARBAR BARRBRACKET
	      GREATER GREATERRBRACKET RBRACE GREATERRBRACE PLUS MINUS MINUSDOT
	      UNDERSCORE TILDE QUESTION
	      EOF))

; Should be empty
     (define-empty-tokens Precedences
       (<prec_let> <prec_type_def> <prec_fun> <prec_match> <prec_try>
		   <prec_list> <prec_if> <prec_type_arrow> <prec_unary_minus>
		   <prec_appl> <prec_constr_appl>))
       
     (define stx-for-original-property (read-syntax #f (open-input-string "original")))

     (define-syntax (token stx)
       (syntax-case stx ()
         [(_ name val)
          (identifier? (syntax name))
          (let ([name (syntax name)])
            (with-syntax ([token-name (datum->syntax-object
                                       name
                                       (string->symbol
                                        (format "token-~a" (syntax-e name))))]
                          [source-name (datum->syntax-object name 'source-name)]
                          [start-pos (datum->syntax-object name 'start-pos)]
                          [end-pos (datum->syntax-object name 'end-pos)])
              (syntax (let ([start start-pos]
                            [end end-pos])
                        (token-name 
			 (datum->syntax-object #f val
					       (list
						source-name
						(position-line start)
						(position-col start)
						(position-offset start)
						(- (position-offset end)
						   (position-offset start)))
					       stx-for-original-property))))))]))

     (define-syntax (delay-token stx)
       (syntax-case stx ()
         [(_ name val)
          (identifier? (syntax (eval name)))
          (let ([name (syntax name)])
            (with-syntax ([source-name (datum->syntax-object name 'source-name)]
                          [start-pos (datum->syntax-object name 'start-pos)]
                          [end-pos (datum->syntax-object name 'end-pos)])
              (syntax (let ([start start-pos]
                            [end end-pos])
                        ((eval (datum->syntax-object #f (string->symbol (format "token-~a" name))))
			 (datum->syntax-object #f val
					       (list
						source-name
						(position-line start)
						(position-col start)
						(position-offset start)
						(- (position-offset end)
						   (position-offset start)))
					       stx-for-original-property))))))]))


     (define-syntax (delay-ttoken stx)
       (syntax-case stx ()
		    [(_ name)
		     (syntax (delay-token name 'name))]))

     (define-syntax (ttoken stx)
       (syntax-case stx ()
		    [(_ name)
		     (identifier? (syntax name))
		     (syntax (token name 'name))]))

     (define keyword-table (make-hash-table 'equal))
     (hash-table-put! keyword-table "and" token-AND)
     (hash-table-put! keyword-table "as" token-AS)
     (hash-table-put! keyword-table "assert" token-ASSERT)
     (hash-table-put! keyword-table "begin" token-BEGIN)
     (hash-table-put! keyword-table "class" token-CLASS)
     (hash-table-put! keyword-table "constraint" token-CONSTRAINT)
     (hash-table-put! keyword-table "do" token-DO)
     (hash-table-put! keyword-table "done" token-DONE)
     (hash-table-put! keyword-table "downto" token-DOWNTO)
     (hash-table-put! keyword-table "else" token-ELSE)
     (hash-table-put! keyword-table "end" token-END)
     (hash-table-put! keyword-table "exception" token-EXCEPTION)
     (hash-table-put! keyword-table "external" token-EXTERNAL)
     (hash-table-put! keyword-table "false" token-FALSE)
     (hash-table-put! keyword-table "for" token-FOR)
     (hash-table-put! keyword-table "fun" token-FUN)
     (hash-table-put! keyword-table "function" token-FUNCTION)
     (hash-table-put! keyword-table "functor" token-FUNCTOR)
     (hash-table-put! keyword-table "if" token-IF)
     (hash-table-put! keyword-table "in" token-IN)
     (hash-table-put! keyword-table "include" token-INCLUDE)
     (hash-table-put! keyword-table "inherit" token-INHERIT)
;	   (hash-table-put! keyword-table "initializer" token-INITIALIZER)
     (hash-table-put! keyword-table "lazy" token-LAZY)
     (hash-table-put! keyword-table "let" token-LET)
     (hash-table-put! keyword-table "match" token-MATCH)
     (hash-table-put! keyword-table "method" token-METHOD)
     (hash-table-put! keyword-table "module" token-MODULE)
     (hash-table-put! keyword-table "new" token-NEW)
;	   (hash-table-put! keyword-table "object" token-OBJECT)
     (hash-table-put! keyword-table "of" token-OF)
     (hash-table-put! keyword-table "open" token-OPEN)
     (hash-table-put! keyword-table "or" token-OR)
     (hash-table-put! keyword-table "private" token-PRIVATE)
     (hash-table-put! keyword-table "rec" token-REC)
     (hash-table-put! keyword-table "sig" token-SIG)
     (hash-table-put! keyword-table "struct" token-STRUCT)
     (hash-table-put! keyword-table "then" token-THEN)
     (hash-table-put! keyword-table "to" token-TO)
     (hash-table-put! keyword-table "true" token-TRUE) 
     (hash-table-put! keyword-table "try" token-TRY)
     (hash-table-put! keyword-table "type" token-TYPE)
     (hash-table-put! keyword-table "val" token-VAL)
     (hash-table-put! keyword-table "virtual" token-VIRTUAL)
     (hash-table-put! keyword-table "when" token-WHEN)
     (hash-table-put! keyword-table "while" token-WHILE)
     (hash-table-put! keyword-table "with" token-WITH)
     (hash-table-put! keyword-table "mod" (cons token-INFIXOP3 "mod"))
     (hash-table-put! keyword-table "land" (cons token-INFIXOP3 "land"))
     (hash-table-put! keyword-table "lor" (cons token-INFIXOP3 "lor"))
     (hash-table-put! keyword-table "lxor" (cons token-INFIXOP3 "lxor"))
     (hash-table-put! keyword-table "lsl" (cons token-INFIXOP4 "lsl"))
     (hash-table-put! keyword-table "lsr" (cons token-INFIXOP4 "lsr"))
     (hash-table-put! keyword-table "asr" (cons token-INFIXOP4 "asr"))

     (define (ml-lex source-name)
       (lexer-src-pos
	[(+ blank) (void)]
	["_" (ttoken UNDERSCORE)]
	["~" (ttoken TILDE)]
	[(@ "~" lowercase (* identchar) ":") (let* ([s lexeme]
						    [name (substring s 1 (sub1 (string-length s)))])
					       (if (hash-table-get keyword-table name (lambda () #f))
						   (token ERRORKEYWORDASLABEL (string->symbol s))
						   (token LABEL (string->symbol s))))]
	["?" (ttoken QUESTION)]
	[(@ "?" lowercase (* identchar) ":") (let* ([s lexeme]
						    [name (substring s 1 (sub1 (string-length s)))])
					       (if (hash-table-get keyword-table name (lambda () #f))
						   (token ERRORKEYWORDASLABEL (string->symbol s))
						   (token OPTLABEL (string->symbol s))))]
	[(@ lowercase (* identchar)) (let* ([s lexeme]
						[name (substring s 0 (string-length s))])
					   (if (hash-table-get keyword-table name (lambda () #f))
					       (let ([result (hash-table-get keyword-table name)])
						 (let ([stx-list
							(list
							 source-name
							 (position-line start-pos)
							 (position-col start-pos)
							 (position-offset start-pos)
							 (- (position-offset end-pos)
							    (position-offset start-pos)))])
						   (if (pair? result)
						       ((car result) (datum->syntax-object #f (cdr result) stx-list stx-for-original-property))
						       (result (datum->syntax-object #f lexeme stx-list stx-for-original-property)))))
					       (token LIDENT s)))]
	
	;; No capitalized keywords
	[(@ uppercase (* identchar)) (token UIDENT lexeme)]
	[(: decimal_literal hex_literal oct_literal bin_literal) 
								   (token INT (string->number lexeme))]
	[float_literal (token FLOAT (string->number lexeme))]
	[#\" (token STRING (list->string (get-string input-port)))]
	[(@ #\' (: #\^ #\\ #\') #\') 
	 (token CHAR (string-ref lexeme 1))]
	[(@ #\' #\\ (: #\\ #\' #\n #\t #\b #\r) #\')
	 (token CHAR (let ([s lexeme])
		       (escape_sequence->char (string-ref lexeme 2))))]
	[(@ #\' #\\ digit digit digit #\')
	 (token CHAR (let ([s lexeme])
		       (integer->char (string->number (substring s 2 5)))))]
	[(@ #\' (^ (: #\^ #\\ #\' #\\ )) #\')
	 (token CHAR (string-ref lexeme 1))]
	;; Comment
	[(@ #\( "*")
	 (begin
	   ;; Skip comment, handling nested comments:
	   (let nested-loop ()
	     (let loop ()
	       (let ([m (regexp-match #rx"([(][*])|([*][)])" input-port)])
		 (cond
		  [(not m) (raise-read-error "unexpected EOF while parsng comment" 
					     source-name 
					     (position-line start-pos)
					     (position-col start-pos)
					     (position-offset start-pos)
					     (- (position-offset end-pos)
						(position-offset start-pos)))]
		  [(string=? (car m) "(*") 
		   (nested-loop)
		   (loop)]
		  [else (void)]))))
	   (return-without-pos ((ml-lex source-name) input-port)))]
	;; Character sequence keywords
	[#\# (ttoken SHARP)]
	["&" (ttoken AMPERSAND)]
	["&&" (ttoken AMPERAMPER)]
	[#\` (ttoken BACKQUOTE)]
	[#\' (ttoken QUOTE)]
	[#\( (ttoken LPAREN)]
	[#\) (ttoken RPAREN)]
	["*"  (ttoken STAR )]
	[#\,  (ttoken COMMA )]
	["->" (ttoken MINUSGREATER )]
	[#\.  (ttoken DOT )]
	[".." (ttoken DOTDOT )]
	[":"  (ttoken COLON )]
	["::" (ttoken COLONCOLON )]
	[":=" (ttoken COLONEQUAL )]
	[":>" (ttoken COLONGREATER )]
	[#\;  (ttoken SEMI )]
	[(@ #\; #\;) (ttoken SEMISEMI )]
	["<"  (ttoken LESS )]
	["<-" (ttoken LESSMINUS )]
	["="  (ttoken EQUAL )]
	[#\[  (ttoken LBRACKET )]
	[(@ #\[ #\|) (ttoken LBRACKETBAR )]
	[(@ #\[ "<") (ttoken LBRACKETLESS )]
	[#\]  (ttoken RBRACKET )]
	[#\{  (ttoken LBRACE )]
	[(@ #\{ "<") (ttoken LBRACELESS )]
	[#\|  (ttoken BAR )]
	[(@ #\| #\|) (ttoken BARBAR )]
	[(@ #\| #\]) (ttoken BARRBRACKET )]
	[">"  (ttoken GREATER )]
	[(@ ">" #\]) (ttoken GREATERRBRACKET )]
	[#\}  (ttoken RBRACE )]
	[(@ ">" #\}) (ttoken GREATERRBRACE )]
	["!=" (token INFIXOP0 "!=")]
	["+" (ttoken PLUS)]
	["-" (ttoken MINUS)]
	["-." (ttoken MINUSDOT)]
	[(@ #\! (* symbolchar)) (token PREFIXOP lexeme)]
	[(@ (: #\~ #\?) (+ symbolchar)) (token PREFIXOP lexeme)]
	[(@ (: #\= #\< #\> #\| #\& #\$) (* symbolchar)) (token INFIXOP0 lexeme)]
	[(@ (: #\@ #\^) (* symbolchar)) (token INFIXOP1 lexeme)]
	[(@ (: #\+ #\-) (* symbolchar)) (token INFIXOP2 lexeme)]
	[(@ #\* #\* (* symbolchar)) (token INFIXOP4 lexeme)]
	[(@ (: #\* #\/ #\%) (* symbolchar) ) (token INFIXOP3 lexeme)]
	;; The rest
	[(eof) (ttoken EOF)]
	[(- #\000 #\377) (token UNPARSEABLE (string->symbol lexeme))]))

(define get-string
  (lexer
   (#\" null)
   (escape_sequence (cons (escape_sequence->char lexeme)
			   (get-string input-port)))
   ((@ #\\ newln (* blank)) (get-string input-port))
   ((- #\000 #\377) (cons (string-ref lexeme 0)
			   (get-string input-port)))))

(define (escape_sequence->char es)
  (cond
   ((char=? es #\\) #\\)
   ((char=? es #\') #\')
   ((char=? es #\") #\")
   ((char=? es #\b) #\010)
   ((char=? es #\t) #\011)
   ((char=? es #\n) #\012)
   ((char=? es #\r) #\015)))

)