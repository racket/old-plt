#cs
(module lexer mzscheme

  ;; Lexical Analysis according to the Java Language Specification First Edition 
  ;; chapter 3.
  ;; Lacks all Unicode support

  
  (require (lib "lex.ss" "parser-tools"))

  (provide Operators Separators EmptyLiterals Keywords java-vals get-token get-syntax-token)
  
  (define-empty-tokens Operators
    (PIPE OR OREQUAL
     =	> < !	~	?	:
     ==	<=	>=	!=	&&	++	--
     +	-	*	/	&	^	%	<< >> >>>
     +=	-=	*=	/=	&=	^=	%=	<<=	>>=	>>>=))

  (define-empty-tokens Separators
   (O_PAREN C_PAREN O_BRACE C_BRACE O_BRACKET C_BRACKET SEMI_COLON PERIOD COMMA))
  
  (define-empty-tokens EmptyLiterals
   (NULL_LIT TRUE_LIT FALSE_LIT EOF))
  
  (define-empty-tokens Keywords
   (abstract    default    if            private      this
    boolean     do         implements    protected    throw
    break       double     import        public       throws
    byte        else       instanceof    return       transient
    case        extends    int           short        try
    catch       final      interface     static       void
    char        finally    long          strictfp     volatile
    class       float      native        super        while
    const       for        new           switch
    continue    goto       package       synchronized))

  (define-tokens java-vals
    (STRING_LIT CHAR_LIT INTEGER_LIT LONG_LIT FLOAT_LIT DOUBLE_LIT IDENTIFIER STRING_ERROR))  

  (define (trim-string s f l)
    (substring s f (- (string-length s) l)))


  (define-lex-abbrevs
    ;; 3.4
    (CR #\015)
    (LF #\012)
    (LineTerminator (: CR 
		       LF 
		       (@ CR LF)))
    (InputCharacter (^ CR LF))
    
    ;; 3.6
    (FF #\014)
    (TAB #\011)
    (WhiteSpace (: #\space 
		   TAB
		   FF
		   LineTerminator))

    ;; 3.7 (Had to transform CommentTail and CommentTailStar into one RE)
    ;;     (DocumentationComment only appears in version 1 of the spec)
    (Comment (: TraditionalComment 
		EndOfLineComment
                DocumentationComment))
    (TraditionalComment (@ "/*" NotStar CommentTail))
    (EndOfLineComment (@ "//" (* InputCharacter) (: (eof) LineTerminator)))
    (DocumentationComment (@ "/**" CommentTailStar))
    (CommentTail (@ (* (@ (* NotStar) (+ "*") NotStarNotSlash))
                    (* NotStar)
		    (+ "*")
		    "/"))
    (CommentTailStar (@ (* (@ (* "*") NotStarNotSlash (* NotStar) "*"))
                        (* "*")
                        "/"))
    (NotStar (: (^ "*")))
    (NotStarNotSlash (: (^ "*" "/")))
                     
    (SyntaxComment (: TraditionalCommentEOF
                      EndOfLineComment))
    (TraditionalCommentEOF (@ "/*" CommentTailEOF))
    (CommentTailEOF (: (@ (* (@ (* NotStar) (+ "*") NotStarNotSlash))
                        (* NotStar)
                        (+ "*")
                        "/")
                       (@ (* (@ (* NotStar) (+ "*") NotStarNotSlash))
                        (* NotStar)
                        (* "*")
                        (eof))))

    ;; 3.8 (No need to worry about excluding keywords and such.  They will
    ;;      appear first in the lexer spec)
    (Identifier (@ JavaLetter (* JavaLetterOrDigit)))
    (JavaLetter (: (- "A" "Z")
		   (- "a" "z")
		   "_"
		   "$"))
    (JavaLetterOrDigit (: JavaLetter
			  (- "0" "9")))

    ;; 3.9
    (Keyword (: "abstract"    "default"    "if"            "private"      "this"
		"boolean"     "do"         "implements"    "protected"    "throw"
		"break"       "double"     "import"        "public"       "throws"
		"byte"        "else"       "instanceof"    "return"       "transient"
		"case"        "extends"    "int"           "short"        "try"
		"catch"       "final"      "interface"     "static"       "void"
		"char"        "finally"    "long"          "strictfp"     "volatile"
		"class"       "float"      "native"        "super"        "while"
		"const"       "for"        "new"           "switch"
		"continue"    "goto"       "package"       "synchronized"))

    ;; 3.10.1
    (Digits (+ (- #\0 #\9)))
    (DigitsOpt (* (- #\0 #\9)))
    
    (IntegerTypeSuffix (: "l" "L"))
    (DecimalNumeral (: #\0
		       (@ (- #\1 #\9) (* (- #\0 #\9)))))
    (HexDigit (: (- #\0 #\9)
		 (- #\a #\f)
		 (- #\A #\F)))
    (HexNumeral (: (@ #\0 "x" (+ HexDigit))
		   (@ #\0 "X" (+ HexDigit))))
    (OctalNumeral (@ #\0 (+ (- #\0 #\7))))
    
    ;; 3.10.2
    
    (FloatTypeSuffix (: "f" "F"))
    (DoubleTypeSuffix (: "d" "D"))

    (FloatA (@ Digits #\. DigitsOpt (? ExponentPart)))
    (FloatB (@ #\. Digits (? ExponentPart)))
    (FloatC (@ Digits ExponentPart))
    (FloatD (@ Digits (? ExponentPart)))
    
    (ExponentPart (@ (: "e" "E") (? (: "+" "-")) Digits))
    
    ;; MORE

    ;; 3.10.6
    (EscapeSequence (: "\\b" "\\t" "\\n" "\\f" "\\r" "\\\"" "\\'" "\\\\"
		       (@ #\\ (- #\0 #\3) (- #\0 #\7) (- #\0 #\7))
		       (@ #\\ (- #\0 #\7) (- #\0 #\7))
		       (@ #\\ (- #\0 #\7))))
    
    ;; 3.12
    (Operator (: "="	">" "<" "!"	"~"	"?"	":"
		 "=="	"<="	">="	"!="	"&&" "||"	"++"	"--"
		 "+"	"-"	"*"	"/"	"&" "|"	"^"	"%"	"<<" ">>" ">>>"
		 "+="	"-="	"*="	"/="	"&="	"|="	"^="	"%="	"<<="	">>="	">>>=")))

  ;;Old get-string
  ;; 3.10.5
  ;(define get-string
  ;  (lexer
  ;   (#\" null)
  ;   (EscapeSequence (cons (EscapeSequence->char lexeme)
  ;                         (get-string input-port)))
  ;   ((^ CR LF) (cons (string-ref lexeme 0)
  ;                    (get-string input-port)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;String lexer
  
  (define (get-string input-port)
    (letrec ((tokens (get-string-tokens input-port))
             (last-token (list-ref tokens (sub1 (length tokens))))
             (tokens->string
              (lambda (toks)
                (if (null? (cdr toks))
                    ""
                    (string-append (string (token-value (car (car toks))))
                                   (tokens->string (cdr toks)))))))
      (if (eq? 'STRING_END (get-token-name last-token))
          (token-STRING_LIT (list (tokens->string tokens) (caddr last-token)))
          (token-STRING_ERROR (list (tokens->string tokens) (caddr last-token) (car last-token))))))
  
  (define (get-string-tokens input-port)
    (let ((tok (get-str-tok input-port)))
      (case (get-token-name tok)
        ((STRING_EOF STRING_END STRING_NEWLINE) (list tok))
        (else (cons tok (get-string-tokens input-port))))))

  (define (get-token-name tok)
    (if (token? (car tok))
        (token-name (car tok))
        (car tok)))
  
  (define-tokens str-tok (STRING_CHAR))
  (define-empty-tokens err (STRING_END STRING_EOF STRING_NEWLINE))
  
  (define get-str-tok
    (lexer-src-pos
     (#\" (token-STRING_END))
     (EscapeSequence (token-STRING_CHAR (EscapeSequence->char lexeme)))
     ((^ CR LF) (token-STRING_CHAR (string-ref lexeme 0)))
     ((: CR LF) (token-STRING_NEWLINE))
     ((: (eof) #\032) (token-STRING_EOF))))
  
  ;; 3.10.6
  (define (EscapeSequence->char es)
    (cond
     ((string=? es "\\b") #\010)
     ((string=? es "\\t") #\011)
     ((string=? es "\\n") #\012)
     ((string=? es "\\f") #\014)
     ((string=? es "\\r") #\015)
     ((string=? es "\\\"") #\")
     ((string=? es "\\'") #\')
     ((string=? es "\\\\") #\\)
     (else (integer->char (string->number (trim-string es 1 0) 8)))))
	    
  (define get-token
    (lexer-src-pos
     ;; 3.12
     (Operator (let ((l lexeme))
                 (cond
                   ((string=? l "|") (token-PIPE))
                   ((string=? l "||") (token-OR))
                   ((string=? l "|=") (token-OREQUAL))
                   (else (string->symbol l)))))
     
     ;; 3.11
     ("(" (token-O_PAREN))
     (")" (token-C_PAREN))
     ("{" (token-O_BRACE))
     ("}" (token-C_BRACE))
     ("[" (token-O_BRACKET))
     ("]" (token-C_BRACKET))
     (";" (token-SEMI_COLON))
     ("," (token-COMMA))
     ("." (token-PERIOD))

     ;; 3.10.7
     ("null" (token-NULL_LIT))

     ;; 3.10.5
     (#\" (get-string input-port))
      ;(token-STRING_LIT (list->string (get-string input-port))))

     ;; 3.10.4
     ((@ #\' (^ CR LF #\' #\\) #\')
      (token-CHAR_LIT (string-ref lexeme 1)))
     ((@ #\' EscapeSequence #\') 
      (token-CHAR_LIT (EscapeSequence->char 
                       (trim-string lexeme 1 1))))
     
     ;; 3.10.3
     ("true" (token-TRUE_LIT))
     ("false" (token-FALSE_LIT))

     ;; 3.10.2
     ((: FloatA FloatB FloatC)
      (token-DOUBLE_LIT (string->number lexeme)))
     ((@ (: FloatA FloatB FloatC FloatD) FloatTypeSuffix)
      (token-FLOAT_LIT (string->number (trim-string lexeme 0 1))))
     ((@ (: FloatA FloatB FloatC FloatD) DoubleTypeSuffix)
      (token-DOUBLE_LIT (string->number (trim-string lexeme 0 1))))


     ;; 3.10.1
     (DecimalNumeral
      (token-INTEGER_LIT (string->number lexeme 10)))
     ((@ DecimalNumeral IntegerTypeSuffix)
      (token-LONG_LIT (string->number (trim-string lexeme 0 1) 10)))
     (HexNumeral
      (token-INTEGER_LIT (string->number (trim-string lexeme 2 0) 16)))
     ((@ HexNumeral IntegerTypeSuffix)
      (token-LONG_LIT (string->number (trim-string lexeme 2 1) 16)))
     (OctalNumeral
      (token-INTEGER_LIT (string->number (trim-string lexeme 1 0) 8)))
     ((@ OctalNumeral IntegerTypeSuffix)
      (token-LONG_LIT (string->number (trim-string lexeme 1 1) 8)))

     ;; 3.9
     (Keyword (string->symbol lexeme))

     ;; 3.8
     (Identifier (token-IDENTIFIER lexeme))

     ;; 3.7
     (Comment (return-without-pos (get-token input-port)))

;     ((special-comment) (return-without-pos (get-token input-port)))
     
     ;; 3.6
     ((+ WhiteSpace) (return-without-pos (get-token input-port)))

     ;; 3.5
     ((: (eof) #\032) 'EOF)))
  
  (define (syn-val a b c d)
    (values a b (position-offset c) (position-offset d)))
  
  (define get-syn-string
    (lexer
     ((: (eof) CR LF #\") (position-offset end-pos))
     (EscapeSequence (get-syn-string input-port))
     ((^ CR LF) (get-syn-string input-port))))
    
  (define get-syntax-token
    (lexer
  ;; 3.12
     (Operator
      (syn-val 'keyword lexeme start-pos end-pos))
     
     ;; 3.11
     ((: "(" ")" "{" "}" "[" "]")
      (syn-val 'keyword lexeme start-pos end-pos))
     ;; 3.11
     ((: ";" "," ".")
      (syn-val 'default lexeme start-pos end-pos))

     ;; 3.10.7, 3.10.4, 3.10.3, 3.10.1
     ((: "null" "true" "false"
         ;char-lit
         (@ #\' (^ CR LF #\' #\\) #\')
         (@ #\' EscapeSequence #\') 
         ;Doubles and Floats
         FloatA FloatB FloatC
         (@ (: FloatA FloatB FloatC FloatD) FloatTypeSuffix)
         (@ (: FloatA FloatB FloatC FloatD) FloatTypeSuffix)
         ;Decimal numbers
         DecimalNumeral
         HexNumeral
         OctalNumeral
         (@ DecimalNumeral IntegerTypeSuffix)
         (@ HexNumeral IntegerTypeSuffix)
         (@ OctalNumeral IntegerTypeSuffix))
      (syn-val 'literal lexeme start-pos end-pos))
      
     ;; 3.10.5
     (#\" (values 'string lexeme (position-offset start-pos) (get-syn-string input-port)))

     ;; 3.9
     (Keyword (syn-val 'keyword lexeme start-pos end-pos))

     ;; 3.8
     (Identifier (syn-val 'identifier lexeme start-pos end-pos))

     ;; 3.7
     (SyntaxComment (syn-val 'comment lexeme start-pos end-pos))

     ;; 3.6
     ((+ WhiteSpace) (syn-val 'white-space lexeme start-pos end-pos))

     ;; 3.5
     ((: (eof) #\032) (values 'eof "eof" start-pos end-pos))
     
     ((- #\000 #\377) (syn-val 'error lexeme start-pos end-pos))
     ))
  )