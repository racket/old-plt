#cs
(module lexer mzscheme

  ;; Lexical Analysis according to the Java Language Specification First Edition 
  ;; chapter 3.
  ;; Lacks all Unicode support

  
  (require (lib "lex.ss" "parser-tools"))

  (provide Operators Separators EmptyLiterals Keywords java-vals get-token)
  
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
    (STRING_LIT CHAR_LIT INTEGER_LIT LONG_LIT FLOAT_LIT DOUBLE_LIT IDENTIFIER))  

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
    (EndOfLineComment (@ "//" (* InputCharacter) LineTerminator))
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

  ;; 3.10.5
  (define get-string
    (lexer
     (#\" null)
     (EscapeSequence (cons (EscapeSequence->char lexeme)
                           (get-string input-port)))
     ((^ CR LF) (cons (string-ref lexeme 0)
                      (get-string input-port)))))
  
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
     (#\" (token-STRING_LIT (list->string (get-string input-port))))

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

     ;; 3.6
     ((+ WhiteSpace) (return-without-pos (get-token input-port)))

     ;; 3.5
     ((: (eof) #\032) 'EOF))))