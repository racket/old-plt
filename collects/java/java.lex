;; Mario Latendresse, 11 May 2000.
;;
;; To generate the Java lexer. Taken from java.l
;;

HexDigit	[0-9a-fA-F]
Digit		[0-9]
OctalDigit	[0-7]
TetraDigit	[0-3]
NonZeroDigit	[1-9]
Letter		[a-zA-Z_]
AnyButSlash	[^\/]
AnyButAstr	[^\*]
BLANK		[ ]
TAB		[\009]
FF		[\012]
ESCCHR		[\\]
CR		[\013]
LF		[\010]
UniEsc          [\027]
Separator	[\(\)\{\}\[\]\;\,\.]
Delimiter1	[\=\>\<\!\~\?\:\+\-\*\/\&\|\^\%]
OctEscape1	[\\]{OctalDigit}
OctEscape2	[\\]{OctalDigit}{OctalDigit}
OctEscape3	[\\]{TetraDigit}{OctalDigit}{OctalDigit}
OctEscape	({OctEscape1}|{OctEscape2}|{OctEscape3})
Escape		[\\]([r]|[n]|[b]|[f]|[t]|[\\]|[\']|[\"])
ULetter         ({Letter}|{UniEsc})
Identifier 	{ULetter}({ULetter}|{Digit})*
Comment1        [\/][\*][^@]+({AnyButAstr}|[\*]{AnyButSlash})*[\*]+[\/]
Comment2        [\/][\/].*
Comment		({Comment1}|{Comment2})
Dimension	[\[]({CR}|{LF}|{FF}|{TAB}|{BLANK}|{Comment})*[\]]
IntSuffix	([l]|[L])
DecimalNum	{NonZeroDigit}{Digit}*
OctalNum	[0]{OctalDigit}*
HexNum		[0]([x]|[X]){HexDigit}{HexDigit}*
DecimalNumL	{NonZeroDigit}{Digit}*{IntSuffix}
OctalNumL	[0]{OctalDigit}*{IntSuffix}
HexNumL		[0]([x]|[X]){HexDigit}{HexDigit}*{IntSuffix}
IntegerLiteral	({DecimalNum}|{OctalNum}|{HexNum})
Sign		([\+]|[\-])
FlSuffix	([f]|[F]|[d]|[D])
SuffixD         ([d]|[D])
SuffixF         ([f]|[F])
SignedInt	{Sign}?{Digit}+
Expo		([e]|[E])
ExponentPart	{Expo}{SignedInt}?
Float1          {Digit}+[\.]{Digit}+?{ExponentPart}?
Float2		[\.]{Digit}+{ExponentPart}?
Float3		{Digit}+{ExponentPart}
Float4		{Digit}+
FloatingPoint	({Float1}|{Float2}|{Float3}|{Float4})
FloatingPointD	({Float1}{SuffixD}|{Float2}{SuffixD}|{Float3}{SuffixD}|{Float4}{SuffixD})
FloatingPointF	({Float1}{SuffixF}|{Float2}{SuffixF}|{Float3}{SuffixF}|{Float4}{SuffixF})
AnyChrChr	[^\\']
AnyStrChr	[^\\"]
Character	[\']({Escape}|{OctEscape}|{AnyChrChr})[\']
String		[\"]({Escape}|{OctEscape}|{AnyStrChr})*[\"]
Numeric  	({IntegerLiteral}|{FloatingPoint})
Literal		({Numeric}|{Character}|{String})

%%
;;
;; The rules
;;

{CR}   		(yycontinue)
{LF}		(yycontinue)
{FF}		(yycontinue)
{TAB}		(yycontinue)
{BLANK}		(yycontinue) ;; BLANK

;; Added for Contract Java, @pre, @post, /*@ ... @*/.

"/*@"           (cons java:BEGCONTRACT-tok (java:lexeme yytext))
"@*/"           (cons java:ENDCONTRACT-tok (java:lexeme yytext))

"@pre"          (cons java:PRE-tok       (java:lexeme yytext)) 
"@post"         (cons java:POST-tok      (java:lexeme yytext)) 
"@old"          (cons java:OLD-tok       (java:lexeme yytext)) 
"@result"       (cons java:RESULT-tok    (java:lexeme yytext)) 

;;
{Comment}	(yycontinue)
;;
;;
;;
{Separator}	(cons (cdr (assoc yytext 
                             `(("(" . ,java:LPAREN-tok)
                               (")" . ,java:RPAREN-tok)
                               ("{" . ,java:LBRACE-tok)
                               ("}" . ,java:RBRACE-tok)
                               ("[" . ,java:LSQPAREN-tok)
                               ("]" . ,java:RSQPAREN-tok)
                               (";" . ,java:SEMICOLON-tok)
                               ("," . ,java:COMMA-tok)
                               ("." . ,java:DOT-tok))))   
                      (java:lexeme yytext)) 
{Delimiter1}	(cons (cdr (assoc yytext
                            `(("=" . ,java:EQUAL-tok)
                              (">" . ,java:GT-tok)
                              ("<" . ,java:LT-tok)
                              ("!" . ,java:EXCLAMATION-tok)
                              ("~" . ,java:TILDE-tok)
                              ("?" . ,java:QUESTION-tok)
                              (":" . ,java:COLON-tok)
                              ("+" . ,java:PLUS-tok)
                              ("-" . ,java:MINUS-tok)
                              ("*" . ,java:STAR-tok)
                              ("/" . ,java:SLASH-tok)
                              ("&" . ,java:AND-tok)
                              ("|" . ,java:OR-tok)
                              ("^" . ,java:XOR-tok)
                              ("%" . ,java:PERCENT-tok))))
                      (java:lexeme yytext))
{Dimension}	(cons java:OP_DIM-tok     (java:lexeme yytext)) 


"=="		(cons java:OP_EQ-tok      (java:lexeme yytext)) 
"<="		(cons java:OP_LE-tok      (java:lexeme yytext)) 
">="		(cons java:OP_GE-tok      (java:lexeme yytext)) 
"!="		(cons java:OP_NE-tok      (java:lexeme yytext)) 
"||"		(cons java:OP_LOR-tok     (java:lexeme yytext)) 
"&&"		(cons java:OP_LAND-tok    (java:lexeme yytext)) 
"++"		(cons java:OP_INC-tok     (java:lexeme yytext)) 
"--"		(cons java:OP_DEC-tok     (java:lexeme yytext)) 
">>"		(cons java:OP_SHR-tok     (java:lexeme yytext)) 
"<<"		(cons java:OP_SHL-tok     (java:lexeme yytext)) 
">>>"		(cons java:OP_SHRR-tok    (java:lexeme yytext)) 
"+="		(cons java:ASS_ADD-tok    (java:lexeme yytext)) 
"-="		(cons java:ASS_SUB-tok    (java:lexeme yytext)) 
"*="		(cons java:ASS_MUL-tok    (java:lexeme yytext)) 
"/="		(cons java:ASS_DIV-tok    (java:lexeme yytext)) 
"&="		(cons java:ASS_AND-tok    (java:lexeme yytext)) 
"|="		(cons java:ASS_OR-tok     (java:lexeme yytext)) 
"^="		(cons java:ASS_XOR-tok    (java:lexeme yytext)) 
"%="		(cons java:ASS_MOD-tok    (java:lexeme yytext)) 
"<<="		(cons java:ASS_SHL-tok    (java:lexeme yytext)) 
">>="		(cons java:ASS_SHR-tok    (java:lexeme yytext)) 
">>>="		(cons java:ASS_SHRR-tok   (java:lexeme yytext)) 
		
"abstract"	(cons java:ABSTRACT-tok   (java:lexeme yytext)) 
"boolean"	(cons java:BOOLEAN-tok    (java:lexeme yytext)) 
"break"		(cons java:BREAK-tok      (java:lexeme yytext)) 
"byte"		(cons java:BYTE-tok       (java:lexeme yytext)) 
"case"		(cons java:CASE-tok       (java:lexeme yytext)) 
;; "cast"		(cons java:CAST-tok       (java:lexeme yytext)) 
"catch"		(cons java:CATCH-tok      (java:lexeme yytext)) 
"char"		(cons java:CHAR-tok       (java:lexeme yytext)) 
"class"		(cons java:CLASS-tok      (java:lexeme yytext)) 
"const"		(cons java:CONST-tok      (java:lexeme yytext)) 
"continue"	(cons java:CONTINUE-tok   (java:lexeme yytext)) 
"default"	(cons java:DEFAULT-tok    (java:lexeme yytext)) 
"do"            (cons java:DO-tok         (java:lexeme yytext)) 
"double"	(cons java:DOUBLE-tok     (java:lexeme yytext)) 
"else"		(cons java:ELSE-tok       (java:lexeme yytext)) 
"extends"	(cons java:EXTENDS-tok    (java:lexeme yytext)) 
"false"		(cons java:BOOLLIT-tok    (java:lexeme yytext)) 
"final"		(cons java:FINAL-tok      (java:lexeme yytext)) 
"finally"	(cons java:FINALLY-tok    (java:lexeme yytext)) 
"float"		(cons java:FLOAT-tok      (java:lexeme yytext)) 
"for"		(cons java:FOR-tok        (java:lexeme yytext)) 
;;"future"	(cons java:FUTURE-tok     (java:lexeme yytext)) 
;;"generic"	(cons java:GENERIC-tok    (java:lexeme yytext)) 
"goto"		(cons java:GOTO-tok       (java:lexeme yytext)) 
"if"		(cons java:IF-tok         (java:lexeme yytext)) 
"implements"    (cons java:IMPLEMENTS-tok (java:lexeme yytext)) 
"import"	(cons java:IMPORT-tok     (java:lexeme yytext)) 
;;"inner"		(cons java:INNER-tok      (java:lexeme yytext)) 
"instanceof"	(cons java:INSTANCEOF-tok (java:lexeme yytext)) 
"int"		(cons java:INT-tok        (java:lexeme yytext)) 
"interface"	(cons java:INTERFACE-tok  (java:lexeme yytext)) 
"long"		(cons java:LONG-tok       (java:lexeme yytext)) 
"native"	(cons java:NATIVE-tok     (java:lexeme yytext)) 
"new"		(cons java:NEW-tok        (java:lexeme yytext)) 
"null"		(cons java:JNULL-tok      (java:lexeme yytext)) 
;;"operator"	(cons java:OPERATOR-tok   (java:lexeme yytext)) 
;;"outer"		(cons java:OUTER-tok      (java:lexeme yytext)) 
"package"	(cons java:PACKAGE-tok    (java:lexeme yytext)) 
"private"	(cons java:PRIVATE-tok    (java:lexeme yytext)) 
"protected"	(cons java:PROTECTED-tok  (java:lexeme yytext)) 
"public"	(cons java:PUBLIC-tok     (java:lexeme yytext)) 
;;"rest"		(cons java:REST-tok       (java:lexeme yytext)) 
"return"	(cons java:RETURN-tok     (java:lexeme yytext)) 
"short"		(cons java:SHORT-tok      (java:lexeme yytext)) 
"static"	(cons java:STATIC-tok     (java:lexeme yytext)) 
"super"		(cons java:SUPER-tok      (java:lexeme yytext)) 
"switch"	(cons java:SWITCH-tok     (java:lexeme yytext)) 
"synchronized"	(cons java:SYNCHRONIZED-tok (java:lexeme yytext)) 
"this"		(cons java:THIS-tok       (java:lexeme yytext)) 
"throw"		(cons java:THROW-tok      (java:lexeme yytext)) 
"throws"	(cons java:THROWS-tok     (java:lexeme yytext)) 
"transient"	(cons java:TRANSIENT-tok  (java:lexeme yytext)) 
"true"		(cons java:BOOLLIT-tok    (java:lexeme yytext)) 
"try"		(cons java:TRY-tok        (java:lexeme yytext)) 
;;"var"		(cons java:VAR-tok        (java:lexeme yytext)) 
"void"		(cons java:VOID-tok       (java:lexeme yytext)) 
"volatile"	(cons java:VOLATILE-tok   (java:lexeme yytext)) 
"while"		(cons java:WHILE-tok      (java:lexeme yytext)) 

{Identifier}	(cons java:IDENTIFIER-tok (java:lexeme yytext)) 

{DecimalNum}    (cons java:LITERAL-DEC-tok (java:lexeme yytext)) 
{OctalNum}      (cons java:LITERAL-OCT-tok (java:lexeme yytext)) 
{HexNum}        (cons java:LITERAL-HEX-tok (java:lexeme yytext)) 
{DecimalNumL}   (cons java:LITERAL-DEC-LONG-tok (java:lexeme yytext)) 
{OctalNumL}     (cons java:LITERAL-OCT-LONG-tok (java:lexeme yytext)) 
{HexNumL}       (cons java:LITERAL-HEX-LONG-tok (java:lexeme yytext)) 

{FloatingPoint} (cons java:LITERAL-FLOATING-tok (java:lexeme yytext)) 
{FloatingPointD} (cons java:LITERAL-DOUBLE-tok   (java:lexeme yytext)) 
{FloatingPointF} (cons java:LITERAL-FLOAT-tok    (java:lexeme yytext)) 

{Character}     (cons java:LITERAL-CHAR-tok   (java:lexeme yytext)) 
{String}	(cons java:LITERAL-STRING-tok (java:lexeme yytext)) 
<<ERROR>>       (error-lexer yygetc yytext yycontinue)
