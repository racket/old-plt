(module parser mzscheme

  (require (lib "string.ss"))
  (require (lib "yacc.ss" "parser-tools"))
  (require (lib "lex.ss" "parser-tools"))
  (require (lib "list.ss"))

  (require "xl-util.ss")
  (require "formula.ss")

  (provide call-parser
           make-parser)

  (define (string-downcase s)
    (let ([tmp (string-copy s)])
      (string-lowercase! tmp)
      tmp))

  (define-tokens data-tokens 
    (STRING 
     QUOTED_STRING 
     NUMBER
     CELL-REF
     IDENTIFIER))
  
  (define-tokens op-tokens
    (ADD-OP
     MULT-OP
     BOOL-OP
     TBL-BEGIN
     EXP-OP))
  
  (define-empty-tokens cmp-tokens
    (LT LTE NE GT GTE)) 
  
  (define-empty-tokens syntax-tokens
    (INVALID-TOKEN
     EOF
     EQ 
     NEG ; for precedence
     LPAREN RPAREN
     LBRACE RBRACE
     COMMA
     RANGE-SEP SHEET-SEP))
  
  (define-lex-abbrevs 
    [digit (- "0" "9")]
    [number-sequence (+ digit)]
    [number-with-point (@ number-sequence "." number-sequence)]
    [unsigned-number (: number-sequence number-with-point)]
    [signed-number (@ "-" unsigned-number)]
    [number (: unsigned-number signed-number)]
    [letter (: (- "a" "z") (- "A" "Z"))]
    [alphanum_ (: digit letter "_")]
    [alphanum (: letter digit)]
    [cell-letter-sequence (@ letter (? letter))]
    [cell-number-sequence (: digit (@ digit digit)
                             (@ digit digit digit)
                             (@ digit digit digit digit))]
    [cell-reference (@ (? "$") cell-letter-sequence cell-number-sequence)]
    [whitespace (: #\space #\tab #\newline #\return)]
    [add-op (: "+" "-")]
    [mult-op (: "*" "/")]
    [exp-op "^"]
    [bool-op (: "<" ">" "<=" ">=" "=")]
    [tbl-begin "=TABLE("]
    [function-name 
     (: "NOT"
	"AND"
	"OR"
	"SUM"
	"AVERAGE"
	"MIN"
	"MAX"
	)]
    [identifier (: (@ (* letter) (* alphanum_) "_" (* alphanum_))
                   (@ (* alphanum) letter)
                   (@ letter letter letter (* alphanum)))])
  
  (define xl-lex
    (lexer
     [whitespace 
      (xl-lex input-port)]
     [(eof) 'EOF]
     [number
      (token-NUMBER (string->number lexeme))]
     [add-op
      (token-ADD-OP (string->symbol lexeme))]
     [mult-op
      (token-MULT-OP (string->symbol lexeme))]
     [exp-op
      (token-EXP-OP (string->symbol lexeme))]
     [bool-op
      (token-BOOL-OP (string->symbol lexeme))]
     [cell-reference
      (token-CELL-REF (string->symbol (string-downcase lexeme)))]
     [identifier (token-IDENTIFIER lexeme)]
     [tbl-begin (token-TBL-BEGIN lexeme)]
     [":" (token-RANGE-SEP)]
     ["," (token-COMMA)]
     ["(" (token-LPAREN)]
     [")" (token-RPAREN)]
     ["{" (token-LBRACE)]
     ["}" (token-RBRACE)]
     ["=" (token-EQ)]))
  
  (define make-parser 
    (lambda (symbol-table)
      (parser
       
       (start start)
       (end EOF)
       (error (lambda (a b c) (void)))
       (tokens data-tokens 
               op-tokens
               cmp-tokens
               syntax-tokens)
     
       (precs (left ADD-OP)
              (left MULT-OP)
              (left EXP-OP)
              (left BOOL-OP)
              (left NEG))
     
       (grammar
        
        (start [() #f]
               [(error start) $2]
               [(expr) $1])
        
        (formula [(EQ expr) $2])
      
        (expr
         ; cells have their own names
         [(CELL-REF) (make-cell-ref $1 (list $1))]
         [(IDENTIFIER)
          (let ([cell-loc (cadr (assoc $1 symbol-table))])
            (make-named-cell-ref cell-loc (list cell-loc) (string->symbol $1)))]
         [(NUMBER) (make-xl-number (gensym) null $1)]
         [(TBL-BEGIN expr COMMA RPAREN)
          (make-tbl-top
           (gensym)
           (formula-dependencies $2)
           $2)]
         [(TBL-BEGIN COMMA expr RPAREN)
          (make-tbl-left
           (gensym)
           (formula-dependencies $3)
           $3)]
         [(expr ADD-OP expr) 
          (make-binary-op 
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(expr MULT-OP expr) 
          (make-binary-op 
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(expr EXP-OP expr) 
          (make-binary-op 
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(expr BOOL-OP expr)
          (make-boolean-op
           (gensym)
           (append (formula-dependencies $1)
                   (formula-dependencies $3))
           $2
           $1 $3)]
         [(ADD-OP expr) (prec NEG)
          (make-unary-op 
           (gensym)
           (formula-dependencies $2)
           $1 $2)]
         [(IDENTIFIER LPAREN RPAREN)
          (make-application
           (gensym)
           empty (string->symbol (string-downcase $1)) empty)]
         [(IDENTIFIER LPAREN args RPAREN)
          (make-application 
           (gensym) 
           (cadr $3) (string->symbol (string-downcase $1)) (car $3))])
        
        ; returns list of symbols denoting cells
        (cell-range
         [(CELL-REF RANGE-SEP CELL-REF)
          (get-range-cells $1 $3)])
        
        ; returns two-elt list, (expr deps)
        (arg
         [(expr) (list (list $1) (formula-dependencies $1))]
         [(cell-range) (list (map (lambda (c)
                                    (make-cell-ref c (list c)))
                                  $1)
                             $1)])
        (args
         [(arg) (list (car $1) (cadr $1))]
         [(arg COMMA args) 
          (let ([arg1 (car $1)]
                [dep1 (cadr $1)]
                [arg2 (car $3)]
                [dep2 (cadr $3)])
            (list (append arg1 arg2) (append dep1 dep2)))])))))
  
  (define (call-parser parser s)
    (let* ([port (open-input-string s)]
	   [lex-thunk (lambda () (xl-lex port))])
      (parser lex-thunk))))
