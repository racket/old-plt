#cs
(module sml-parse mzscheme
  (require (lib "contracts.ss")
           (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax"))

  (provide/contract
   (parse-ml-file (string? . -> . false?))
   (parse-ml-port (input-port? string? . -> . false?)))
  
  (define (parse-ml-port ip f)
    (let ((get-token (make-get-token #f)))
      (port-count-lines! ip)
      (file-path f)
      (parse-sml (lambda () (get-token ip)))))
  
  (define (parse-ml-file f)
    (call-with-input-file f
      (lambda (p)
        (parse-ml-port p f))))
  
  
  (define-empty-tokens sml-etoks
    (ABSTYPE AND ANDALSO ARROW AS BAR CASE COLON
     COLONGT COMMA DARROW DATATYPE DO DOTDOTDOT ELSE END
     EOF EQTYPE EQUALS EXCEPTION FN FUN FUNCTOR HANDLE
     HASH HASHLBRACKET IF IN INCLUDE INFIX INFIXR LBRACE
     LBRACKET LET LOCAL LPAREN NONFIX OF OP OPEN ORELSE
     PRIM_EQTYPE PRIM_REFTYPE PRIM_TYPE PRIM_VAL QUOTEL RAISE
     RBRACE RBRACKET REC RPAREN SEMICOLON SHARING SIG
     SIGNATURE STAR STRUCT STRUCTURE THEN TYPE
     UNDERBAR VAL WHERE WHILE WITH WITHTYPE))
  
  (define-tokens sml-toks 
    (CHAR ID NEGINT NZDIGIT NZPOSINT2 QUAL_ID QUAL_STAR
     QUOTEM QUOTER REAL STRING TYVAR WORD ZDIGIT ZPOSINT2))
  
  (define-lex-abbrevs
   (letter (: (- "a" "z") (- "A" "Z")))
   (digit (- "0" "9"))
   (hex-digit (: (- "0" "9") (- "A" "F") (- "a" "f")))
   (letter-or-digit (: (letter) (digit)))
   (misc-idents (: "!" "%" "&" "$" "#" "+" "-" "/" ":" "<" "=" ">" "?" "@" "\\"
                   "~" "|" "*"))
   (id (: (@ (letter) (* (: (letter-or-digit) "_" "'")))
          (+ (: (misc-idents) "`" "^"))))
   (qid (: (@ (letter) (* (: (letter-or-digit) "_" "'")))
           (+ (: (misc-idents) "^"))))
   (aqid (: (@ (letter) (* (: (letter-or-digit) "_" "'")))
            (+ (misc-idents))))
   (string-contents (: (^ "\\" "\"") 
                       (@ "\\" (+ (: " " "\t" "\n" "\r")) "\\")
                       "\\^\\"
                       (@ "\\" (- #\000 #\377)))))
  
  (define keyword-table
    (let ((kt (make-hash-table)))
      (for-each (lambda (k) (hash-table-put! kt (car k) (cadr k)))
                `((abstype ABSTYPE)
                  (and AND)
                  (andalso ANDALSO)
                  (as AS)
                  (case CASE)
                  (datatype DATATYPE)
                  (do DO)
                  (else ELSE)
                  (eqtype EQTYPE)
                  (end END)
                  (exception EXCEPTION)
                  (fn FN)
                  (fun FUN)
                  (functor FUNCTOR)
                  (handle HANDLE)
                  (if IF)
                  (in IN)
                  (include INCLUDE)
                  (infix INFIX)
                  (infixr INFIXR)
                  (let LET)
                  (local LOCAL)
                  (nonfix NONFIX)
                  (of OF)
                  (op OP)
                  (open OPEN)
                  (orelse ORELSE)
                  (prim_eqtype PRIM_EQTYPE)
                  (prim_EQtype PRIM_REFTYPE)
                  (prim_type PRIM_TYPE)
                  (prim_val PRIM_VAL)
                  (raise RAISE)
                  (rec REC)
                  (sharing SHARING)
                  (sig SIG)
                  (signature SIGNATURE)
                  (struct STRUCT)
                  (structure STRUCTURE)
                  (then THEN)
                  (type TYPE)
                  (val VAL)
                  (where WHERE)
                  (while WHILE)
                  (with WITH)
                  (withtype WITHTYPE)
                  (,(string->symbol "#") HASH)
                  (,(string->symbol "->") ARROW)
                  (,(string->symbol "|") BAR)
                  (,(string->symbol ":>") COLONGT)
                  (,(string->symbol ":") COLON)
                  (,(string->symbol "=>") DARROW)
                  (,(string->symbol "=") EQUALS)
                  (,(string->symbol "*") STAR)))
      kt))
  
  (define (get-keyword k)
    (hash-table-get keyword-table (string->symbol k) (lambda () (token-ID k))))
  
  (define (get-qualified-id l)
    (let* ((len (string-length l))
           (id (let loop ((i 0)
                          (n 0)
                          (acc 0))
                 (cond
                   ((>= n len)
                    (cons (substring l i len) acc))
                   ((char=? (string-ref l n) #\.)
                    (loop (add1 n) (add1 n) (cons (substring l i n) acc)))
                   (else
                    (loop i (add1 n) acc))))))
      (cond
        ((equal? `("*") id) (token-QUAL_STAR id))
        (else (token-QUAL_ID id)))))
           
  (define (raise-error msg sp ep)
    (raise-read-error msg
                      (file-path)
                      (position-line sp)
                      (position-col sp)
                      (position-offset sp)
                      (- (position-offset ep) (position-offset sp))))
  
  (define (process-sml-string s sp)
    (let ((ip (open-input-string s)))
      (let loop ((i 0)
                 (acc null))
        (let-values (((num next-char) (get-char-from-string ip)))
          (cond
            ((char? next-char)
             (loop (+ num i)
                   (cons next-char acc)))
            ((eq? 'skip next-char)
             (loop (+ num i) acc))
            ((eq? 'eof next-char)
             (list->string (reverse acc)))
            (else
             (raise-read-error next-char
                               (file-path)
                               (position-line sp)
                               (position-col sp)
                               (+ i (position-offset sp))
                               num)))))))

  (define get-char-from-string
    (lexer
     ("\\a" (values 2 #\007))
     ("\\b" (values 2 #\010))
     ("\\t" (values 2 #\011))
     ("\\n" (values 2 #\012))
     ("\\v" (values 2 #\013))
     ("\\f" (values 2 #\014))
     ("\\r" (values 2 #\015))
     ("\\\\" (values 2 #\\))
     ("\\\"" (values 2 #\"))
     ((@ "\\" (+ (: " " "\t" "\n" "\r")) "\\")
      (values (string-length lexeme) 'skip))
     ((@ "\\^" (- "@" "_"))
      (values 2 (integer->char (- (char->integer (string-ref lexeme 2)) 64))))
     ((@ "\\" (digit) (digit) (digit))
      (let ((ascii-value (string->number (substring lexeme 1 4))))
        (values 4 (if (> ascii-value 255) 
                      "character code is too large"
                      (integer->char ascii-value)))))
     ((@ "\\u" (hex-digit) (hex-digit) (hex-digit) (hex-digit))
      (let ((ascii-value (string->number (substring lexeme 2 6)  16)))
        (values 6 (if (> ascii-value 255)
                      "character code is too large"
                      (integer->char ascii-value)))))
     ("\\" (values 1 "ill-formed escape sequence"))
     ((: "\n" "\r") (values 1 "newline not permitted in string"))
     ((: #\177 #\377 (- #\001 #\032))
      (values 1 "invalid character in string"))
     ((- #\000 #\377) (values 1 (string-ref lexeme 0)))
     ((eof) (values 0 'eof))))
             
     
     
  (define (make-get-token quotation?)
    (let ((lexing-mode 'normal)
          (comment-depth 0)
          (par-count null))
      (letrec ((token (lambda (ip)
                        (case lexing-mode
                          ((normal) (if quotation? (token-nq ip) (token-n ip)))
                          ((quote) (quotation ip))
                          ((antiquote) (anti-quotation ip)))))
               (token-n
                (lexer-src-pos
                 ((+ (: " " "\n" "\r" "\t" "\f")) 
                  (return-without-pos (token-n input-port)))
                 ("(*"
                  (begin
                    (set! comment-depth (add1 comment-depth))
                    (comment input-port)
                    (return-without-pos (token-n input-port))))
                 ("*)" (raise-error "*) not in comment" start-pos end-pos))
                 ((@ "'" (+ (: (letter-or-digit) "_" "'"))) (token-TYVAR lexeme))
                 ("0" (token-ZDIGIT 0))
                 ((- "1" "9") (token-NZDIGIT (string->number lexeme)))
                 ((@ "0" (+ (digit))) (token-ZPOSINT2 (string->number lexeme)))
                 ((@ (- "1" "9") (+ (digit))) (token-NZPOSINT2 (string->number lexeme)))
                 ((@ "~" (+ (digit)))
                  (token-NEGINT (- (string->number (substring lexeme 1 (string-length lexeme))))))
                 ((@ "0w" (+ (digit)))
                  (token-WORD (string->number (substring lexeme 2 (string-length lexeme)))))
                 ((@ "0wx" (+ (: (digit) (- "a" "f") (- "A" "F"))))
                  (token-WORD (string->number (substring lexeme 3 (string-length lexeme)) 16)))
                 ((@ (? "~")
                     (+ (digit)) 
                     (? (@ "." (+ (digit))))
                     (? (@  (: "e" "E") (? "~") (+ (digit)))))
                  (token-REAL (exact->inexact
                               (string->number (regexp-replace* "~" lexeme "-")))))
                 ((@ (? "#") "\"" (* (string-contents)) "\"")
                  (if (char=? #\# (string-ref lexeme 0))
                      (token-CHAR (process-sml-string lexeme start-pos))
                      (token-STRING (process-sml-string lexeme start-pos))))
                 ((@ (? "#") "\"" (* (string-contents)) (eof))
                  (raise-error "File ended inside of quotation" start-pos end-pos))
                 ("_" 'UNDERBAR)
                 ("," 'COMMA)
                 ("..." 'DOTDOTDOT)
                 ("{" 'LBRACE)
                 ("}" 'RBRACE)
                 ("[" 'LBRACKET)
                 ("#[" 'HASHLBRACKET)
                 ("]" 'RBRACKET)
                 ("("
                  (begin
                    (unless (null? par-count)
                      (set! par-count (cons (add1 (car par-count)) (cdr par-count))))
                    'LPAREN))
                 (")"
                  (begin
                    (cond
                      ((null? par-count)
                       'RPAREN)
                      (else
                       (let ((count (sub1 (car par-count))))
                         (cond
                           ((= count 0)
                            (set! lexing-mode 'quote)
                            (return-without-pos (token input-port)))
                           (else
                            (set! par-count (cons count (cdr par-count)))
                            'RPAREN)))))))
                 (";" 'SEMICOLON)
                 ((eof) 'EOF)
                 ((id) (get-keyword lexeme))
                 ((@ (+ (@ (id) ".")) (id)) (get-qualified-id lexeme))))
               (token-nq
                (lexer-src-pos
                 ((+ (: " " "\n" "\r" "\t" "\f")) 
                  (return-without-pos (token-n input-port)))
                 ("(*"
                  (begin
                    (set! comment-depth (add1 comment-depth))
                    (comment input-port)
                    (return-without-pos (token-n input-port))))
                 ("*)" (raise-error "*) not in comment" start-pos end-pos))
                 ((@ "'" (+ (: (letter-or-digit) "_" "'"))) (token-TYVAR lexeme))
                 ("0" (token-ZDIGIT 0))
                 ((- "1" "9") (token-NZDIGIT (string->number lexeme)))
                 ((@ "0" (+ (digit))) (token-ZPOSINT2 (string->number lexeme)))
                 ((@ (- "1" "9") (+ (digit))) (token-NZPOSINT2 (string->number lexeme)))
                 ((@ "~" (+ (digit)))
                  (token-NEGINT (- (string->number (substring lexeme 1 (string-length lexeme))))))
                 ((@ "0w" (+ (digit)))
                  (token-WORD (string->number (substring lexeme 2 (string-length lexeme)))))
                 ((@ "0wx" (+ (: (digit) (- "a" "f") (- "A" "F"))))
                  (token-WORD (string->number (substring lexeme 3 (string-length lexeme)) 16)))
                 ((@ (? "~")
                     (+ (digit)) 
                     (? (@ "." (+ (digit))))
                     (? (@  (: "e" "E") (? "~") (+ (digit)))))
                  (token-REAL (exact->inexact
                               (string->number (regexp-replace* "~" lexeme "-")))))
                 ((@ (? "#") "\"" (* (string-contents)) "\"")
                  (if (char=? #\# (string-ref lexeme 0))
                      (token-CHAR (process-sml-string lexeme start-pos))
                      (token-STRING (process-sml-string lexeme start-pos))))
                 ((@ (? "#") "\"" (* (string-contents)) (eof))
                  (raise-error "File ended inside of quotation" start-pos end-pos))
                 ("_" 'UNDERBAR)
                 ("," 'COMMA)
                 ("..." 'DOTDOTDOT)
                 ("{" 'LBRACE)
                 ("}" 'RBRACE)
                 ("[" 'LBRACKET)
                 ("#[" 'HASHLBRACKET)
                 ("]" 'RBRACKET)
                 ("("
                  (begin
                    (unless (null? par-count)
                      (set! par-count (cons (add1 (car par-count)) (cdr par-count))))
                    'LPAREN))
                 (")"
                  (begin
                    (cond
                      ((null? par-count)
                       'RPAREN)
                      (else
                       (let ((count (sub1 (car par-count))))
                         (cond
                           ((= count 0)
                            (set! lexing-mode 'quote)
                            (return-without-pos (token input-port)))
                           (else
                            (set! par-count (cons count (cdr par-count)))
                            'RPAREN)))))))
                 (";" 'SEMICOLON)
                 ((eof) 'EOF)
                 ((qid) (get-keyword lexeme))
                 ((@ (+ (@ (qid) ".")) (qid)) (get-qualified-id lexeme))
                 ("`"
                  (begin
                    (set! lexing-mode 'quote)
                    'QUOTEL))))
               (comment
                (lexer
                 ("(*"
                  (begin
                    (set! comment-depth (add1 comment-depth))
                    (comment input-port)))
                 ("*)"
                  (begin
                    (set! comment-depth (sub1 comment-depth))
                    (when (> comment-depth 0) (comment input-port))))
                 ((eof) (raise-error "File ended inside of comment" start-pos end-pos))
                 ((- #\000 #\377) (comment input-port))))
               (quotation
                (lexer-src-pos
                 ((@ (* (^ "`" "^")) "`")
                  (begin
                    (set! lexing-mode 'normal)
                    (token-QUOTER lexeme)))
                 ((@ (* (^ "`" "^")) "^")
                  (begin
                    (set! lexing-mode 'antiquote)
                    (token-QUOTEM lexeme)))
                 ((@ (* (^ "`" "^")) (eof))
                  (raise-error "File ended inside of quotation" start-pos end-pos))))
               (anti-quotation
                (lexer-src-pos
                 ((aqid)
                  (begin
                    (set! lexing-mode 'quote)
                    (get-keyword lexeme)))
                 ("("
                  (begin
                    (set! par-count (cons 1 par-count))
                    (set! lexing-mode 'normal)
                    (return-without-pos (token-nq input-port))))
                 ("`" (raise-error "missing antiquote" start-pos end-pos))
                 ((eof) (raise-error "File ended inside antiquote" start-pos end-pos))
                 ((- #\000 #\377) (raise-error "malformed antiquote" start-pos end-pos)))))
        token)))
          
         
  (define parse-sml
    (parser
     (start ToplevelPhrase SigFile StructFile TopSpecFile TopDecFile)
     (end EOF)
     (error (lambda (tok-ok? tok-name tok-val spos epos)
              (cond
                (tok-ok?
                 (raise-error (format "parse error at ~a" tok-name) spos epos))
                (else
                 (error 'parse-sml "Unknown token type ~a" tok-name)))))
     (tokens sml-toks sml-etoks)
     (src-pos)
     ;;(debug "sml.table")
     (precs (right AND)
            (nonassoc DARROW)
            (nonassoc BAR)
            (nonassoc ELSE)
            (nonassoc DO)
            (nonassoc RAISE)
            (right HANDLE)
            (right ORELSE)
            (right ANDALSO)
            (right AS)
            (right ARROW)
            (nonassoc ID EQUALS)
            (right STAR))
     (suppress)
     (grammar
      (Ident 
       ((ID) #f)
       ((STAR) #f))
      (IdentWithLoc 
       ((Ident) #f))
      (OpIdent
       ((Ident) #f)
       ((OP Ident) #f))
      (EqIdent
       ((Ident) #f)
       ((EQUALS) #f))
      (ModId
       ((IdentWithLoc) #f))
      (SigId
       ((IdentWithLoc) #f))
      (TypeIdent
       ((ID) #f))
      (LongTypeIdent
       ((TypeIdent) #f)
       ((QUAL_ID) #f))
      (LongIdent
       ((Ident) #f)
       ((QUAL_ID) #f)
       ((QUAL_STAR) #f))
      (LongOpIdent
       ((LongIdent) #f)
       ((OP Ident) #f)
       ((OP QUAL_ID) #f)
       ((OP QUAL_STAR) #f))
      (LongOpEqIdent
       ((LongOpIdent) #f)
       ((EQUALS) #f)
       ((OP EQUALS) #f))
      (TyVar
       ((TYVAR) #f))
      (EqIdent_seq1
       ((EqIdent EqIdent_seq1) #f)
       ((EqIdent) #f))
      (LongModId
       ((LongOpIdent) #f))
      (LongModIdInfo_seq1
       ((LongModId LongModIdInfo_seq1) #f)
       ((LongModId) #f))
      (DIGIT_opt
       ((ZDIGIT) #f)
       ((NZDIGIT) #f) (() #f))
      (Integer
       ((ZPOSINT2) #f)
       ((NZPOSINT2) #f)
       ((NEGINT) #f)
       ((ZDIGIT) #f)
       ((NZDIGIT) #f))
      (NumLabel
       ((NZPOSINT2) #f)
       ((NZDIGIT) #f))
      (Label
       ((Ident) #f)
       ((NumLabel) #f))
      (Arity
       ((ZPOSINT2) #f)
       ((NZPOSINT2) #f)
       ((ZDIGIT) #f)
       ((NZDIGIT) #f))
      (ToplevelPhrase
       ((Exp EOPh) #f)
       ((KWDec_seq1 EOPh) #f)
       ((EOPh) #f))
      (EOPh
       ((SEMICOLON) #f))
       ;;((EOF) #f))
      (SemiEof
       ((SEMICOLON SemiEof) #f))
       ;;((EOF) #f))
      (Dec
       ((KWDec Dec) #f)
       ((SEMICOLON Dec) #f)
       (() #f))
      (KWDec_seq1 
       ((KWDec KWDec_seq1) #f)
       ((KWDec) #f))
      (TopDecFile
       ;;((KWDec_seq EOF) #f))
       ((KWDec_seq) #f))
      (StructFile
       ((STRUCTURE ModId EQUALS ModExp SemiEof) #f)
       ((STRUCTURE ModId COLONGT SigId EQUALS ModExp SemiEof) #f)
       ;;((KWCoreDec_seq EOF) #f))
       ((KWCoreDec_seq) #f))
      (KWDec_seq
       ((KWDec KWDec_seq) #f)
       ((SEMICOLON KWDec_seq) #f)
       (() #f))
      (KWCoreDec_seq
       ((KWCoreDec KWCoreDec_seq) #f)
       ((SEMICOLON KWCoreDec_seq) #f)
       (() #f))
      (KWDec
       ((KWCoreDec) #f)
       ((KWModuleDec) #f))
      (KWModuleDec
       ((STRUCTURE ModBind_seq1) #f)
       ((FUNCTOR FunBind_seq1) #f)
       ((SIGNATURE SigBind_seq1) #f))
      (KWCoreDec
       ((VAL ValBind) #f)
       ((VAL TyVarSeq1 ValBind) #f)
       ((PRIM_VAL PrimValBind) #f)
       ((PRIM_VAL TyVarSeq1 PrimValBind) #f)
       ((FUN FValBind) #f)
       ((FUN TyVarSeq1 FValBind) #f)
       ((TYPE TypBind) #f)
       ((PRIM_TYPE TypDesc) #f)
       ((PRIM_EQTYPE TypDesc) #f)
       ((PRIM_REFTYPE TypDesc) #f)
       ((DATATYPE DatBind_0 WithType_opt) #f)
       ((DATATYPE DatBind_n WithType_opt) #f)
       ((DATATYPE TyCon EQUALS DATATYPE TyConPath) #f)
       ((ABSTYPE DatBind WithType_opt WITH Dec END) #f)
       ((EXCEPTION ExBind) #f)
       ((LOCAL Dec IN Dec END) #f)
       ((OPEN LongModIdInfo_seq1) #f)
       ((INFIX DIGIT_opt EqIdent_seq1) #f)
       ((INFIXR DIGIT_opt EqIdent_seq1) #f)
       ((NONFIX EqIdent_seq1) #f))
      (ValBind
       ((Pat EQUALS Exp AndValBind_opt) #f)
       ((REC FnValBind) #f))
      (AndValBind_opt ((AND ValBind) #f) (() #f))
      (PrimValBind
       ((OpIdent COLON Ty EQUALS Arity STRING AndPrimValBind_opt) #f))
      (AndPrimValBind_opt
       ((AND PrimValBind) #f)
       (() #f))
      (FnValBind
       ((Pat EQUALS Exp AndFnValBind_opt) #f)
       ((REC FnValBind) #f))
      (AndFnValBind_opt
       ((AND FnValBind) #f)
       (() #f))
      (TypBind
       ((TyVarSeq TyCon EQUALS Ty AndTypBind_opt) #f))
      (AndTypBind_opt
       ((AND TypBind) #f)
       (() #f))
      (DatBind
       ((TyVarSeq TyCon EQUALS ConBind AndDatBind_opt) #f))
      (DatBind_0
       ((TyCon EQUALS ConBind AndDatBind_opt) #f))
      (DatBind_n
       ((TyVarSeq1 TyCon EQUALS ConBind AndDatBind_opt) #f))
      (AndDatBind_opt
       ((AND DatBind) #f)
       (() #f))
      (ConBind
       ((OpIdent OfTy_opt BarConBind_opt) #f))
      (BarConBind_opt
       ((BAR ConBind) #f)
       (() #f))
      (WithType_opt
       ((WITHTYPE TypBind) #f)
       (() #f))
      (ExBind
       ((OpIdent OfTy_opt AndExBind_opt) #f)
       ((OpIdent EQUALS LongOpEqIdent AndExBind_opt) #f))
      (AndExBind_opt
       ((AND ExBind) #f)
       (() #f))
      (ExDesc
       ((OpIdent OfTy_opt AndExDesc_opt) #f))
      (AndExDesc_opt
       ((AND ExDesc) #f)
       (() #f))
      (ColonTy_opt
       ((COLON Ty) #f)
       (() #f))
      (OfTy_opt
       ((OF Ty) #f)
       (() #f))
      (FValBind
       ((FClauseWithLoc AndFValBind_opt) #f))
      (AndFValBind_opt
       ((AND FValBind) #f)
       (() #f))
      (FClauseWithLoc
       ((FClause) #f))
      (FClause
       ((AtPat_seq1 ColonTy_opt EQUALS Exp BarFClause_opt) #f))
      (BarFClause_opt
       ((BAR FClause) #f)
       (() #f))
      (SCon
       ((Integer) #f)
       ((WORD) #f)
       ((CHAR) #f)
       ((REAL) #f)
       ((STRING) #f))
      (VIdPathInfo
       ((LongOpEqIdent) #f))
      (AtExp
       ((SCon) #f)
       ((VIdPathInfo) #f)
       ((LET Dec IN Exp END) #f)
       ((HASH Label) #f)
       ((LPAREN Exp RPAREN) #f)
       ((LPAREN RPAREN) #f)
       ((LPAREN ExpComma_seq2 RPAREN) #f)
       ((LPAREN ExpSemicolon_seq2 RPAREN) #f)
       ((LBRACE ExpRow_opt RBRACE) #f)
       ((LET Dec IN ExpSemicolon_seq2 END) #f)
       ((LBRACKET STRUCTURE ModExp AS SigExp RBRACKET) #f)
       ((LBRACKET FUNCTOR ModExp AS SigExp RBRACKET) #f)
       ((LBRACKET ExpComma_seq0 RBRACKET) #f)
       ((HASHLBRACKET ExpComma_seq0 RBRACKET) #f)
       ((QUOTEL QuoteTail) #f))
      (QuoteTail
       ((QUOTER) #f)
       ((QUOTEM ExpQuoteTail) #f))
      (ExpQuoteTail
       ((Exp QuoteTail) #f))
      (ExpComma_seq0
       ((ExpComma_seq1) #f)
       (() #f))
      (ExpComma_seq1
       ((Exp COMMA ExpComma_seq1) #f)
       ((Exp) #f))
      (ExpComma_seq2
       ((Exp COMMA ExpComma_seq1) #f))
      (ExpSemicolon_seq2
       ((Exp SEMICOLON ExpSemicolon_seq2) #f)
       ((Exp SEMICOLON Exp) #f))
      (AtExp_seq1
       ((AtExp AtExp_seq1) #f)
       ((AtExp) #f))
      (ExpRow_opt
       ((ExpRow) #f)
       (() #f))
      (ExpRow
       ((Label EQUALS Exp CommaExpRow_opt) #f))
      (CommaExpRow_opt
       ((COMMA ExpRow) #f)
       (() #f))
      (InfixExp
       ((AtExp_seq1) #f))
      (Exp
       ((InfixExp) #f)
       ((Exp COLON Ty) #f)
       ((Exp ANDALSO Exp) #f)
       ((Exp ORELSE Exp) #f)
       ((Exp HANDLE Match) #f)
       ((RAISE Exp) #f)
       ((IF Exp THEN Exp ELSE Exp) #f)
       ((WHILE Exp DO Exp) #f)
       ((CASE Exp OF MatchWithLoc) #f)
       ((FN Match) #f))
      (MatchWithLoc
       ((Match) #f))
      (Match
       ((MRule BAR Match) #f)
       ((MRule) (prec DARROW) #f))
      (MRule
       ((Pat DARROW Exp) #f))
      (InfixPat
       ((AtPat_seq1) #f))
      (Pat
       ((InfixPat) #f)
       ((Pat COLON Ty) #f)
       ((Pat AS Pat) #f))
      (AtPat
       ((UNDERBAR) #f)
       ((SCon) #f)
       ((LongOpIdent) #f)
       ((LBRACE PatRow_opt RBRACE) #f)
       ((LPAREN Pat RPAREN) #f)
       ((LPAREN RPAREN) #f)
       ((LPAREN PatComma_seq2 RPAREN) #f)
       ((LBRACKET PatComma_seq0 RBRACKET) #f)
       ((HASHLBRACKET PatComma_seq0 RBRACKET) #f))
      (PatRow_opt
       ((PatRow) #f)
       (() #f))
      (PatRow
       ((DOTDOTDOT) #f)
       ((Label EQUALS Pat CommaPatRow_opt) #f)
       ((IdentWithLoc ColonTy_opt AsPat_opt CommaPatRow_opt) #f))
      (AsPat_opt
       ((AS Pat) #f)
       (() #f))
      (CommaPatRow_opt
       ((COMMA PatRow) #f)
       (() #f))
      (AtPat_seq1
       ((AtPat AtPat_seq1) #f)
       ((AtPat) #f))
      (PatComma_seq0
       ((PatComma_seq1) #f)
       (() #f))
      (PatComma_seq1
       ((Pat COMMA PatComma_seq1) #f)
       ((Pat) #f))
      (PatComma_seq2
       ((Pat COMMA PatComma_seq1) #f))
      (TyCon
       ((ID) #f))
      (WhereModBind_opt
       ((WHERE ModId OptConEqualsModExp) #f)
       (() #f))
      (TyConPath
       ((LongTypeIdent WhereModBind_opt) #f))
      (Ty
       ((TupleTy ARROW Ty) #f)
       ((TupleTy) #f))
      (TupleTy
       ((Ty_sans_STAR) #f)
       ((Ty_sans_STAR STAR TupleTy) #f))
      (Ty_sans_STAR
       ((LPAREN TyComma_seq2 RPAREN TyConPath) #f)
       ((Ty_sans_STAR TyConPath) #f)
       ((AtomicTy) #f))
      (TyComma_seq2
       ((Ty COMMA TyComma_seq2) #f)
       ((Ty COMMA Ty) #f))
      (AtomicTy
       ((TyConPath) #f)
       ((TyVar) #f)
       ((LBRACE TyRow_opt RBRACE) #f)
       ((LBRACKET SigExp RBRACKET) #f)
       ((LPAREN Ty RPAREN) #f))
      (TyRow_opt
       ((TyRow) #f)
       (() #f))
      (TyRow
       ((Label COLON Ty CommaTyRow_opt) #f))
      (CommaTyRow_opt
       ((COMMA TyRow) #f)
       (() #f))
      (TyVarSeq
       ((TyVarSeq1) #f)
       (() #f))
      (TyVarSeq1
       ((TyVar) #f)
       ((LPAREN TyVarComma_seq1 RPAREN) #f))
      (TyVarComma_seq1
       ((TyVar COMMA TyVarComma_seq1) #f)
       ((TyVar) #f))
      (LongTyConEqnTail
       ((LongTypeIdent) #f)
       ((LongTyConEqn) #f))
      (LongTyConEqn
       ((LongTypeIdent EQUALS LongTyConEqnTail) #f))
      (LongModIdEqnTail
       ((LongModId) #f)
       ((LongModIdEqn) #f))
      (LongModIdEqn
       ((LongModId EQUALS LongModIdEqnTail) #f))
      (LongModIdEqnWithLoc
       ((LongModIdEqn) #f))
      (Spec
       ((Spec KWSpec) #f)
       ((Spec SHARING TYPE LongTyConEqn) #f)
       ((Spec SHARING LongModIdEqnWithLoc) #f)
       ((Spec SEMICOLON) #f)
       (() #f))
      (TopSpecFile
       ;;((Spec_seq EOF) #f))
       ((Spec_seq) #f))
      (SigFile
       ((SIGNATURE SigId EQUALS SigExp SemiEof) #f)
       ;;((CoreSpec_seq EOF) #f))
       ((CoreSpec_seq) #f))
      (Spec_seq
       ((Spec_seq KWSpec) #f)
       ((Spec_seq SEMICOLON) #f)
       (() #f))
      (CoreSpec_seq
       ((CoreSpec_seq KWCoreSpec) #f)
       ((CoreSpec_seq SEMICOLON) #f)
       (() #f))
      (KWSpec
       ((KWCoreSpec) #f)
       ((KWModuleSpec) #f))
      (KWCoreSpec
       ((VAL TyVarSeq ValDesc) #f)
       ((PRIM_VAL PrimValBind) #f)
       ((PRIM_VAL TyVarSeq1 PrimValBind) #f)
       ((TYPE TypBind) #f)
       ((TYPE TypDesc) #f)
       ((EQTYPE TypDesc) #f)
       ((PRIM_REFTYPE TypDesc) #f)
       ((DATATYPE DatBind_0 WithType_opt) #f)
       ((DATATYPE DatBind_n WithType_opt) #f)
       ((DATATYPE TyCon EQUALS DATATYPE TyConPath) #f)
       ((EXCEPTION ExDesc) #f)
       ((LOCAL Spec IN Spec END) #f)
       ((OPEN LongModIdInfo_seq1) #f)
       ((INFIX DIGIT_opt EqIdent_seq1) #f)
       ((INFIXR DIGIT_opt EqIdent_seq1) #f)
       ((NONFIX EqIdent_seq1) #f))
      (SigId_seq2
       ((SigId SigId_seq2) #f)
       ((SigId SigId) #f))
      (KWModuleSpec
       ((STRUCTURE ModDesc_seq1) #f)
       ((FUNCTOR FunDesc_seq1) #f)
       ((INCLUDE SigExp) #f)
       ((INCLUDE SigId_seq2) #f)
       ((SIGNATURE SigBind_seq1) #f))
      (ValDesc
       ((OpIdent COLON Ty AndValDesc_opt) #f))
      (AndValDesc_opt
       ((AND ValDesc) #f) (() #f))
      (TypDesc
       ((TyVarSeq TyCon AndTypDesc_opt) #f))
      (AndTypDesc_opt
       ((AND TypDesc) #f)
       (() #f))
      (ModBind_seq1
       ((ModId OptConEqualsModExp AndModBind_opt) #f)
       ((ModId AS SigExp EQUALS Exp AndModBind_opt) #f))
      (AndModBind_opt
       ((AND ModBind_seq1) #f)
       (() #f))
      (OptConEqualsModExp
       ((EQUALS ModExp) #f)
       ((COLON SigExp EQUALS ModExp) #f)
       ((COLONGT SigExp EQUALS ModExp) #f))
      (FunBind_seq1
       ((ModId AS SigExp EQUALS Exp AndFunBind_opt) #f)
       ((ModId OptConEqualsModExp AndFunBind_opt) #f)
       ((ModId LPAREN ModId COLON SigExp RPAREN FunBindBody AndFunBind_opt) #f)
       ((ModId LPAREN Spec RPAREN OptConEqualsModExp AndFunBind_opt) #f)
       ((ModId ModId COLON SigExp FunBindBody AndFunBind_opt) #f)
       ((ModId SIG Spec END OptConEqualsModExp AndFunBind_opt) #f))
      (AndFunBind_opt
       ((AND FunBind_seq1) #f)
       (() #f))
      (SigBind_seq1
       ((SigId EQUALS SigExp AndSigBind_opt) #f))
      (AndSigBind_opt
       ((AND SigBind_seq1) #f)
       (() #f))
      (FunBindBody
       ((OptConEqualsModExp) #f)
       ((LPAREN ModId COLON SigExp RPAREN FunBindBody) #f)
       ((ModId COLON SigExp FunBindBody) #f))
      (AtModExp
       ((STRUCT Dec END) #f)
       ((LongModId) #f)
       ((LET Dec IN ModExp END) #f)
       ((LPAREN ModExp RPAREN) #f)
       ((LPAREN Dec RPAREN) #f))
      (ModExp
       ((AtModExp_seq1) #f)
       ((ModExp COLONGT SigExp) #f)
       ((ModExp COLON SigExp) #f)
       ((FUNCTOR ModId COLON SigExp DARROW ModExp) #f)
       ((FUNCTOR LPAREN ModId COLON SigExp RPAREN DARROW ModExp) #f)
       ((REC LPAREN ModId COLON SigExp RPAREN ModExp) #f))
      (AtModExp_seq1
       ((AtModExp AtModExp_seq1) #f)
       ((AtModExp) #f))
      (ModDesc_seq1
       ((ModId COLON SigExp AndModDesc_opt) #f))
      (AndModDesc_opt
       ((AND ModDesc_seq1) #f)
       (() #f))
      (FunDescBody
       ((COLON SigExp) #f)
       ((LPAREN ModId COLON SigExp RPAREN FunDescBody) #f)
       ((ModId COLON SigExp FunDescBody) #f))
      (FunDesc_seq1
       ((ModId FunDescBody AndFunDesc_opt) #f))
      (AndFunDesc_opt
       ((AND FunDesc_seq1) #f)
       (() #f))
      (SigExp
       ((SIG Spec END) #f)
       ((SigId) #f)
       ((SigExp WHERE WhereType) #f)
       ((FUNCTOR LPAREN ModId COLON SigExp RPAREN ARROW SigExp) #f)
       ((FUNCTOR ModId COLON SigExp ARROW SigExp) #f)
       ((REC LPAREN ModId COLON SigExp RPAREN SigExp) #f))
      (WhereType
       ((TYPE TyVarSeq LongTypeIdent EQUALS Ty AndWhereType_opt) #f))
      (AndWhereType_opt
       ((AND WhereType) #f)
       (() #f)))))
  
  (define mosml-files
    `("/home/sowens/mosml/examples/calc/calc.sml"
      "/home/sowens/mosml/examples/cgi/cgiex1.sml"
      "/home/sowens/mosml/examples/cgi/cgiex2.sml"
      "/home/sowens/mosml/examples/cgi/cgitest.sml"
      "/home/sowens/mosml/examples/lexyacc/Data.sml"
      "/home/sowens/mosml/examples/lexyacc/Main.sml"
      "/home/sowens/mosml/examples/manual/Evaluate.sml"
      "/home/sowens/mosml/examples/manual/Expr.sml"
      "/home/sowens/mosml/examples/manual/Reduce.sml"
      "/home/sowens/mosml/examples/mls/mls.sml"
      "/home/sowens/mosml/examples/modules/array.sml"
      "/home/sowens/mosml/examples/modules/bootstrap.sml"
      "/home/sowens/mosml/examples/modules/choice.sml"
      "/home/sowens/mosml/examples/modules/collect.sml"
      "/home/sowens/mosml/examples/modules/matrix.sml"
      "/home/sowens/mosml/examples/modules/poly.sml"
      "/home/sowens/mosml/examples/modules/recursion.sml"
      "/home/sowens/mosml/examples/modules/sieve.sml"
      "/home/sowens/mosml/examples/parsercomb/Parsercomb.sml"
      "/home/sowens/mosml/examples/paulson/sample.sml"
      "/home/sowens/mosml/examples/paulson/sample10.sml"
      "/home/sowens/mosml/examples/paulson/sample2.sml"
      "/home/sowens/mosml/examples/paulson/sample3.sml"
      "/home/sowens/mosml/examples/paulson/sample4.sml"
      "/home/sowens/mosml/examples/paulson/sample5.sml"
      "/home/sowens/mosml/examples/paulson/sample7.sml"
      "/home/sowens/mosml/examples/paulson/sample8.sml"
      "/home/sowens/mosml/examples/paulson/sample9.sml"
      "/home/sowens/mosml/examples/paulson/test10.sml"
      "/home/sowens/mosml/examples/pretty/ppexpr.sml"
      "/home/sowens/mosml/examples/pretty/pproman.sml"
      "/home/sowens/mosml/examples/small/perms.sml"
      "/home/sowens/mosml/examples/small/countperms.sml"
      "/home/sowens/mosml/examples/small/countqueens.sml"
      "/home/sowens/mosml/examples/small/queens.sml"
      "/home/sowens/mosml/examples/small/roman.sml"
      "/home/sowens/mosml/examples/small/subsets.sml"
      "/home/sowens/mosml/examples/small/subsum.sml"
      "/home/sowens/mosml/examples/units/Evaluate.sml"
      "/home/sowens/mosml/examples/units/Expr.sml"
      "/home/sowens/mosml/examples/units/Reduce.sml"
      "/home/sowens/mosml/examples/units/Test.sml"
      "/home/sowens/mosml/examples/weak/hashcons.sml"
      "/home/sowens/mosml/examples/webserver/echoserver.sml"
      "/home/sowens/mosml/examples/webserver/minimalserver.sml"
      "/home/sowens/mosml/examples/webserver/mosmlserver.sml"
      "/home/sowens/mosml/examples/webserver/useit.sml"
      "/home/sowens/mosml/src/compiler/Asynt.sml"
      "/home/sowens/mosml/src/compiler/Arg.sml"
      "/home/sowens/mosml/src/compiler/test/A.sml"
      "/home/sowens/mosml/src/compiler/test/B.sml"
      "/home/sowens/mosml/src/compiler/test/C.sml"
      "/home/sowens/mosml/src/compiler/test/applic.sml"
      "/home/sowens/mosml/src/compiler/test/array.sml"
      "/home/sowens/mosml/src/compiler/test/array2.sml"
      "/home/sowens/mosml/src/compiler/test/church-functors-mosml.sml"
      "/home/sowens/mosml/src/compiler/test/coerce.sml"
      "/home/sowens/mosml/src/compiler/test/eq.sml"
      "/home/sowens/mosml/src/compiler/test/evalorder.sml"
      "/home/sowens/mosml/src/compiler/test/gnvsap.sml"
      "/home/sowens/mosml/src/compiler/test/greederr.sml"
      "/home/sowens/mosml/src/compiler/test/hiorder.sml"
      "/home/sowens/mosml/src/compiler/test/inf.sml"
      "/home/sowens/mosml/src/compiler/test/link.sml"
      "/home/sowens/mosml/src/compiler/test/matcherr.sml"
      "/home/sowens/mosml/src/compiler/test/matchsuc.sml"
      "/home/sowens/mosml/src/compiler/test/modres.sml"
      "/home/sowens/mosml/src/compiler/test/okasaki.sml"
      "/home/sowens/mosml/src/compiler/test/opnlcl.sml"
      "/home/sowens/mosml/src/compiler/test/opntop.sml"
      "/home/sowens/mosml/src/compiler/test/poly.sml"
      "/home/sowens/mosml/src/compiler/test/recmod.sml"
      "/home/sowens/mosml/src/compiler/test/refeq.sml"
      "/home/sowens/mosml/src/compiler/test/sample10.sml"
      "/home/sowens/mosml/src/compiler/test/sample2.sml"
      "/home/sowens/mosml/src/compiler/test/sample3.sml"
      "/home/sowens/mosml/src/compiler/test/sample4.sml"
      "/home/sowens/mosml/src/compiler/test/sample5.sml"
      "/home/sowens/mosml/src/compiler/test/sample7.sml"
      "/home/sowens/mosml/src/compiler/test/sample8.sml"
      "/home/sowens/mosml/src/compiler/test/sample9.sml"
      "/home/sowens/mosml/src/compiler/test/scope.sml"
      "/home/sowens/mosml/src/compiler/test/sharing.sml"
      "/home/sowens/mosml/src/compiler/test/sieve.sml"
      "/home/sowens/mosml/src/compiler/test/sigcon.sml"
      "/home/sowens/mosml/src/compiler/test/test.sml"
      "/home/sowens/mosml/src/compiler/test/test10.sml"
      "/home/sowens/mosml/src/compiler/test/where.sml"
      "/home/sowens/mosml/src/compiler/test/wheretyp.sml"
      "/home/sowens/mosml/src/compiler/Asyntfn.sml"
      "/home/sowens/mosml/src/compiler/Back.sml"
      "/home/sowens/mosml/src/compiler/Buffcode.sml"
      "/home/sowens/mosml/src/compiler/Code_dec.sml"
      "/home/sowens/mosml/src/compiler/Compiler.sml"
      "/home/sowens/mosml/src/compiler/Const.sml"
      "/home/sowens/mosml/src/compiler/Elab.sml"
      "/home/sowens/mosml/src/compiler/Emit_phr.sml"
      "/home/sowens/mosml/src/compiler/Emitcode.sml"
      "/home/sowens/mosml/src/compiler/Exec_phr.sml"
      "/home/sowens/mosml/src/compiler/Fnlib.sml"
      "/home/sowens/mosml/src/compiler/Front.sml"
      "/home/sowens/mosml/src/compiler/Globals.sml"
      "/home/sowens/mosml/src/compiler/Hasht.sml"
      "/home/sowens/mosml/src/compiler/Infixres.sml"
      "/home/sowens/mosml/src/compiler/Infixst.sml"
      "/home/sowens/mosml/src/compiler/Instruct.sml"
      "/home/sowens/mosml/src/compiler/Labels.sml"
      "/home/sowens/mosml/src/compiler/Lambda.sml"
      "/home/sowens/mosml/src/compiler/Link.sml"
      "/home/sowens/mosml/src/compiler/Load_phr.sml"
      "/home/sowens/mosml/src/compiler/Location.sml"
      "/home/sowens/mosml/src/compiler/Mainc.sml"
      "/home/sowens/mosml/src/compiler/Mainl.sml"
      "/home/sowens/mosml/src/compiler/Maint.sml"
      "/home/sowens/mosml/src/compiler/Match.sml"
      "/home/sowens/mosml/src/compiler/Memory.sml"
      "/home/sowens/mosml/src/compiler/Miscsys.sml"
      "/home/sowens/mosml/src/compiler/Mixture.sml"
      "/home/sowens/mosml/src/compiler/Ovlres.sml"
      "/home/sowens/mosml/src/compiler/Patch.sml"
      "/home/sowens/mosml/src/compiler/Pr_lam.sml"
      "/home/sowens/mosml/src/compiler/Pr_zam.sml"
      "/home/sowens/mosml/src/compiler/Prim.sml"
      "/home/sowens/mosml/src/compiler/Prim_opc.sml"
      "/home/sowens/mosml/src/compiler/Primdec.sml"
      "/home/sowens/mosml/src/compiler/Printexc.sml"
      "/home/sowens/mosml/src/compiler/Readword.sml"
      "/home/sowens/mosml/src/compiler/Reloc.sml"
      "/home/sowens/mosml/src/compiler/Rtvals.sml"
      "/home/sowens/mosml/src/compiler/Sigmtch.sml"
      "/home/sowens/mosml/src/compiler/Smlexc.sml"
      "/home/sowens/mosml/src/compiler/Smlperv.sml"
      "/home/sowens/mosml/src/compiler/Smlprim.sml"
      "/home/sowens/mosml/src/compiler/Smltop.sml"
      "/home/sowens/mosml/src/compiler/Sort.sml"
      "/home/sowens/mosml/src/compiler/Stack.sml"
      "/home/sowens/mosml/src/compiler/Symtable.sml"
      "/home/sowens/mosml/src/compiler/Synchk.sml"
      "/home/sowens/mosml/src/compiler/Tr_const.sml"
      "/home/sowens/mosml/src/compiler/Tr_env.sml"
      "/home/sowens/mosml/src/compiler/Types.sml"
      "/home/sowens/mosml/src/compiler/Units.sml"
      "/home/sowens/mosml/src/compiler/Filename.sml"
      "/home/sowens/mosml/src/compiler/Config.sml"
      "/home/sowens/mosml/src/compiler/Parser.sml"
      "/home/sowens/mosml/src/compiler/Lexer.sml"
      "/home/sowens/mosml/src/compiler/Opcodes.sml"
      "/home/sowens/mosml/src/compiler/Predef.sml"
      "/home/sowens/mosml/src/compiler/Prim_c.sml"
      "/home/sowens/mosml/src/convert/Convert.sml"
      "/home/sowens/mosml/src/convert/GenPm.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Asynt.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Database.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Hasht.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Htmlsigs.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Parsspec.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Printbase.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Stack.sml"
      "/home/sowens/mosml/src/doc/helpsigs/Texsigs.sml"
      "/home/sowens/mosml/src/doc/helpsigs/makebase.sml"
      "/home/sowens/mosml/src/dynlibs/crypt/crypt.sml"
      "/home/sowens/mosml/src/dynlibs/interface/smlside.sml"
      "/home/sowens/mosml/src/dynlibs/interface/smlside_mac.sml"
      "/home/sowens/mosml/src/dynlibs/intinf/IntInf.sml"
      "/home/sowens/mosml/src/dynlibs/intinf/fac.sml"
      "/home/sowens/mosml/src/dynlibs/intinf/testintinf.sml"
      "/home/sowens/mosml/src/dynlibs/intinf/testintinf_mac.sml"
      "/home/sowens/mosml/src/dynlibs/mgd/Graphs.sml"
      "/home/sowens/mosml/src/dynlibs/mgd/testgdimage.sml"
      "/home/sowens/mosml/src/dynlibs/mgdbm/example1.sml"
      "/home/sowens/mosml/src/dynlibs/mgdbm/example2.sml"
      "/home/sowens/mosml/src/dynlibs/mgdbm/testgdbm.sml"
      "/home/sowens/mosml/src/dynlibs/mmysql/testmysql.sml"
      "/home/sowens/mosml/src/dynlibs/mpq/testpsql.sml"
      "/home/sowens/mosml/src/dynlibs/mregex/testregex.sml"
      "/home/sowens/mosml/src/dynlibs/mregex/testregex_mac.sml"
      "/home/sowens/mosml/src/dynlibs/msocket/testclient.sml"
      "/home/sowens/mosml/src/dynlibs/msocket/testserver.sml"
      "/home/sowens/mosml/src/dynlibs/msocket/testsocket.sml"
      "/home/sowens/mosml/src/dynlibs/munix/testunix.sml"
      "/home/sowens/mosml/src/launch/testprog.sml"
      "/home/sowens/mosml/src/lex/Gram_aux.sml"
      "/home/sowens/mosml/src/lex/Lexgen.sml"
      "/home/sowens/mosml/src/lex/Mainlex.sml"
      "/home/sowens/mosml/src/lex/Output.sml"
      "/home/sowens/mosml/src/lex/Scan_aux.sml"
      "/home/sowens/mosml/src/lex/Syntax.sml"
      "/home/sowens/mosml/src/lex/Grammar.sml"
      "/home/sowens/mosml/src/lex/Scanner.sml"
      "/home/sowens/mosml/src/mosmllib/test/callback/testcallback.sml"
      "/home/sowens/mosml/src/mosmllib/test/callback/testcallbackmac.sml"
      "/home/sowens/mosml/src/mosmllib/test/array.sml"
      "/home/sowens/mosml/src/mosmllib/test/array2.sml"
      "/home/sowens/mosml/src/mosmllib/test/arraysort.sml"
      "/home/sowens/mosml/src/mosmllib/test/auxil.sml"
      "/home/sowens/mosml/src/mosmllib/test/bytechar.sml"
      "/home/sowens/mosml/src/mosmllib/test/bytecmac.sml"
      "/home/sowens/mosml/src/mosmllib/test/callback.sml"
      "/home/sowens/mosml/src/mosmllib/test/cmdline.sml"
      "/home/sowens/mosml/src/mosmllib/test/date.sml"
      "/home/sowens/mosml/src/mosmllib/test/dospath.sml"
      "/home/sowens/mosml/src/mosmllib/test/filesmac.sml"
      "/home/sowens/mosml/src/mosmllib/test/filesys.sml"
      "/home/sowens/mosml/src/mosmllib/test/general.sml"
      "/home/sowens/mosml/src/mosmllib/test/generalmac.sml"
      "/home/sowens/mosml/src/mosmllib/test/int.sml"
      "/home/sowens/mosml/src/mosmllib/test/list.sml"
      "/home/sowens/mosml/src/mosmllib/test/listpair.sml"
      "/home/sowens/mosml/src/mosmllib/test/listsort.sml"
      "/home/sowens/mosml/src/mosmllib/test/macpath.sml"
      "/home/sowens/mosml/src/mosmllib/test/math.sml"
      "/home/sowens/mosml/src/mosmllib/test/mosml.sml"
      "/home/sowens/mosml/src/mosmllib/test/polyhash.sml"
      "/home/sowens/mosml/src/mosmllib/test/real.sml"
      "/home/sowens/mosml/src/mosmllib/test/string.sml"
      "/home/sowens/mosml/src/mosmllib/test/stringcvt.sml"
      "/home/sowens/mosml/src/mosmllib/test/stringmac.sml"
      "/home/sowens/mosml/src/mosmllib/test/substring.sml"
      "/home/sowens/mosml/src/mosmllib/test/susp.sml"
      "/home/sowens/mosml/src/mosmllib/test/test.sml"
      "/home/sowens/mosml/src/mosmllib/test/testmac.sml"
      "/home/sowens/mosml/src/mosmllib/test/testrun.sml"
      "/home/sowens/mosml/src/mosmllib/test/textio.sml"
      "/home/sowens/mosml/src/mosmllib/test/time.sml"
      "/home/sowens/mosml/src/mosmllib/test/timer.sml"
      "/home/sowens/mosml/src/mosmllib/test/unixpath.sml"
      "/home/sowens/mosml/src/mosmllib/test/vector.sml"
      "/home/sowens/mosml/src/mosmllib/test/weak.sml"
      "/home/sowens/mosml/src/mosmllib/test/word.sml"
      "/home/sowens/mosml/src/mosmllib/test/word8.sml"
      "/home/sowens/mosml/src/mosmllib/test/word8array.sml"
      "/home/sowens/mosml/src/mosmllib/test/word8vector.sml"
      "/home/sowens/mosml/src/mosmllib/AppleScript.sml"
      "/home/sowens/mosml/src/mosmllib/Array2.sml"
      "/home/sowens/mosml/src/mosmllib/Arraysort.sml"
      "/home/sowens/mosml/src/mosmllib/BasicIO.sml"
      "/home/sowens/mosml/src/mosmllib/BinIO.sml"
      "/home/sowens/mosml/src/mosmllib/Binarymap.sml"
      "/home/sowens/mosml/src/mosmllib/Binaryset.sml"
      "/home/sowens/mosml/src/mosmllib/Bool.sml"
      "/home/sowens/mosml/src/mosmllib/Byte.sml"
      "/home/sowens/mosml/src/mosmllib/Callback.sml"
      "/home/sowens/mosml/src/mosmllib/Char.sml"
      "/home/sowens/mosml/src/mosmllib/CharArray.sml"
      "/home/sowens/mosml/src/mosmllib/CharVector.sml"
      "/home/sowens/mosml/src/mosmllib/CommandLine.sml"
      "/home/sowens/mosml/src/mosmllib/Date.sml"
      "/home/sowens/mosml/src/mosmllib/Dynarray.sml"
      "/home/sowens/mosml/src/mosmllib/Dynlib.sml"
      "/home/sowens/mosml/src/mosmllib/Gdbm.sml"
      "/home/sowens/mosml/src/mosmllib/Gdimage.sml"
      "/home/sowens/mosml/src/mosmllib/IO.sml"
      "/home/sowens/mosml/src/mosmllib/Intmap.sml"
      "/home/sowens/mosml/src/mosmllib/Intset.sml"
      "/home/sowens/mosml/src/mosmllib/Lexing.sml"
      "/home/sowens/mosml/src/mosmllib/List.sml"
      "/home/sowens/mosml/src/mosmllib/ListPair.sml"
      "/home/sowens/mosml/src/mosmllib/Listsort.sml"
      "/home/sowens/mosml/src/mosmllib/Location.sml"
      "/home/sowens/mosml/src/mosmllib/Math.sml"
      "/home/sowens/mosml/src/mosmllib/Misc.sml"
      "/home/sowens/mosml/src/mosmllib/Mosmlcgi.sml"
      "/home/sowens/mosml/src/mosmllib/Mosmlcookie.sml"
      "/home/sowens/mosml/src/mosmllib/Msp.sml"
      "/home/sowens/mosml/src/mosmllib/Mysql.sml"
      "/home/sowens/mosml/src/mosmllib/NJ93.sml"
      "/home/sowens/mosml/src/mosmllib/Nonstdio.sml"
      "/home/sowens/mosml/src/mosmllib/OS.sml"
      "/home/sowens/mosml/src/mosmllib/Obj.sml"
      "/home/sowens/mosml/src/mosmllib/Option.sml"
      "/home/sowens/mosml/src/mosmllib/PP.sml"
      "/home/sowens/mosml/src/mosmllib/Parsing.sml"
      "/home/sowens/mosml/src/mosmllib/Polygdbm.sml"
      "/home/sowens/mosml/src/mosmllib/Polyhash.sml"
      "/home/sowens/mosml/src/mosmllib/Postgres.sml"
      "/home/sowens/mosml/src/mosmllib/Random.sml"
      "/home/sowens/mosml/src/mosmllib/Real.sml"
      "/home/sowens/mosml/src/mosmllib/Regex.sml"
      "/home/sowens/mosml/src/mosmllib/SML90.sml"
      "/home/sowens/mosml/src/mosmllib/Signal.sml"
      "/home/sowens/mosml/src/mosmllib/Socket.sml"
      "/home/sowens/mosml/src/mosmllib/Splaymap.sml"
      "/home/sowens/mosml/src/mosmllib/Splayset.sml"
      "/home/sowens/mosml/src/mosmllib/Splaytree.sml"
      "/home/sowens/mosml/src/mosmllib/String.sml"
      "/home/sowens/mosml/src/mosmllib/StringCvt.sml"
      "/home/sowens/mosml/src/mosmllib/Substring.sml"
      "/home/sowens/mosml/src/mosmllib/Susp.sml"
      "/home/sowens/mosml/src/mosmllib/TextIO.sml"
      "/home/sowens/mosml/src/mosmllib/Time.sml"
      "/home/sowens/mosml/src/mosmllib/Timer.sml"
      "/home/sowens/mosml/src/mosmllib/Unix.sml"
      "/home/sowens/mosml/src/mosmllib/Word8.sml"
      "/home/sowens/mosml/src/mosmllib/Array.sml"
      "/home/sowens/mosml/src/mosmllib/FileSys.sml"
      "/home/sowens/mosml/src/mosmllib/Help.sml"
      "/home/sowens/mosml/src/mosmllib/Int.sml"
      "/home/sowens/mosml/src/mosmllib/Mosml.sml"
      "/home/sowens/mosml/src/mosmllib/Path.sml"
      "/home/sowens/mosml/src/mosmllib/Process.sml"
      "/home/sowens/mosml/src/mosmllib/Strbase.sml"
      "/home/sowens/mosml/src/mosmllib/Vector.sml"
      "/home/sowens/mosml/src/mosmllib/Weak.sml"
      "/home/sowens/mosml/src/mosmllib/Word.sml"
      "/home/sowens/mosml/src/mosmllib/Word8Array.sml"
      "/home/sowens/mosml/src/mosmllib/Word8Vector.sml"
      "/home/sowens/mosml/src/mosmlpm/test/B-sig.sml"
      "/home/sowens/mosml/src/mosmlpm/test/A.sml"
      "/home/sowens/mosml/src/mosmlpm/test/B.sml"
      "/home/sowens/mosml/src/mosmlpm/test/C.sml"
      "/home/sowens/mosml/src/mosmlpm/test/D.sml"
      "/home/sowens/mosml/src/mosmlpm/ArgParse.sml"
      "/home/sowens/mosml/src/mosmlpm/Compilerinterface.sml"
      "/home/sowens/mosml/src/mosmlpm/PMBasic.sml"
      "/home/sowens/mosml/src/mosmlpm/PMCompile.sml"
      "/home/sowens/mosml/src/mosmlpm/Parsercomb.sml"
      "/home/sowens/mosml/src/mosmlpm/Systemcompile.sml"
      "/home/sowens/mosml/src/mosmlpm/mosmlpm.sml"
      "/home/sowens/mosml/src/test/mosmlyac/test3aux.sml"
      "/home/sowens/mosml/src/test/mosmlyac/test3main.sml"
      "/home/sowens/mosml/src/test/constfai.sml"
      "/home/sowens/mosml/src/test/constsuc.sml"
      "/home/sowens/mosml/src/test/maxconst.sml"
      "/home/sowens/mosml/src/test/ovlfail.sml"
      "/home/sowens/mosml/src/test/ovlsucc.sml"
      "/home/sowens/mosml/src/test/recfail.sml"
      "/home/sowens/mosml/src/test/recsucc.sml"
      "/home/sowens/mosml/src/test/test.sml"
      "/home/sowens/mosml/src/test/test1.sml"
      "/home/sowens/mosml/src/test/test2.sml"
      "/home/sowens/mosml/src/test/test3.sml"
      "/home/sowens/mosml/src/test/test4.sml"
      "/home/sowens/mosml/src/test/test5.sml"
      "/home/sowens/mosml/src/test/test6.sml"
      "/home/sowens/mosml/src/test/test7.sml"
      "/home/sowens/mosml/src/test/test8.sml"
      "/home/sowens/mosml/src/test/test9.sml"
      "/home/sowens/mosml/src/test/testa.sml"
      "/home/sowens/mosml/src/test/testb.sml"
      "/home/sowens/mosml/src/test/testc.sml"
      "/home/sowens/mosml/src/test/testcon.sml"
      "/home/sowens/mosml/src/test/testd.sml"
      "/home/sowens/mosml/src/test/teste.sml"
      "/home/sowens/mosml/src/test/testint.sml"
      "/home/sowens/mosml/src/test/testmatc.sml"
      "/home/sowens/mosml/src/test/testpp.sml"
      "/home/sowens/mosml/src/test/testprs.sml"
      "/home/sowens/mosml/src/test/testsyn.sml"
      "/home/sowens/mosml/src/test/testty.sml"
      "/home/sowens/mosml/src/test/typerr.sml"
      "/home/sowens/mosml/src/toolssrc/Maine.sml"
      "/home/sowens/mosml/src/toolssrc/Smltope.sml"
      "/home/sowens/mosml/src/toolssrc/cutdeps.sml"
      "/home/sowens/mosml/src/toolssrc/Deppars.sml"
      "/home/sowens/mosml/src/toolssrc/Deplex.sml"
      "/home/sowens/mosml/src/toolssrc/Mosmldep.sml"))

  (define hol-files
    `("/home/sowens/hol/examples/Thery.sml"
      "/home/sowens/hol/examples/MLsyntax/MLScript.sml"
      "/home/sowens/hol/examples/autopilot.sml"
      "/home/sowens/hol/examples/euclid.sml"
      "/home/sowens/hol/examples/fol.sml"
      "/home/sowens/hol/examples/root2.sml"
      "/home/sowens/hol/examples/taut.sml"
      "/home/sowens/hol/examples/tempScript.sml"
      "/home/sowens/hol/examples/RSA/binomialScript.sml"
      "/home/sowens/hol/examples/RSA/congruentScript.sml"
      "/home/sowens/hol/examples/RSA/dividesScript.sml"
      "/home/sowens/hol/examples/RSA/factorialScript.sml"
      "/home/sowens/hol/examples/RSA/fermatScript.sml"
      "/home/sowens/hol/examples/RSA/gcdScript.sml"
      "/home/sowens/hol/examples/RSA/powerScript.sml"
      "/home/sowens/hol/examples/RSA/primeScript.sml"
      "/home/sowens/hol/examples/RSA/rsaScript.sml"
      "/home/sowens/hol/examples/RSA/summationScript.sml"
      "/home/sowens/hol/examples/Rijndael/RoundOpScript.sml"
      "/home/sowens/hol/examples/Rijndael/sboxScript.sml"
      "/home/sowens/hol/examples/Sugar2/PathScript.sml"
      "/home/sowens/hol/examples/Sugar2/Sugar2Script.sml"
      "/home/sowens/hol/examples/Sugar2/Sugar2SemanticsScript.sml"
      "/home/sowens/hol/examples/arm6/armScript.sml"
      "/home/sowens/hol/examples/arm6/coreScript.sml"
      "/home/sowens/hol/examples/arm6/correctScript.sml"
      "/home/sowens/hol/examples/arm6/lemmasLib.sml"
      "/home/sowens/hol/examples/arm6/lemmasScript.sml"
      "/home/sowens/hol/examples/arm6/onestepScript.sml"
      "/home/sowens/hol/examples/bmark/Bmark.sml"
      "/home/sowens/hol/examples/ind_def/algebraScript.sml"
      "/home/sowens/hol/examples/ind_def/clScript.sml"
      "/home/sowens/hol/examples/ind_def/milScript.sml"
      "/home/sowens/hol/examples/ind_def/opsemScript.sml"
      "/home/sowens/hol/examples/lambda/dBScript.sml"
      "/home/sowens/hol/examples/lambda/ncScript.sml"
      "/home/sowens/hol/examples/miller/RSA/binomialScript.sml"
      "/home/sowens/hol/examples/miller/RSA/congruentScript.sml"
      "/home/sowens/hol/examples/miller/RSA/fermatScript.sml"
      "/home/sowens/hol/examples/miller/RSA/powerScript.sml"
      "/home/sowens/hol/examples/miller/RSA/rsaScript.sml"
      "/home/sowens/hol/examples/miller/RSA/summationScript.sml"
      "/home/sowens/hol/examples/miller/formalize/boolContext.sml"
      "/home/sowens/hol/examples/miller/formalize/extra_boolScript.sml"
      "/home/sowens/hol/examples/miller/formalize/extra_listScript.sml"
      "/home/sowens/hol/examples/miller/formalize/extra_numScript.sml"
      "/home/sowens/hol/examples/miller/formalize/extra_pred_setScript.sml"
      "/home/sowens/hol/examples/miller/formalize/extra_pred_setTools.sml"
      "/home/sowens/hol/examples/miller/formalize/extra_realScript.sml"
      "/home/sowens/hol/examples/miller/formalize/formalizeUseful.sml"
      "/home/sowens/hol/examples/miller/formalize/listContext.sml"
      "/home/sowens/hol/examples/miller/formalize/measureScript.sml"
      "/home/sowens/hol/examples/miller/formalize/numContext.sml"
      "/home/sowens/hol/examples/miller/formalize/orderScript.sml"
      "/home/sowens/hol/examples/miller/formalize/pred_setContext.sml"
      "/home/sowens/hol/examples/miller/formalize/probabilityScript.sml"
      "/home/sowens/hol/examples/miller/formalize/realContext.sml"
      "/home/sowens/hol/examples/miller/formalize/sequenceScript.sml"
      "/home/sowens/hol/examples/miller/formalize/sequenceTools.sml"
      "/home/sowens/hol/examples/miller/groups/abelian_groupScript.sml"
      "/home/sowens/hol/examples/miller/groups/arithContext.sml"
      "/home/sowens/hol/examples/miller/groups/extra_arithScript.sml"
      "/home/sowens/hol/examples/miller/groups/extra_binomialScript.sml"
      "/home/sowens/hol/examples/miller/groups/finite_groupContext.sml"
      "/home/sowens/hol/examples/miller/groups/finite_groupScript.sml"
      "/home/sowens/hol/examples/miller/groups/ftaScript.sml"
      "/home/sowens/hol/examples/miller/groups/groupContext.sml"
      "/home/sowens/hol/examples/miller/groups/groupScript.sml"
      "/home/sowens/hol/examples/miller/groups/mult_groupContext.sml"
      "/home/sowens/hol/examples/miller/groups/mult_groupScript.sml"
      "/home/sowens/hol/examples/miller/groups/num_polyScript.sml"
      "/home/sowens/hol/examples/miller/ho_prover/ho_basicTools.sml"
      "/home/sowens/hol/examples/miller/ho_prover/ho_discrimTools.sml"
      "/home/sowens/hol/examples/miller/ho_prover/ho_proverTools.sml"
      "/home/sowens/hol/examples/miller/ho_prover/ho_proverUseful.sml"
      "/home/sowens/hol/examples/miller/ho_prover/skiScript.sml"
      "/home/sowens/hol/examples/miller/ho_prover/skiTools.sml"
      "/home/sowens/hol/examples/miller/ho_prover/unifyTools.sml"
      "/home/sowens/hol/examples/miller/miller/miller_rabinScript.sml"
      "/home/sowens/hol/examples/miller/miller/miller_rabinTools.sml"
      "/home/sowens/hol/examples/miller/miller/miller_rabin_mlScript.sml"
      "/home/sowens/hol/examples/miller/prob/probLib.sml"
      "/home/sowens/hol/examples/miller/prob/probScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_algebraScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_bernoulliScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_binomialScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_canonScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_canonTools.sml"
      "/home/sowens/hol/examples/miller/prob/prob_diceScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_geometricScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_pseudoScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_pseudoTools.sml"
      "/home/sowens/hol/examples/miller/prob/prob_trichotomyScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_trichotomyTools.sml"
      "/home/sowens/hol/examples/miller/prob/prob_uniformScript.sml"
      "/home/sowens/hol/examples/miller/prob/prob_uniformTools.sml"
      "/home/sowens/hol/examples/miller/prob/prob_walkScript.sml"
      "/home/sowens/hol/examples/miller/subtypes/subtypeScript.sml"
      "/home/sowens/hol/examples/miller/subtypes/subtypeTools.sml"
      "/home/sowens/hol/examples/miller/subtypes/subtypeUseful.sml"
      "/home/sowens/hol/examples/parity/PARITY.sml"
      "/home/sowens/hol/help/src/Asynt.sml"
      "/home/sowens/hol/help/src/Database.sml"
      "/home/sowens/hol/help/src/Doc2Html.sml"
      "/home/sowens/hol/help/src/Doc2Tex.sml"
      "/home/sowens/hol/help/src/Doc2Txt.sml"
      "/home/sowens/hol/help/src/Flash.sml"
      "/home/sowens/hol/help/src/HOLPage.sml"
      "/home/sowens/hol/help/src/Hasht.sml"
      "/home/sowens/hol/help/src/Htmlsigs.sml"
      "/home/sowens/hol/help/src/Keepers.sml"
      "/home/sowens/hol/help/src/ParseDoc.sml"
      "/home/sowens/hol/help/src/Parsspec.sml"
      "/home/sowens/hol/help/src/Printbase.sml"
      "/home/sowens/hol/help/src/Stack.sml"
      "/home/sowens/hol/help/src/Symbolic.sml"
      "/home/sowens/hol/help/src/Texsigs.sml"
      "/home/sowens/hol/help/src/makebase.sml"
      "/home/sowens/hol/help/src/Lexer.sml"
      "/home/sowens/hol/help/src/Parser.sml"
      "/home/sowens/hol/src/0/CoreKernel-sig.sml"
      "/home/sowens/hol/src/0/Count.sml"
      "/home/sowens/hol/src/0/Definition-sig.sml"
      "/home/sowens/hol/src/0/Definition.sml"
      "/home/sowens/hol/src/0/Feedback.sml"
      "/home/sowens/hol/src/0/Globals.sml"
      "/home/sowens/hol/src/0/HOLset.sml"
      "/home/sowens/hol/src/0/HolKernel.sml"
      "/home/sowens/hol/src/0/KernelTypes.sml"
      "/home/sowens/hol/src/0/Lexis.sml"
      "/home/sowens/hol/src/0/Lib.sml"
      "/home/sowens/hol/src/0/Net-sig.sml"
      "/home/sowens/hol/src/0/Net.sml"
      "/home/sowens/hol/src/0/Overlay.sml"
      "/home/sowens/hol/src/0/Profile.sml"
      "/home/sowens/hol/src/0/Raw-sig.sml"
      "/home/sowens/hol/src/0/Sig.sml"
      "/home/sowens/hol/src/0/Subst.sml"
      "/home/sowens/hol/src/0/Tag-sig.sml"
      "/home/sowens/hol/src/0/Tag.sml"
      "/home/sowens/hol/src/0/Term-sig.sml"
      "/home/sowens/hol/src/0/Term.sml"
      "/home/sowens/hol/src/0/Theory-sig.sml"
      "/home/sowens/hol/src/0/Theory.sml"
      "/home/sowens/hol/src/0/TheoryPP-sig.sml"
      "/home/sowens/hol/src/0/TheoryPP.sml"
      "/home/sowens/hol/src/0/Thm-sig.sml"
      "/home/sowens/hol/src/0/Thm.sml"
      "/home/sowens/hol/src/0/Type-sig.sml"
      "/home/sowens/hol/src/0/Type.sml"
      "/home/sowens/hol/src/HolBdd/Examples/KatiPuzzle/KatiPuzzleScript.sml"
      "/home/sowens/hol/src/HolBdd/Examples/Solitaire/HexSolitaireScript.sml"
      "/home/sowens/hol/src/HolBdd/Examples/Solitaire/MiniTLHexSolitaireScript.sml"
      "/home/sowens/hol/src/HolBdd/Examples/Solitaire/SolitaireScript.sml"
      "/home/sowens/hol/src/HolBdd/HolBddLib.sml"
      "/home/sowens/hol/src/HolBdd/PrintBdd.sml"
      "/home/sowens/hol/src/HolBdd/Varmap.sml"
      "/home/sowens/hol/src/HolBdd/MachineTransitionScript.sml"
      "/home/sowens/hol/src/HolBdd/MachineTransitionTheory.sml"
      "/home/sowens/hol/src/HolBdd/DerivedBddRules.sml"
      "/home/sowens/hol/src/HolBdd/PrimitiveBddRules.sml"
      "/home/sowens/hol/src/HolSat/SatSolvers.sml"
      "/home/sowens/hol/src/HolSat/normalFormsTest.sml"
      "/home/sowens/hol/src/HolSat/HolSatLib.sml"
      "/home/sowens/hol/src/HolSat/defCNFScript.sml"
      "/home/sowens/hol/src/HolSat/defCNFTheory.sml"
      "/home/sowens/hol/src/HolSat/defCNF.sml"
      "/home/sowens/hol/src/HolSat/normalForms.sml"
      "/home/sowens/hol/src/IndDef/IndDefLib.sml"
      "/home/sowens/hol/src/IndDef/IndDefRules.sml"
      "/home/sowens/hol/src/IndDef/InductiveDefinition.sml"
      "/home/sowens/hol/src/IndDef/examples/algebraScript.sml"
      "/home/sowens/hol/src/IndDef/examples/clScript.sml"
      "/home/sowens/hol/src/IndDef/examples/milScript.sml"
      "/home/sowens/hol/src/IndDef/examples/opsemScript.sml"
      "/home/sowens/hol/src/bag/bagLib.sml"
      "/home/sowens/hol/src/bag/bagSimps.sml"
      "/home/sowens/hol/src/bag/bagSyntax.sml"
      "/home/sowens/hol/src/bag/containerScript.sml"
      "/home/sowens/hol/src/bag/mnUtils.sml"
      "/home/sowens/hol/src/bag/bagScript.sml"
      "/home/sowens/hol/src/bag/bagTheory.sml"
      "/home/sowens/hol/src/bag/containerTheory.sml"
      "/home/sowens/hol/src/basicProof/BasicProvers.sml"
      "/home/sowens/hol/src/bool/Abbrev.sml"
      "/home/sowens/hol/src/bool/Conv.sml"
      "/home/sowens/hol/src/bool/DB.sml"
      "/home/sowens/hol/src/bool/DefnBase.sml"
      "/home/sowens/hol/src/bool/Drule.sml"
      "/home/sowens/hol/src/bool/Ho_Net.sml"
      "/home/sowens/hol/src/bool/Ho_Rewrite.sml"
      "/home/sowens/hol/src/bool/Pmatch.sml"
      "/home/sowens/hol/src/bool/Psyntax.sml"
      "/home/sowens/hol/src/bool/QConv.sml"
      "/home/sowens/hol/src/bool/Rewrite.sml"
      "/home/sowens/hol/src/bool/Rsyntax.sml"
      "/home/sowens/hol/src/bool/Tactic.sml"
      "/home/sowens/hol/src/bool/Tactical.sml"
      "/home/sowens/hol/src/bool/Thm_cont.sml"
      "/home/sowens/hol/src/bool/TypeBase.sml"
      "/home/sowens/hol/src/bool/TypeBasePure.sml"
      "/home/sowens/hol/src/bool/boolLib.sml"
      "/home/sowens/hol/src/bool/boolScript.sml"
      "/home/sowens/hol/src/bool/boolSyntax.sml"
      "/home/sowens/hol/src/bool/fastbuild.sml"
      "/home/sowens/hol/src/bool/holmakebuild.sml"
      "/home/sowens/hol/src/bool/boolTheory.sml"
      "/home/sowens/hol/src/bool/Prim_rec.sml"
      "/home/sowens/hol/src/boss/SingleStep.sml"
      "/home/sowens/hol/src/boss/bossLib.sml"
      "/home/sowens/hol/src/combin/combinScript.sml"
      "/home/sowens/hol/src/combin/combinSyntax.sml"
      "/home/sowens/hol/src/combin/combinTheory.sml"
      "/home/sowens/hol/src/compute/examples/Arith.sml"
      "/home/sowens/hol/src/compute/examples/MergeSort.sml"
      "/home/sowens/hol/src/compute/examples/Sort.sml"
      "/home/sowens/hol/src/compute/src/clauses.sml"
      "/home/sowens/hol/src/compute/src/computeLib.sml"
      "/home/sowens/hol/src/compute/src/compute_rules.sml"
      "/home/sowens/hol/src/compute/src/equations.sml"
      "/home/sowens/hol/src/datatype/basicrec/Define_type.sml"
      "/home/sowens/hol/src/datatype/basicrec/rec_typeScript.sml"
      "/home/sowens/hol/src/datatype/Datatype.sml"
      "/home/sowens/hol/src/datatype/EnumType.sml"
      "/home/sowens/hol/src/datatype/Mutual.sml"
      "/home/sowens/hol/src/datatype/ind_types.sml"
      "/home/sowens/hol/src/datatype/equiv/EquivType.sml"
      "/home/sowens/hol/src/datatype/mutrec/examples/example.sml"
      "/home/sowens/hol/src/datatype/mutrec/examples/test.sml"
      "/home/sowens/hol/src/datatype/mutrec/examples/var_example.sml"
      "/home/sowens/hol/src/datatype/mutrec/examples/var_example2.sml"
      "/home/sowens/hol/src/datatype/mutrec/ConsThms.sml"
      "/home/sowens/hol/src/datatype/mutrec/MutRecDef.sml"
      "/home/sowens/hol/src/datatype/mutrec/MutRecMask.sml"
      "/home/sowens/hol/src/datatype/mutrec/Recftn.sml"
      "/home/sowens/hol/src/datatype/mutrec/TypeInfo.sml"
      "/home/sowens/hol/src/datatype/mutrec/mutrecLib.sml"
      "/home/sowens/hol/src/datatype/mutrec/utils/elsaUtils.sml"
      "/home/sowens/hol/src/datatype/mutual/examples/assertion.sml"
      "/home/sowens/hol/src/datatype/mutual/examples/pat.sml"
      "/home/sowens/hol/src/datatype/mutual/examples/rectypes.sml"
      "/home/sowens/hol/src/datatype/mutual/examples/smallML.sml"
      "/home/sowens/hol/src/datatype/mutual/examples/test.sml"
      "/home/sowens/hol/src/datatype/mutual/Def_MN_Func.sml"
      "/home/sowens/hol/src/datatype/mutual/Def_MN_Type.sml"
      "/home/sowens/hol/src/datatype/mutual/MutualIndThen.sml"
      "/home/sowens/hol/src/datatype/mutual/mutualLib.sml"
      "/home/sowens/hol/src/datatype/mutual/src/define_mutual_types.sml"
      "/home/sowens/hol/src/datatype/mutual/src/load_nested_rec_lib.sml"
      "/home/sowens/hol/src/datatype/mutual/src/mk_mutual_lib.sml"
      "/home/sowens/hol/src/datatype/mutual/src/mutualLib.sml"
      "/home/sowens/hol/src/datatype/mutual/src/mutual_induct_then.sml"
      "/home/sowens/hol/src/datatype/mutual/src/recftn.sml"
      "/home/sowens/hol/src/datatype/nestrec/examples/e1.sml"
      "/home/sowens/hol/src/datatype/nestrec/examples/tmp.sml"
      "/home/sowens/hol/src/datatype/nestrec/examples/tv.sml"
      "/home/sowens/hol/src/datatype/nestrec/DefType.sml"
      "/home/sowens/hol/src/datatype/nestrec/DefTypeInfo.sml"
      "/home/sowens/hol/src/datatype/nestrec/ExistsFuns.sml"
      "/home/sowens/hol/src/datatype/nestrec/GenFuns.sml"
      "/home/sowens/hol/src/datatype/nestrec/NestedRecMask.sml"
      "/home/sowens/hol/src/datatype/nestrec/StringTable.sml"
      "/home/sowens/hol/src/datatype/nestrec/TypeOpTable.sml"
      "/home/sowens/hol/src/datatype/nestrec/TypeTable.sml"
      "/home/sowens/hol/src/datatype/nestrec/nested_recLib.sml"
      "/home/sowens/hol/src/datatype/parse/ParseDatatype.sml"
      "/home/sowens/hol/src/datatype/record/RecordType.sml"
      "/home/sowens/hol/src/datatype/ind_typeScript.sml"
      "/home/sowens/hol/src/datatype/ind_typeTheory.sml"
      "/home/sowens/hol/src/finite_map/finite_mapScript.sml"
      "/home/sowens/hol/src/finite_map/finite_mapTheory.sml"
      "/home/sowens/hol/src/goalstack/Bwd.sml"
      "/home/sowens/hol/src/goalstack/GoalstackPure.sml"
      "/home/sowens/hol/src/goalstack/History.sml"
      "/home/sowens/hol/src/goalstack/goalstackLib.sml"
      "/home/sowens/hol/src/hol88/hol88Lib.sml"
      "/home/sowens/hol/src/integer/testing/gen_bc_problem.sml"
      "/home/sowens/hol/src/integer/testing/genproblem.sml"
      "/home/sowens/hol/src/integer/testing/readproblemfile.sml"
      "/home/sowens/hol/src/integer/testing/test_cases.sml"
      "/home/sowens/hol/src/integer/testing/test_coopers.sml"
      "/home/sowens/hol/src/integer/testing/test_omega.sml"
      "/home/sowens/hol/src/integer/testing/testdp.sml"
      "/home/sowens/hol/src/integer/CSimp.sml"
      "/home/sowens/hol/src/integer/Cooper.sml"
      "/home/sowens/hol/src/integer/CooperMath.sml"
      "/home/sowens/hol/src/integer/CooperShell.sml"
      "/home/sowens/hol/src/integer/CooperSyntax.sml"
      "/home/sowens/hol/src/integer/OmegaSimple.sml"
      "/home/sowens/hol/src/integer/IntDP_Munge.sml"
      "/home/sowens/hol/src/integer/Omega.sml"
      "/home/sowens/hol/src/integer/OmegaMLShadow.sml"
      "/home/sowens/hol/src/integer/OmegaShell.sml"
      "/home/sowens/hol/src/integer/OmegaTest.sml"
      "/home/sowens/hol/src/integer/integerRingTheory.sml"
      "/home/sowens/hol/src/integer/intSimps.sml"
      "/home/sowens/hol/src/integer/intSyntax.sml"
      "/home/sowens/hol/src/integer/integerRingLib.sml"
      "/home/sowens/hol/src/integer/integerRingScript.sml"
      "/home/sowens/hol/src/integer/jrhUtils.sml"
      "/home/sowens/hol/src/integer/primeScript.sml"
      "/home/sowens/hol/src/integer/dividesScript.sml"
      "/home/sowens/hol/src/integer/dividesTheory.sml"
      "/home/sowens/hol/src/integer/jrhCore.sml"
      "/home/sowens/hol/src/integer/integerScript.sml"
      "/home/sowens/hol/src/integer/integerTheory.sml"
      "/home/sowens/hol/src/integer/CooperCore.sml"
      "/home/sowens/hol/src/integer/primeTheory.sml"
      "/home/sowens/hol/src/integer/DeepSyntaxScript.sml"
      "/home/sowens/hol/src/integer/gcdScript.sml"
      "/home/sowens/hol/src/integer/gcdTheory.sml"
      "/home/sowens/hol/src/integer/CooperThms.sml"
      "/home/sowens/hol/src/integer/int_arithScript.sml"
      "/home/sowens/hol/src/integer/int_arithTheory.sml"
      "/home/sowens/hol/src/integer/DeepSyntaxTheory.sml"
      "/home/sowens/hol/src/integer/OmegaSymbolic.sml"
      "/home/sowens/hol/src/integer/intLib.sml"
      "/home/sowens/hol/src/integer/OmegaScript.sml"
      "/home/sowens/hol/src/integer/OmegaTheory.sml"
      "/home/sowens/hol/src/integer/OmegaMath.sml"
      "/home/sowens/hol/src/list/examples/test.sml"
      "/home/sowens/hol/src/list/src/ListConv1.sml"
      "/home/sowens/hol/src/list/src/listLib.sml"
      "/home/sowens/hol/src/list/src/listSimps.sml"
      "/home/sowens/hol/src/list/src/listSyntax.sml"
      "/home/sowens/hol/src/list/src/operatorScript.sml"
      "/home/sowens/hol/src/list/src/rich_listSimps.sml"
      "/home/sowens/hol/src/list/src/operatorTheory.sml"
      "/home/sowens/hol/src/list/src/listScript.sml"
      "/home/sowens/hol/src/list/src/listTheory.sml"
      "/home/sowens/hol/src/list/src/rich_listScript.sml"
      "/home/sowens/hol/src/list/src/rich_listTheory.sml"
      "/home/sowens/hol/src/lite/liteLib.sml"
      "/home/sowens/hol/src/llist/llistScript.sml"
      "/home/sowens/hol/src/llist/llistTheory.sml"
      "/home/sowens/hol/src/marker/markerLib.sml"
      "/home/sowens/hol/src/marker/markerScript.sml"
      "/home/sowens/hol/src/marker/markerTheory.sml"
      "/home/sowens/hol/src/meson/test.sml"
      "/home/sowens/hol/src/meson/src/Canon_Port.sml"
      "/home/sowens/hol/src/meson/src/jrhTactics.sml"
      "/home/sowens/hol/src/meson/src/mesonLib.sml"
      "/home/sowens/hol/src/muddy/MuddyCore.sml"
      "/home/sowens/hol/src/muddy/bdd.sml"
      "/home/sowens/hol/src/muddy/bvec.sml"
      "/home/sowens/hol/src/muddy/fdd.sml"
      "/home/sowens/hol/src/num/arith/src/Arith.sml"
      "/home/sowens/hol/src/num/arith/src/Arith_cons.sml"
      "/home/sowens/hol/src/num/arith/src/Exists_arith.sml"
      "/home/sowens/hol/src/num/arith/src/Gen_arith.sml"
      "/home/sowens/hol/src/num/arith/src/Instance.sml"
      "/home/sowens/hol/src/num/arith/src/Int_extra.sml"
      "/home/sowens/hol/src/num/arith/src/Norm_arith.sml"
      "/home/sowens/hol/src/num/arith/src/Norm_bool.sml"
      "/home/sowens/hol/src/num/arith/src/Norm_ineqs.sml"
      "/home/sowens/hol/src/num/arith/src/Prenex.sml"
      "/home/sowens/hol/src/num/arith/src/RJBConv.sml"
      "/home/sowens/hol/src/num/arith/src/Rationals.sml"
      "/home/sowens/hol/src/num/arith/src/Sol_ranges.sml"
      "/home/sowens/hol/src/num/arith/src/Solve.sml"
      "/home/sowens/hol/src/num/arith/src/Solve_ineqs.sml"
      "/home/sowens/hol/src/num/arith/src/Streams.sml"
      "/home/sowens/hol/src/num/arith/src/Sub_and_cond.sml"
      "/home/sowens/hol/src/num/arith/src/Sup_Inf.sml"
      "/home/sowens/hol/src/num/arith/src/Term_coeffs.sml"
      "/home/sowens/hol/src/num/arith/src/Theorems.sml"
      "/home/sowens/hol/src/num/arith/src/Thm_convs.sml"
      "/home/sowens/hol/src/num/arith/src/numSimps.sml"
      "/home/sowens/hol/src/num/NonRecSize.sml"
      "/home/sowens/hol/src/num/reduce/src/Arithconv.sml"
      "/home/sowens/hol/src/num/reduce/src/Boolconv.sml"
      "/home/sowens/hol/src/num/reduce/src/reduceLib.sml"
      "/home/sowens/hol/src/num/theories/Num_conv.sml"
      "/home/sowens/hol/src/num/theories/numScript.sml"
      "/home/sowens/hol/src/num/theories/numSyntax.sml"
      "/home/sowens/hol/src/num/theories/numeralScript.sml"
      "/home/sowens/hol/src/num/theories/prim_recScript.sml"
      "/home/sowens/hol/src/num/theories/numTheory.sml"
      "/home/sowens/hol/src/num/theories/prim_recTheory.sml"
      "/home/sowens/hol/src/num/theories/arithmeticScript.sml"
      "/home/sowens/hol/src/num/theories/arithmeticTheory.sml"
      "/home/sowens/hol/src/num/theories/numeralTheory.sml"
      "/home/sowens/hol/src/num/numLib.sml"
      "/home/sowens/hol/src/one/oneScript.sml"
      "/home/sowens/hol/src/one/oneTheory.sml"
      "/home/sowens/hol/src/option/optionLib.sml"
      "/home/sowens/hol/src/option/optionSimps.sml"
      "/home/sowens/hol/src/option/optionSyntax.sml"
      "/home/sowens/hol/src/option/optionScript.sml"
      "/home/sowens/hol/src/option/optionTheory.sml"
      "/home/sowens/hol/src/pair/src/PairRules.sml"
      "/home/sowens/hol/src/pair/src/PairedLambda.sml"
      "/home/sowens/hol/src/pair/src/pairLib.sml"
      "/home/sowens/hol/src/pair/src/pairScript.sml"
      "/home/sowens/hol/src/pair/src/pairSimps.sml"
      "/home/sowens/hol/src/pair/src/pairSyntax.sml"
      "/home/sowens/hol/src/pair/src/pairTools.sml"
      "/home/sowens/hol/src/pair/src/pairTheory.sml"
      "/home/sowens/hol/src/parse/Absyn.sml"
      "/home/sowens/hol/src/parse/GrammarSpecials.sml"
      "/home/sowens/hol/src/parse/HOLgrammars.sml"
      "/home/sowens/hol/src/parse/HOLtokens.sml"
      "/home/sowens/hol/src/parse/Hol_pp.sml"
      "/home/sowens/hol/src/parse/Literal.sml"
      "/home/sowens/hol/src/parse/Overload.sml"
      "/home/sowens/hol/src/parse/Parse.sml"
      "/home/sowens/hol/src/parse/Parse_support.sml"
      "/home/sowens/hol/src/parse/Preterm.sml"
      "/home/sowens/hol/src/parse/Pretype.sml"
      "/home/sowens/hol/src/parse/fragstr.sml"
      "/home/sowens/hol/src/parse/monadic_parse.sml"
      "/home/sowens/hol/src/parse/optmonad.sml"
      "/home/sowens/hol/src/parse/parse_term.sml"
      "/home/sowens/hol/src/parse/parse_type.sml"
      "/home/sowens/hol/src/parse/seq.sml"
      "/home/sowens/hol/src/parse/seqmonad.sml"
      "/home/sowens/hol/src/parse/stmonad.sml"
      "/home/sowens/hol/src/parse/term_grammar.sml"
      "/home/sowens/hol/src/parse/term_pp.sml"
      "/home/sowens/hol/src/parse/term_pp_types.sml"
      "/home/sowens/hol/src/parse/term_tokens.sml"
      "/home/sowens/hol/src/parse/type_grammar.sml"
      "/home/sowens/hol/src/parse/type_pp.sml"
      "/home/sowens/hol/src/parse/type_tokens.sml"
      "/home/sowens/hol/src/portableML/Arbint.sml"
      "/home/sowens/hol/src/portableML/Arbnum.sml"
      "/home/sowens/hol/src/portableML/PIntMap.sml"
      "/home/sowens/hol/src/portableML/Portable.sml"
      "/home/sowens/hol/src/portableML/Redblackset.sml"
      "/home/sowens/hol/src/pred_set/src/PFset_conv.sml"
      "/home/sowens/hol/src/pred_set/src/PGspec.sml"
      "/home/sowens/hol/src/pred_set/src/PSet_ind.sml"
      "/home/sowens/hol/src/pred_set/src/pred_setLib.sml"
      "/home/sowens/hol/src/pred_set/src/pred_setSyntax.sml"
      "/home/sowens/hol/src/pred_set/src/pred_setScript.sml"
      "/home/sowens/hol/src/pred_set/src/pred_setTheory.sml"
      "/home/sowens/hol/src/pred_set/src/pred_setSimps.sml"
      "/home/sowens/hol/src/pred_set/src/fixedPointScript.sml"
      "/home/sowens/hol/src/pred_set/src/fixedPointTheory.sml"
      "/home/sowens/hol/src/prob/boolean_sequenceTools.sml"
      "/home/sowens/hol/src/prob/probLib.sml"
      "/home/sowens/hol/src/prob/prob_uniformScript.sml"
      "/home/sowens/hol/src/prob/prob_canonTools.sml"
      "/home/sowens/hol/src/prob/prob_pseudoScript.sml"
      "/home/sowens/hol/src/prob/prob_pseudoTools.sml"
      "/home/sowens/hol/src/prob/probTools.sml"
      "/home/sowens/hol/src/prob/prob_pseudoTheory.sml"
      "/home/sowens/hol/src/prob/prob_extraScript.sml"
      "/home/sowens/hol/src/prob/prob_extraTheory.sml"
      "/home/sowens/hol/src/prob/probScript.sml"
      "/home/sowens/hol/src/prob/prob_indepScript.sml"
      "/home/sowens/hol/src/prob/boolean_sequenceScript.sml"
      "/home/sowens/hol/src/prob/boolean_sequenceTheory.sml"
      "/home/sowens/hol/src/prob/prob_extraTools.sml"
      "/home/sowens/hol/src/prob/prob_uniformTheory.sml"
      "/home/sowens/hol/src/prob/state_transformerScript.sml"
      "/home/sowens/hol/src/prob/state_transformerTheory.sml"
      "/home/sowens/hol/src/prob/prob_canonScript.sml"
      "/home/sowens/hol/src/prob/prob_canonTheory.sml"
      "/home/sowens/hol/src/prob/probUtil.sml"
      "/home/sowens/hol/src/prob/probTheory.sml"
      "/home/sowens/hol/src/prob/prob_algebraScript.sml"
      "/home/sowens/hol/src/prob/prob_algebraTheory.sml"
      "/home/sowens/hol/src/prob/prob_indepTheory.sml"
      "/home/sowens/hol/src/prob/prob_uniformTools.sml"
      "/home/sowens/hol/src/q/QLib.sml"
      "/home/sowens/hol/src/q/Q.sml"
      "/home/sowens/hol/src/real/Diff.sml"
      "/home/sowens/hol/src/real/hratScript.sml"
      "/home/sowens/hol/src/real/hrealScript.sml"
      "/home/sowens/hol/src/real/netsScript.sml"
      "/home/sowens/hol/src/real/polyScript.sml"
      "/home/sowens/hol/src/real/powserScript.sml"
      "/home/sowens/hol/src/real/realLib.sml"
      "/home/sowens/hol/src/real/realSimps.sml"
      "/home/sowens/hol/src/real/realaxScript.sml"
      "/home/sowens/hol/src/real/seqScript.sml"
      "/home/sowens/hol/src/real/topologyScript.sml"
      "/home/sowens/hol/src/real/transcScript.sml"
      "/home/sowens/hol/src/real/hratTheory.sml"
      "/home/sowens/hol/src/real/hrealTheory.sml"
      "/home/sowens/hol/src/real/realaxTheory.sml"
      "/home/sowens/hol/src/real/realScript.sml"
      "/home/sowens/hol/src/real/realTheory.sml"
      "/home/sowens/hol/src/real/topologyTheory.sml"
      "/home/sowens/hol/src/real/netsTheory.sml"
      "/home/sowens/hol/src/real/seqTheory.sml"
      "/home/sowens/hol/src/real/limScript.sml"
      "/home/sowens/hol/src/real/limTheory.sml"
      "/home/sowens/hol/src/real/RealArith.sml"
      "/home/sowens/hol/src/real/polyTheory.sml"
      "/home/sowens/hol/src/real/powserTheory.sml"
      "/home/sowens/hol/src/real/transcTheory.sml"
      "/home/sowens/hol/src/refute/AC.sml"
      "/home/sowens/hol/src/refute/Canon.sml"
      "/home/sowens/hol/src/refute/refuteLib.sml"
      "/home/sowens/hol/src/relation/relationScript.sml"
      "/home/sowens/hol/src/relation/relationTheory.sml"
      "/home/sowens/hol/src/res_quan/src/Cond_rewrite.sml"
      "/home/sowens/hol/src/res_quan/src/res_quanLib.sml"
      "/home/sowens/hol/src/res_quan/src/res_quanScript.sml"
      "/home/sowens/hol/src/res_quan/src/res_quanTheory.sml"
      "/home/sowens/hol/src/res_quan/src/res_quanTools.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreClausalForm.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreDefinitions.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreEnvironment.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreEqualities.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreGeneralize.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreInduction.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreIrrelevance.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreLib.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreRewriteRules.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreShells.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreStructEqual.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreSupport.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreTermsAndClauses.sml"
      "/home/sowens/hol/src/retired/BoyerMoore/BoyerMooreWaterfall.sml"
      "/home/sowens/hol/src/retired/decision/examples.sml"
      "/home/sowens/hol/src/retired/decision/src/CongruenceClosure.sml"
      "/home/sowens/hol/src/retired/decision/src/CongruenceClosurePairs.sml"
      "/home/sowens/hol/src/retired/decision/src/CongruenceClosureTypes.sml"
      "/home/sowens/hol/src/retired/decision/src/Decide.sml"
      "/home/sowens/hol/src/retired/decision/src/DecideNum.sml"
      "/home/sowens/hol/src/retired/decision/src/DecidePair.sml"
      "/home/sowens/hol/src/retired/decision/src/DecideProp.sml"
      "/home/sowens/hol/src/retired/decision/src/DecideTypes.sml"
      "/home/sowens/hol/src/retired/decision/src/DecideUninterp.sml"
      "/home/sowens/hol/src/retired/decision/src/DecisionArithConvs.sml"
      "/home/sowens/hol/src/retired/decision/src/DecisionConv.sml"
      "/home/sowens/hol/src/retired/decision/src/DecisionNormConvs.sml"
      "/home/sowens/hol/src/retired/decision/src/DecisionSupport.sml"
      "/home/sowens/hol/src/retired/decision/src/DecisionTheorems.sml"
      "/home/sowens/hol/src/retired/decision/src/LazyRules.sml"
      "/home/sowens/hol/src/retired/decision/src/LazyThm.sml"
      "/home/sowens/hol/src/retired/decision/src/NormalizeBool.sml"
      "/home/sowens/hol/src/retired/decision/src/NumArith.sml"
      "/home/sowens/hol/src/retired/decision/src/NumArithCons.sml"
      "/home/sowens/hol/src/retired/decision/src/NumHOLType.sml"
      "/home/sowens/hol/src/retired/decision/src/NumInequalityCoeffs.sml"
      "/home/sowens/hol/src/retired/decision/src/NumType.sml"
      "/home/sowens/hol/src/retired/decision/src/Taut.sml"
      "/home/sowens/hol/src/retired/decision/src/decisionLib.sml"
      "/home/sowens/hol/src/retired/hol90/HOLScript.sml"
      "/home/sowens/hol/src/retired/hol90/HOLSimps.sml"
      "/home/sowens/hol/src/retired/hol90/hol90Lib.sml"
      "/home/sowens/hol/src/retired/ind_def/examples/algebraScript.sml"
      "/home/sowens/hol/src/retired/ind_def/examples/clScript.sml"
      "/home/sowens/hol/src/retired/ind_def/examples/milScript.sml"
      "/home/sowens/hol/src/retired/ind_def/examples/opsemScript.sml"
      "/home/sowens/hol/src/retired/ind_def/src/ind_defLib.sml"
      "/home/sowens/hol/src/retired/set/src/Fset_conv.sml"
      "/home/sowens/hol/src/retired/set/src/Gspec.sml"
      "/home/sowens/hol/src/retired/set/src/Set_ind.sml"
      "/home/sowens/hol/src/retired/set/src/finsetScript.sml"
      "/home/sowens/hol/src/retired/set/src/setLib.sml"
      "/home/sowens/hol/src/retired/set/src/setSimps.sml"
      "/home/sowens/hol/src/retired/tree/ltreeScript.sml"
      "/home/sowens/hol/src/retired/tree/treeScript.sml"
      "/home/sowens/hol/src/ring/examples/tests.sml"
      "/home/sowens/hol/src/ring/src/abs_tools.sml"
      "/home/sowens/hol/src/ring/src/abstraction.sml"
      "/home/sowens/hol/src/ring/src/canonicalScript.sml"
      "/home/sowens/hol/src/ring/src/numRingLib.sml"
      "/home/sowens/hol/src/ring/src/numRingScript.sml"
      "/home/sowens/hol/src/ring/src/prelimScript.sml"
      "/home/sowens/hol/src/ring/src/quote.sml"
      "/home/sowens/hol/src/ring/src/quoteScript.sml"
      "/home/sowens/hol/src/ring/src/ringLib.sml"
      "/home/sowens/hol/src/ring/src/ringNormScript.sml"
      "/home/sowens/hol/src/ring/src/ringScript.sml"
      "/home/sowens/hol/src/ring/src/semi_ringScript.sml"
      "/home/sowens/hol/src/ring/src/prelimTheory.sml"
      "/home/sowens/hol/src/ring/src/quoteTheory.sml"
      "/home/sowens/hol/src/ring/src/semi_ringTheory.sml"
      "/home/sowens/hol/src/ring/src/canonicalTheory.sml"
      "/home/sowens/hol/src/ring/src/ringTheory.sml"
      "/home/sowens/hol/src/ring/src/ringNormTheory.sml"
      "/home/sowens/hol/src/ring/src/numRingTheory.sml"
      "/home/sowens/hol/src/simp/test.sml"
      "/home/sowens/hol/src/simp/src/Cache.sml"
      "/home/sowens/hol/src/simp/src/Cond_rewr.sml"
      "/home/sowens/hol/src/simp/src/Opening.sml"
      "/home/sowens/hol/src/simp/src/Satisfy.sml"
      "/home/sowens/hol/src/simp/src/SatisfySimps.sml"
      "/home/sowens/hol/src/simp/src/Sequence.sml"
      "/home/sowens/hol/src/simp/src/Trace.sml"
      "/home/sowens/hol/src/simp/src/Traverse.sml"
      "/home/sowens/hol/src/simp/src/Travrules.sml"
      "/home/sowens/hol/src/simp/src/Unify.sml"
      "/home/sowens/hol/src/simp/src/Unwind.sml"
      "/home/sowens/hol/src/simp/src/boolSimps.sml"
      "/home/sowens/hol/src/simp/src/combinSimps.sml"
      "/home/sowens/hol/src/simp/src/pureSimps.sml"
      "/home/sowens/hol/src/simp/src/simpLib.sml"
      "/home/sowens/hol/src/string/stringLib.sml"
      "/home/sowens/hol/src/string/stringScript.sml"
      "/home/sowens/hol/src/string/stringSimps.sml"
      "/home/sowens/hol/src/string/stringSyntax.sml"
      "/home/sowens/hol/src/string/stringTheory.sml"
      "/home/sowens/hol/src/sum/sumSimps.sml"
      "/home/sowens/hol/src/sum/sumSyntax.sml"
      "/home/sowens/hol/src/sum/sumScript.sml"
      "/home/sowens/hol/src/sum/sumTheory.sml"
      "/home/sowens/hol/src/taut/tautLib.sml"
      "/home/sowens/hol/src/temporal/examples.sml"
      "/home/sowens/hol/src/temporal/src/Omega_AutomataScript.sml"
      "/home/sowens/hol/src/temporal/src/Past_Temporal_LogicScript.sml"
      "/home/sowens/hol/src/temporal/src/Temporal_LogicScript.sml"
      "/home/sowens/hol/src/temporal/src/schneiderUtils.sml"
      "/home/sowens/hol/src/temporal/src/temporalLib.sml"
      "/home/sowens/hol/src/temporal/src/Temporal_LogicTheory.sml"
      "/home/sowens/hol/src/temporal/src/Past_Temporal_LogicTheory.sml"
      "/home/sowens/hol/src/temporal/src/Omega_AutomataTheory.sml"
      "/home/sowens/hol/src/tfl/examples/sorting/partitionScript.sml"
      "/home/sowens/hol/src/tfl/examples/sorting/permScript.sml"
      "/home/sowens/hol/src/tfl/examples/sorting/sortingScript.sml"
      "/home/sowens/hol/src/tfl/src/test/test.97.sml"
      "/home/sowens/hol/src/tfl/src/test/test.98.sml"
      "/home/sowens/hol/src/tfl/src/test/test.sml"
      "/home/sowens/hol/src/tfl/src/test/test1.sml"
      "/home/sowens/hol/src/tfl/src/test/test2.sml"
      "/home/sowens/hol/src/tfl/src/Defn.sml"
      "/home/sowens/hol/src/tfl/src/Functional.sml"
      "/home/sowens/hol/src/tfl/src/Induction.sml"
      "/home/sowens/hol/src/tfl/src/RW.sml"
      "/home/sowens/hol/src/tfl/src/Rules.sml"
      "/home/sowens/hol/src/tfl/src/TotalDefn.sml"
      "/home/sowens/hol/src/tfl/src/wfrecUtils.sml"
      "/home/sowens/hol/src/unwind/unwindLib.sml"
      "/home/sowens/hol/src/word/src/wordLib.sml"
      "/home/sowens/hol/src/word/theories/Base.sml"
      "/home/sowens/hol/src/word/theories/bword_bitopScript.sml"
      "/home/sowens/hol/src/word/theories/wordScript.sml"
      "/home/sowens/hol/src/word/theories/word_numScript.sml"
      "/home/sowens/hol/src/word/theories/word_baseScript.sml"
      "/home/sowens/hol/src/word/theories/word_baseTheory.sml"
      "/home/sowens/hol/src/word/theories/word_bitopScript.sml"
      "/home/sowens/hol/src/word/theories/word_bitopTheory.sml"
      "/home/sowens/hol/src/word/theories/word_numTheory.sml"
      "/home/sowens/hol/src/word/theories/bword_numScript.sml"
      "/home/sowens/hol/src/word/theories/bword_numTheory.sml"
      "/home/sowens/hol/src/word/theories/bword_arithScript.sml"
      "/home/sowens/hol/src/word/theories/bword_arithTheory.sml"
      "/home/sowens/hol/src/word/theories/bword_bitopTheory.sml"
      "/home/sowens/hol/src/word/theories/wordTheory.sml"
      "/home/sowens/hol/src/word32/abbrevUtil.sml"
      "/home/sowens/hol/src/word32/selectUtil.sml"
      "/home/sowens/hol/src/word32/word32Lib.sml"
      "/home/sowens/hol/src/word32/word32Script.sml"
      "/home/sowens/hol/src/word32/wordFunctor.sml"
      "/home/sowens/hol/src/word32/bitsScript.sml"
      "/home/sowens/hol/src/word32/bitsTheory.sml"
      "/home/sowens/hol/src/word32/word32Theory.sml"
      "/home/sowens/hol/tools/Holmake/Systeml.sml"
      "/home/sowens/hol/tools/Holmake/Holdep.sml"
      "/home/sowens/hol/tools/Holmake/Holmake.sml"
      "/home/sowens/hol/tools/Holmake/Holmake_rules.sml"
      "/home/sowens/hol/tools/Holmake/Holmake_types.sml"
      "/home/sowens/hol/tools/Holmake/unix-systeml.sml"
      "/home/sowens/hol/tools/Holmake/winNT-systeml.sml"
      "/home/sowens/hol/tools/Holmake/Parser.sml"
      "/home/sowens/hol/tools/Holmake/Lexer.sml"
      "/home/sowens/hol/tools/Holmake/Holmake_parse.sml"
      "/home/sowens/hol/tools/Holmake/Holmake_tokens.sml"
      "/home/sowens/hol/tools/build.sml"
      "/home/sowens/hol/tools/end-init-boss.sml"
      "/home/sowens/hol/tools/end-init.sml"
      "/home/sowens/hol/tools/make_iss.sml"
      "/home/sowens/hol/tools/unquote-init.sml"
      "/home/sowens/hol/tools/win-config.sml"
      "/home/sowens/hol/tools/quote-filter/quote-filter.sml"
      "/home/sowens/hol/tools/quote-filter/filter.sml"
      "/home/sowens/hol/tools/configure.sml"))
  
  
  )