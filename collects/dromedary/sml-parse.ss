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
    (let ((get-token (make-get-token #t)))
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
  
  (define (process-sml-string s)
    ())
  
  (define (make-get-token quotation)
    (let ((lexing-mode 'normal)
          (comment-depth 0)
          (par-count null))
      (letrec ((token (lambda (ip)
                        (case lexing-mode
                          ((normal) (if quotation (token-nq ip) (token-n ip)))
                          ((quote) (void))
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
                 ("0" (token-ZDIGIT "0"))
                 ((- "1" "9") (token-NZDIGIT lexeme))
                 ((@ "0" (+ (digit))) (token-ZPOSINT2 lexeme))
                 ((@ (- "1" "9") (+ (digit))) (token-NZPOSINT2 lexeme))
                 ((@ "~" (+ (digit))) (token-NEGINT lexeme))
                 ((@ "0w" (+ (digit))) (token-WORD lexeme))
                 ((@ "0wx" (+ (: (digit) (- "a" "f") (- "A" "F")))) (token-WORD lexeme))
                 ((@ (? "~")
                     (+ (digit)) 
                     (? (@ "." (+ (digit))))
                     (? (@  (: "e" "E") (? "~") (+ (digit)))))
                  (token-REAL lexeme))
                 ((@ (? "#") "\"" (* (string-contents)) "\"")
                  (if (char=? #\# (string-ref lexeme 0))
                      (token-CHAR lexeme)
                      (token-STRING lexeme)))
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
                  (return-without-pos (token-nq input-port)))
                 ("(*"
                  (begin
                    (set! comment-depth (add1 comment-depth))
                    (comment input-port)
                    (return-without-pos (token-nq input-port))))
                 ("*)" (raise-error "*) not in comment" start-pos end-pos))
                 ((@ "'" (+ (: (letter-or-digit) "_" "'"))) (token-TYVAR lexeme))
                 ("0" (token-ZDIGIT "0"))
                 ((- "1" "9") (token-NZDIGIT lexeme))
                 ((@ "0" (+ (digit))) (token-ZPOSINT2 lexeme))
                 ((@ (- "1" "9") (+ (digit))) (token-NZPOSINT2 lexeme))
                 ((@ "~" (+ (digit))) (token-NEGINT lexeme))
                 ((@ "0w" (+ (digit))) (token-WORD lexeme))
                 ((@ "0wx" (+ (: (digit) (- "a" "f") (- "A" "F")))) (token-WORD lexeme))
                 ((@ (? "~")
                     (+ (digit)) 
                     (? (@ "." (+ (digit))))
                     (? (@  (: "e" "E") (? "~") (+ (digit)))))
                  (token-REAL lexeme))
                 ((@ (? "#") "\"" (* (string-contents)) "\"")
                  (if (char=? #\# (string-ref lexeme 0))
                      (token-CHAR lexeme)
                      (token-STRING lexeme)))
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
     (start TopDecFile)
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
  
  (define files
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
    
    
    )