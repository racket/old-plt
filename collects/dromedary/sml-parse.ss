#cs
(module sml-parse mzscheme
  (require (lib "contracts.ss")
           (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           (lib "string.ss"))

  (define-empty-tokens sml-etoks
    (ABSTYPE AND ANDALSO ARROW AS BAR CASE COLON
     COLONGT COMMA DARROW DATATYPE DO DOTDOTDOT ELSE END
     EOF EQTYPE EQUALS EXCEPTION FN FUN FUNCTOR HANDLE
     HASH HASHLBRACKET IF IN INCLUDE INFIX INFIXR LBRACE
     LBRACKET LET LOCAL LPAREN NONFIX OF OP OPEN ORELSE
     PRIM_EQTYPE PRIM_REFTYPE PRIM_TYPE PRIM_VAL QUOTEL RAISE
     RBRACE RBRACKET REC RPAREN SEMICOLON SHARING SIG
     SIGNATURE STAR STRING STRUCT STRUCTURE THEN TYPE
     UNDERBAR VAL WHERE WHILE WITH WITHTYPE EOF2))
  
  (define-tokens sml-toks 
    (CHAR ID NEGINT NZDIGIT NZPOSINT2 QUAL_ID QUAL_STAR
     QUOTEM QUOTER REAL TYVAR WORD ZDIGIT ZPOSINT2))

  (define lex-comment
    (lexer
     ("(*" (lex-comment input-port))
     ("*)" (void))
     ((+ 
  
  (define get-token
    (lexer-src-pos
     ("(*" (begin
             (lex-comment input-port)
             (return-without-pos (get-token input-port))))
     ((: "abstype" "and" "andalso" "as" "case" "datatype" "do" "else" "eqtype"
         "end" "exception" "fn" "fun" "functor" "handle" "if" "in" "include"
         "infix" "infixr" "let" "local" "nonfix" "of" "op" "open" "orelse"
         "prim_eqtype" "prim_type" "prim_val" "raise" "rec" "sharing" "sig"
         "signature" "struct" "then" "type" "val" "where" "while" "with" "withtype")
      (string->symbol (string-uppercase! lexeme)))
     ("prim_EQtype" 'PRIM_REFTYPE)
     ("#" 'HASH)
     ("->" 'ARROW)
     ("|" 'BAR)
     (":>" 'COLONGT)
     (":" 'COLON)
     ("=>" 'DARROW)
     ("=" 'EQUALS)
     ("*" 'STAR)
     ))
         
         
  
  
  (define parse-sml
    (parser
     (start ToplevelPhrase)
     (end EOF2)
     (error (lambda (a b stx spos epos)
              (raise-read-error (format "parse error near ~a" (syntax-e stx))
                                (syntax-source stx)
                                (syntax-line stx)
                                (syntax-column stx)
                                (syntax-position stx)
                                (syntax-span stx))))
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
       ((SEMICOLON) #f)
       ((EOF) #f))
      (SemiEof
       ((SEMICOLON SemiEof) #f)
       ((EOF) #f))
      (Dec
       ((KWDec Dec) #f)
       ((SEMICOLON Dec) #f)
       (() #f))
      (KWDec_seq1 
       ((KWDec KWDec_seq1) #f)
       ((KWDec) #f))
      (TopDecFile
       ((KWDec_seq EOF) #f))
      (StructFile
       ((STRUCTURE ModId EQUALS ModExp SemiEof) #f)
       ((STRUCTURE ModId COLONGT SigId EQUALS ModExp SemiEof) #f)
       ((KWCoreDec_seq EOF) #f))
      (KWDec_seq
       ((KWDec KWDec_seq) #f)
       ((SEMICOLON KWDec_seq) #f)
       (() #f))
      (KWCoreDec_seq
       ((KWCoreDec KWCoreDec_seq) #f)
       ((SEMICOLON KWCoreDec_seq) #f)
       (() #f))
      (KWDec ((KWCoreDec) #f) ((KWModuleDec) #f))
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
       ((Spec_seq EOF) #f))
      (SigFile
       ((SIGNATURE SigId EQUALS SigExp SemiEof) #f)
       ((CoreSpec_seq EOF) #f))
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
       (() #f))))))
  