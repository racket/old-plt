(require-library "unitsig.ss")

(define zodiac:system@			; : zodiac:system^
  (compound-unit/sig 
    (import
      (INTERFACE : zodiac:interface^)
      (PARAMETERS : plt:parameters^))
    (link
      [MISC : zodiac:misc^ (zodiac:misc@)]
      [TOP-STRUCTS : zodiac:structures^ 
	(zodiac:structures@)]
      [SCAN-STRUCTS : zodiac:scanner-structs^
	(zodiac:scanner-structs@ TOP-STRUCTS)]
      [READ-STRUCTS : zodiac:reader-structs^
	(zodiac:reader-structs@ TOP-STRUCTS)]
      [SCAN-PARMS : zodiac:scanner-parameters^
	(zodiac:scanner-parameters@ TOP-STRUCTS)]
      [SCAN-CODE : zodiac:scanner-code^
	(zodiac:scanner-code@ TOP-STRUCTS SCAN-STRUCTS READ-STRUCTS 
	  PARAMETERS SCAN-PARMS INTERFACE)]
      [READ-CODE : zodiac:reader-code^
	(zodiac:reader-code@ TOP-STRUCTS SCAN-STRUCTS READ-STRUCTS
	  PARAMETERS SCAN-PARMS INTERFACE SCAN-CODE)]
      [SEXP : zodiac:sexp^
	(zodiac:raw-sexp@ MISC TOP-STRUCTS READ-STRUCTS INTERFACE)]
      [PATTERN : zodiac:pattern^
	(zodiac:pattern@ MISC SEXP READ-STRUCTS SCHEME-CORE)]
      [EXPANDER : zodiac:expander^
	(zodiac:expander@ MISC SEXP TOP-STRUCTS READ-STRUCTS
	  SCHEME-CORE INTERFACE)]
      [CORRELATE : zodiac:correlate^
	(zodiac:correlate@ TOP-STRUCTS)]
      [BACK-PROTOCOL : zodiac:back-protocol^
	(zodiac:back-protocol@ MISC INTERFACE)]
      [SCHEME-CORE : zodiac:scheme-core^
	(zodiac:scheme-core@ TOP-STRUCTS MISC SEXP READ-STRUCTS
	  BACK-PROTOCOL EXPANDER INTERFACE PATTERN PARAMETERS)]
      [SCHEME-MAIN : zodiac:scheme-main^
	(zodiac:scheme-main@ MISC TOP-STRUCTS SCAN-PARMS
	  READ-STRUCTS READ-CODE SEXP
	  PATTERN SCHEME-CORE BACK-PROTOCOL EXPANDER INTERFACE PARAMETERS)]
      [SCHEME-SPIDEY : zodiac:scheme-mrspidey^
	(zodiac:scheme-mrspidey@ MISC TOP-STRUCTS READ-STRUCTS SEXP
	  PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
      [SCHEME-OBJ : zodiac:scheme-objects^
	(zodiac:scheme-objects@ MISC TOP-STRUCTS READ-STRUCTS SEXP
	  PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
      [SCHEME-UNIT : zodiac:scheme-units^
	(zodiac:scheme-units@ MISC TOP-STRUCTS SCAN-PARMS
	  READ-STRUCTS READ-CODE SEXP
	  PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
      [SCHEME-OBJ+UNIT : zodiac:scheme-objects+units^
	(zodiac:scheme-objects+units@ MISC TOP-STRUCTS READ-STRUCTS SEXP
	  PATTERN EXPANDER INTERFACE
	  SCHEME-CORE SCHEME-MAIN SCHEME-OBJ SCHEME-UNIT)])
    (export (open TOP-STRUCTS) (open SCAN-PARMS)
      (open READ-STRUCTS) (open READ-CODE)
      (open SEXP) (open PATTERN) (open CORRELATE) (open BACK-PROTOCOL)
      (open EXPANDER)
      (open SCHEME-CORE) (open SCHEME-MAIN)
      (open SCHEME-OBJ) (open SCHEME-UNIT)
      (open SCHEME-OBJ+UNIT)
      (open SCHEME-SPIDEY))))
