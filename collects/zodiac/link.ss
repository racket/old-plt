(compound-unit/sig 
  (import
    (INTERFACE : zodiac:interface^)
    (PARAMETERS : plt:parameters^))
  (link
    [MISC : zodiac:misc^
      ((include-unit "misc.ss"))]
    [TOP-STRUCTS : zodiac:structures^ 
      ((include-unit "basestr.ss"))]
    [SCAN-STRUCTS : zodiac:scanner-structs^
      ((include-unit "scanstr.ss")
	TOP-STRUCTS)]
    [READ-STRUCTS : zodiac:reader-structs^
      ((include-unit "readstr.ss")
	TOP-STRUCTS)]
    [SCAN-PARMS : zodiac:scanner-parameters^
      ((include-unit "scanparm.ss")
	TOP-STRUCTS)]
    [SCAN-CODE : zodiac:scanner-code^
      ((include-unit "scanner.ss")
	TOP-STRUCTS SCAN-STRUCTS READ-STRUCTS 
	PARAMETERS SCAN-PARMS INTERFACE)]
    [READ-CODE : zodiac:reader-code^
      ((include-unit "reader.ss")
	TOP-STRUCTS SCAN-STRUCTS READ-STRUCTS
	PARAMETERS SCAN-PARMS INTERFACE SCAN-CODE)]
    [SEXP : zodiac:sexp^
      ((include-unit "sexp.ss")
	MISC TOP-STRUCTS READ-STRUCTS INTERFACE)]
    [PATTERN : zodiac:pattern^
      ((include-unit "pattern.ss")
	MISC SEXP READ-STRUCTS SCHEME-CORE)]
    [EXPANDER : zodiac:expander^
      ((include-unit "x.ss")
	MISC SEXP TOP-STRUCTS READ-STRUCTS
	SCHEME-CORE INTERFACE)]
    [CORRELATE : zodiac:correlate^
      ((include-unit "corelate.ss")
	TOP-STRUCTS)]
    [BACK-PROTOCOL : zodiac:back-protocol^
      ((include-unit "back.ss")
	MISC INTERFACE)]
    [SCHEME-CORE : zodiac:scheme-core^
      ((include-unit "scm-core.ss")
	TOP-STRUCTS MISC SEXP READ-STRUCTS
	BACK-PROTOCOL EXPANDER INTERFACE PATTERN PARAMETERS)]
    [SCHEME-MAIN : zodiac:scheme-main^
      ((include-unit "scm-main.ss")
	MISC TOP-STRUCTS SCAN-PARMS
	READ-STRUCTS READ-CODE SEXP
	PATTERN SCHEME-CORE BACK-PROTOCOL EXPANDER INTERFACE PARAMETERS)]
    [SCHEME-SPIDEY : zodiac:scheme-mrspidey^
      ((include-unit "scm-spdy.ss")
	MISC TOP-STRUCTS READ-STRUCTS SEXP
	PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
    [SCHEME-OBJ : zodiac:scheme-objects^
      ((include-unit "scm-obj.ss")
	MISC TOP-STRUCTS READ-STRUCTS SEXP
	PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
    [SCHEME-UNIT : zodiac:scheme-units^
      ((include-unit "scm-unit.ss")
	MISC TOP-STRUCTS SCAN-PARMS READ-STRUCTS READ-CODE SEXP
	PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
    [SCHEME-OBJ+UNIT : zodiac:scheme-objects+units^
      ((include-unit "scm-ou.ss")
	MISC TOP-STRUCTS READ-STRUCTS SEXP PATTERN EXPANDER INTERFACE
	SCHEME-CORE SCHEME-MAIN SCHEME-OBJ SCHEME-UNIT)])
  (export (open TOP-STRUCTS) (open SCAN-PARMS)
    (open READ-STRUCTS) (open READ-CODE)
    (open SEXP) (open PATTERN) (open CORRELATE) (open BACK-PROTOCOL)
    (open EXPANDER)
    (open SCHEME-CORE) (open SCHEME-MAIN)
    (open SCHEME-OBJ) (open SCHEME-UNIT)
    (open SCHEME-OBJ+UNIT)
    (open SCHEME-SPIDEY)))
