(compound-unit/sig 
  (import
    (INTERFACE : zodiac:interface^)
    (PARAMETERS : plt:parameters^)
    (PRETTY : mzlib:pretty-print^)
    (MZLIB-FILE : mzlib:file^))
  (link
    [MISC : zodiac:misc^
      ((reference-unit/sig "misc.ss") PRETTY)]
    [TOP-STRUCTS : zodiac:structures^ 
      ((reference-unit/sig "basestr.ss"))]
    [SCAN-STRUCTS : zodiac:scanner-structs^
      ((reference-unit/sig "scanstr.ss")
	TOP-STRUCTS)]
    [READ-STRUCTS : zodiac:reader-structs^
      ((reference-unit/sig "readstr.ss")
	TOP-STRUCTS)]
    [SCAN-PARMS : zodiac:scanner-parameters^
      ((reference-unit/sig "scanparm.ss")
	TOP-STRUCTS)]
    [SCAN-CODE : zodiac:scanner-code^
      ((reference-unit/sig "scanner.ss")
	TOP-STRUCTS SCAN-STRUCTS READ-STRUCTS 
	PARAMETERS SCAN-PARMS INTERFACE)]
    [READ-CODE : zodiac:reader-code^
      ((reference-unit/sig "reader.ss")
	TOP-STRUCTS SCAN-STRUCTS READ-STRUCTS
	PARAMETERS SCAN-PARMS INTERFACE SCAN-CODE)]
    [SEXP : zodiac:sexp^
      ((reference-unit/sig "sexp.ss")
	MISC TOP-STRUCTS READ-STRUCTS INTERFACE)]
    [PATTERN : zodiac:pattern^
      ((reference-unit/sig "pattern.ss")
	MISC SEXP READ-STRUCTS SCHEME-CORE)]
    [EXPANDER : zodiac:expander^
      ((reference-unit/sig "x.ss")
	MISC SEXP TOP-STRUCTS READ-STRUCTS
	SCHEME-CORE INTERFACE)]
    [CORRELATE : zodiac:correlate^
      ((reference-unit/sig "corelate.ss")
	TOP-STRUCTS)]
    [BACK-PROTOCOL : zodiac:back-protocol^
      ((reference-unit/sig "back.ss")
	MISC INTERFACE)]
    [SCHEME-CORE : zodiac:scheme-core^
      ((reference-unit/sig "scm-core.ss")
	TOP-STRUCTS MISC SEXP READ-STRUCTS
	BACK-PROTOCOL EXPANDER INTERFACE PATTERN PARAMETERS)]
    [SCHEME-MAIN : zodiac:scheme-main^
      ((reference-unit/sig "scm-main.ss")
	MISC TOP-STRUCTS SCAN-PARMS
	READ-STRUCTS READ-CODE SEXP
	PATTERN SCHEME-CORE BACK-PROTOCOL EXPANDER INTERFACE PARAMETERS)]
    [SCHEME-SPIDEY : zodiac:scheme-mrspidey^
      ((reference-unit/sig "scm-spdy.ss")
	MISC TOP-STRUCTS SCAN-PARMS READ-STRUCTS READ-CODE SEXP PATTERN
	SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE
	MZLIB-FILE)]
    [SCHEME-OBJ : zodiac:scheme-objects^
      ((reference-unit/sig "scm-obj.ss")
	MISC TOP-STRUCTS READ-STRUCTS SEXP
	PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
    [SCHEME-UNIT : zodiac:scheme-units^
      ((reference-unit/sig "scm-unit.ss")
	MISC TOP-STRUCTS SCAN-PARMS READ-STRUCTS READ-CODE SEXP
	PATTERN SCHEME-CORE SCHEME-MAIN BACK-PROTOCOL EXPANDER INTERFACE)]
    [SCHEME-OBJ+UNIT : zodiac:scheme-objects+units^
      ((reference-unit/sig "scm-ou.ss")
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
