
(require-library "pconveru.ss")
(require-library "string.ss")
(require-library "function.ss")

(invoke-open-unit/sig
  (compound-unit/sig
   (import)
   (link [convert@ : mzlib:print-convert^
		   (mzlib:print-convert@ string@ function@ hooks@)]
	 [string@ : mzlib:string^ (mzlib:string@)]
	 [function@ : mzlib:function^ (mzlib:function@)]
	 [hooks@ : mzlib:print-convert-hooks^ (mzlib:print-convert-hooks@)])
   (export (open convert@) (open hooks@)))
  #f)


