
(require-library "fileu.ss")
(require-library "functio.ss")
(require-library "string.ss")

(invoke-open-unit/sig
 (compound-unit/sig
  (import)
  (link [file@ : mzlib:file^ (mzlib:file@ string@ function@)]
	[function@ : mzlib:function^ (mzlib:function@)]
	[string@ : mzlib:string^ (mzlib:string@)])
  (export (open file@)))
 #f)
