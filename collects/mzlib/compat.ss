
(require-library "compatu.ss")
(require-library "functiou.ss")

(invoke-open-unit/sig
 (compound-unit/sig
  (import)
  (link [compat@ : mzlib:compat^ (mzlib:compat@ function@)]
	[function@ : mzlib:function^ (mzlib:function@)])
  (export (open compat@)))
 #f)
