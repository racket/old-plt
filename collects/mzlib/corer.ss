
(compound-unit/sig
   (import)
   (link [pretty-print@ : mzlib:pretty-print^ ((reference-library-unit/sig "prettyr.ss"))]
	 [file@ : mzlib:file^ ((reference-library-unit/sig "filer.ss") string@ function@)]
	 [function@ : mzlib:function^ ((reference-library-unit/sig "functior.ss"))]
	 [compat@ : mzlib:compat^ ((reference-library-unit/sig "compatr.ss") function@)]
	 [string@ : mzlib:string^ ((reference-library-unit/sig "stringr.ss"))]
	 [compile@ : mzlib:compile^ ((reference-library-unit/sig "compiler.ss"))])
   (export (unit pretty-print@)
	   (unit file@)
	   (unit function@)
	   (unit compat@)
	   (unit string@)
	   (unit compile@)))
