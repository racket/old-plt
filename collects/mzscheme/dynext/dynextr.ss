
(compound-unit/sig
 (import)
 (link [compile@ : dynext:compile^ ((reference-relative-library-unit/sig "compiler.ss"))]
       [link@ : dynext:link^ ((reference-relative-library-unit/sig "linkr.ss"))]
       [file@ : dynext:file^ ((reference-relative-library-unit/sig "filer.ss"))])
 (export (unit compile@)
	 (unit link@)
	 (unit file@)))
