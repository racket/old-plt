(reference-library "urls.ss" "net")
(reference-library "refer.ss")
(reference-library "coreu.ss")
(reference-library "qqu.ss" "quasiquote")

(define quasiquote:program@
  (compound-unit/sig
    (import)
    (link
      (MZLIB-CORE : mzlib:core^
	(mzlib:core@))
      (URL : mzlib:url^
	((reference-library-unit/sig "urlr.ss" "net")
	  (MZLIB-CORE file@)))
      (INTERFACE : quasiquote:graphical-interface^
	(quasiquote:graphical-interface@))
      (QUOTESTER : quasiquote:quotester^
	(quasiquote:quotester@ INTERFACE URL)))
    (export
      (open QUOTESTER))))

(invoke-open-unit/sig quasiquote:program@)
