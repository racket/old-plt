(reference-library "macro.ss")
(reference-library "match.ss")
(reference-library "file.ss")

(reference-library "urlu.ss" "net")

(invoke-open-unit/sig
  (compound-unit/sig
    (import
      (FILE : mzlib:file^))
    (link
      (URL : mzlib:url^
	(mzlib:url@ FILE)))
    (export
      (open URL)))
  #f
  mzlib:file^)
