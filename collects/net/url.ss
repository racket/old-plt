(require-library "macro.ss")
(require-library "match.ss")
(require-library "file.ss")

(require-library "urlu.ss" "net")

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
