
(lambda (request failure)
  (case request
    [(name) "zodiac"]
    [(blurb)
      (list
	"Zodiac is the source-correlating, hygienic, macro-system-and-beyond "
	"that is used by the PLT Scheme suite to parse Scheme programs.")]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "zsigs.ss" "zodiac")
			 (require-library "sigs.ss" "zodiac"))]
    [(compile-omit-files)
     (list "sigs.ss" "zsigs.ss" "scm-hanc.ss" "quasi.ss")]
    [(compile-elaboration-zos)
     (list "zsigs.ss" "sigs.ss")]
    [else (failure)]))
