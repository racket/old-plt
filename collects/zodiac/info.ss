
(lambda (request)
  (case request
    [(name) "zodiac"]
    [(compile-prefix) '(begin
			 (require-library "match.ss")
			 (require-library "zsigs.ss" "zodiac")
			 (require-library "sigs.ss" "zodiac")
			 (require-library "namedarg.ss" "zodiac")
			 (require-library "mzlibs.ss")
			 (require-library "sparams.ss" "backward"))]
    [(compile-omit-files)
     (list "namedarg.ss" "sigs.ss" "zsigs.ss" "scm-hanc.ss")]
    [else (error 'zodiac-info "Unknown request: ~s" request)]))
