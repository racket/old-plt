(lambda (sym fail)
  (let ([elab (list "cgis.ss" "mails.ss" "nntps.ss" "pop3s.ss" "urls.ss"
		    "smtps.ss" "heads.ss" "imaps.ss" "dnss.ss" "base64s.ss")])
    (case sym
      [(help-desk-message) (format "Mz/Mr: See the \"To Load\" section of each collection for the command to load it.")]
      [(name) "Net"]
      [(blurb)
       (list
	"The net collection provides a suite of libraries to handle standard "
	"internet-based protocols.")]
      [(compile-prefix) `(begin ,@(map (lambda (x) `(require-library ,x "net")) elab))]
      [(compile-omit-files) elab]
      [(compile-elaboration-zos) elab]
      [else (fail)])))
