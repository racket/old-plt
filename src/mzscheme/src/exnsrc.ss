
#|

Initial symbols are struct types. A non-initial symbol is a struct
type without fields or subtypes. Square brackets are struct fields, *
indicates especially MzScheme-specific (the star is not in the name),
strings are types/comments.

|#

(exn [message "immutable-string" "error message" 
	      continuation-marks "mark-set" "value returned by \\scmfirst{current-continuation-marks} immediately after the error is detected"] 
     -
     (user [] "raised by calling \\scmfirst{error}")
     
     (variable [id "symbol" "the variable's identifier"]
	       "unbound or not-yet-defined global or module variable at run time")

     (application [value "value" "the error-specific inappropriate value"] -
		  (arity [expected "arity" "the correct procedure arity as returned by \\scmfirst{arity}"]
			 "application with the wrong number of arguments")
		  (type [expected "symbol" "name of the expected type"]
			"wrong argument type to a procedure, not including divide-by-zero")
		  (mismatch [] "bad argument combination (e.g., out-of-range index for a vector) or platform-specific integer range error")
		  (divide-by-zero [] "divide by zero; \\rawscm{application-value} is always zero")
		  (*continuation [] "attempt to cross a continuation boundary or apply another thread's continuation"))
     
     (syntax [expr "syntax object or {\\scmfalse}" "illegal expression (or {\\scmfalse} if unknown)"
		   form "symbol or {\\scmfalse}" "the syntactic form name that detected the error (or {\\scmfalse} if unknown)"
		   module "symbol, module path index, or {\\scmfalse}" "the form-defining module (or {\\scmfalse} if unknown)"]
	     "syntax error, but not a \\scmfirst{read} error")
     
     (read [source "value" "source name"
		 line "positive exact integer or {\\scmfalse}" "source line"
		 column "non-negative exact integer or {\\scmfalse}" "source column"
		 position "positive exact integer or {\\scmfalse}" "source position"
		 span "non-negative exact integer or {\\scmfalse}" "source span"]
	   "\\rawscm{read} parsing error"
	   (eof [] "unexpected end-of-file")
	   (non-char [] "unexpected non-character"))
     
     (i/o [] -
	  (port [port "port" "port for attempted operation"] -
		(read [] "error reading from a port")
		(write [] "error writing to a port")
		(closed [] "attempt to operate on a closed port"))
	  (filesystem [pathname "path" "file or directory pathname"
				detail "symbol or {\\scmfalse}" "\\SymbolFirst{ill-formed-path}, \\SymbolFirst{already-exists}, or \\SymbolFirst{wrong-version}, indicating the reason for the exception (if available), or \\rawscm{\\#f}"] 
		      "illegal pathname or error manipulating a filesystem object")
	  (tcp [] "TCP errors")
	  (udp [] "UDP errors"))
     
     (thread [] "raised by \\scmfirst{call-with-custodian}")

     (module [] "raised by \\scmkfirst{module}, \\scmkfirst{require}, etc.")

     (break [continuation "continuation" "a continuation that resumes from the break"] "asynchronous thread break")

     (special-comment [width "non-negative exact integer" "width of the special comment in port positions"]
	"raised by a custom input port's special-reading procedure")
     
     (misc [] "low-level or MzScheme-specific error"
	   (unsupported [] "unsupported feature")
	   (out-of-memory [] "out of memory")))
