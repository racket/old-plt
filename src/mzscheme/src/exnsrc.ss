
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
     
     (variable [id "symbol" "the unbound variable's global identifier"]
	       "unbound global or module variable at run time")

     (application [value "value" "the error-specific inappropriate value"] -
		  (arity [expected "arity" "the correct procedure arity as returned by \\scmfirst{arity}"]
			 "application with the wrong number of arguments")
		  (type [expected "symbol" "name of the expected type"]
			"wrong argument type to a procedure, not including divide-by-zero")
		  (mismatch [] "bad argument combination (e.g., out-of-range index for a vector) or platform-specific integer range error")
		  (divide-by-zero [] "divide by zero; \\scm{application-value} is always zero")
		  (*continuation [] "attempt to cross a continuation boundary or apply another thread's continuation"))
     
     (syntax [expr "syntax object or {\\scmfalse}" "illegal expression (or {\\scmfalse} if unknown)"
		   form "symbol" "the syntactic form name that detected the error"
		   module "symbol or {\\scmfalse}" "the form-defining module (or {\\scmfalse} if unknown)"]
	     "syntax error, but not a \\scmfirst{read} error")
     
     (read [port "input-port" "port being read"
		 line "non-negative exact integer or {\\scmfalse}" "source line"
		 column "non-negative exact integer or {\\scmfalse}" "source column, or position if line is {\\scmfalse}"]
	   "\\scm{read} parsing error"
	   (eof [] "unexpected end-of-file"))
     
     (i/o [] -
	  (port [port "port" "port for attempted operation"] -
		(read [] "error reading from a port")
		(write [] "error writing to a port")
		(closed [] "attempt to operate on a closed port"))
	  (filesystem [pathname "path" "file or directory pathname"
				detail "symbol or {\\scmfalse}" "\\SymbolFirst{ill-formed-path}, \\SymbolFirst{already-exists}, or \\SymbolFirst{wrong-version}, indicating the reason for the exception (if available), or \\scm{\\#f}"] 
		      "illegal pathname or error manipulating a filesystem object")
	  (*tcp [] "TCP errors"))
     
     (thread [] "raised by \\scmfirst{call-with-custodian}")

     (break [continuation "continuation" "a continuation that resumes from the break"] "asynchronous thread break")
     
     (misc [] "low-level or MzScheme-specific error"
	   (unsupported [] "unsupported feature")
	   (out-of-memory [] "out of memory")))
