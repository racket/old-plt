
#|

Initial symbols are struct types. A non-initial symbol is a struct
type without fields or subtypes. Square brackets are struct fields, *
indicates especially MzScheme-specific (the star is not in the name),
strings are types/comments.

|#

(exn [message "string" "error message" 
	      debug-info "anything" "value returned by the current debug info handler (called immediately after the error is detected)"] 
     -
     (user [] "raised by calling \\scmfirst{error}")
     
     (syntax [expr "S-expression" "illegal expression (or \\scm{\\#f} if unknown)"]
	     "all syntax errors, but not \\scmfirst{read} errors")
     
     (variable [id "identifier" "the unbound variable's global identifier"]
	       "unbound global variable at run-time"
	       (*keyword [] "attempt to change the binding of a global keyword"))

     (application [value "error-specific" "the error-specific inappropriate value"] -
		  (arity [expected "arity" "the correct procedure arity as returned by \\scmfirst{arity}"]
			 "application with the wrong number of arguments")
		  (type [expected "symbol" "name of the expected type"]
			"wrong argument type to a procedure, not including divide-by-zero or platform-specific integer range errors")
		  (mismatch [] "illegal index, bad list sizes, inconsistent arguments, etc.")
		  (divide-by-zero [] "divide by zero; \\scm{application-value} is always zero")
		  (*continuation [] "attempt to cross a continuation boundary or apply another thread's continuation"))
     
     (*else [] "fall-through in \\scmfirst{cond} or \\scmfirst{case}")

     (*struct [] "the supertype expression in a \\scmfirst{struct} form returned a value that was not a structure type value")

     (*object [] "non-class for superclass, ivar not found, etc.")
     
     (*unit [] "non-unit for link or invoke, exported variable not found, signature mismatch, etc.")

     (read [port "port" "port being read"] "unexpected close paren, bad character constant, misuse of \\scm{\\#}, etc."
	   (eof [] "unexpected end-of-file"))
     
     (i/o [] -
	  (port [port "port" "port for attempted operation"] -
		(read [] "error reading from a port")
		(write [] "error writing to a port")
		(closed [] "attempt to operate on a closed port")
		(*user [] "user-defined input port returned a non-character from the character-getting procedure"))
	  (filesystem [pathname "string" "pathname"] "bad pathname, file not found, directory not found, collection not found, etc.")
	  (*tcp [] "TCP errors"))

     (misc [] "low-level and MzScheme-specific errors"
	   (unsupported [] "unsupported feature")
	   (user-break [] "asynchronous thread break")
	   (out-of-memory [] "out of memory")))
