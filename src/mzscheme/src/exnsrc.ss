
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
	     "all syntax errors except \\scmfirst{read} errors")
     
     (variable [id "identifier" "the unbound variable's global identifier"]
	       "unbound global variable at run-time"
	       (*keyword [] "attempt to change the binding of a global keyword"))

     (application [value "error-specific" "the error-specific inappropriate value"] -
		  (arity [expected "arity" "the correct procedure arity as returned by \\scmfirst{arity}"]
			 "application with the wrong number of arguments")
		  (type [expected "symbol" "name of the expected type"]
			"wrong argument type to a procedure, not including divide-by-zero")
		  (mismatch [] "bad argument combination (e.g., out-of-range index for a vector) or platform-specific integer range error")
		  (divide-by-zero [] "divide by zero; \\scm{application-value} is always zero")
		  (*continuation [] "attempt to cross a continuation boundary or apply another thread's continuation"))
     
     (*else [] "fall-through in \\scmfirst{cond} or \\scmfirst{case}")

     (*struct [] "the supertype expression in a \\scmfirst{struct} form returned a value that was not a structure type value")

     (*object [] "all object-, class-, and interface-specific errors")
     
     (*unit [] "all unit- and unit/sig-specific errors")

     (read [port "port" "port being read"] "all reader errors"
	   (eof [] "unexpected end-of-file"))
     
     (i/o [] -
	  (port [port "port" "port for attempted operation"] -
		(read [] "error reading from a port")
		(write [] "error writing to a port")
		(closed [] "attempt to operate on a closed port")
		(*user [] "user-defined input port returned a non-character from the character-getting procedure"))
	  (filesystem [pathname "string" "pathname"] "illegal pathname or error manipulating a filesystem object")
	  (*tcp [] "TCP errors"))

     (misc [] "low-level and MzScheme-specific errors"
	   (unsupported [] "unsupported feature")
	   (user-break [] "asynchronous thread break")
	   (out-of-memory [] "out of memory")))
