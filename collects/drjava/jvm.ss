(define-signature jvm^
  (jfind-class
   jfind-method
   jfind-field
   jfind-static-method
   jfind-static-field
   jget-field
   jset-field!
   jcall
   jnew
   jstring->string
   string->jbytes))

(define jvm@
  (unit->unit/sig (load-relative-extension (build-path "compiled" "native" (system-library-subpath) "libmzjvm.so"))
		  ()
		  jvm^))
