;; VM Scheme -> C translation module
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

(unit/sig
 compiler:vm2c^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 compiler:analyze^
	 compiler:const^
	 compiler:rep^
	 compiler:vehicle^
	 compiler:vmstructs^
	 compiler:driver^)

(define local-vars-at-top? #f)

(define vm->c:indent-by 4)
(define vm->c:indent-spaces
  (make-string vm->c:indent-by #\space))

(define vm->c:bucket-names null)
(define vm->c:make-bucket-names!
  (lambda (symbols)
    (set! vm->c:bucket-names
	  (map (lambda (s)
		 (compiler:get-symbol-const! #f s)
		 (list s (symbol-append 'GL (compiler:gensym) s)))
	       symbols))))
(define vm->c:bucket-name
  (lambda (symbol)
    (let ([b (assq symbol vm->c:bucket-names)])
      (if b
	  (cadr b)
	  (compiler:internal-error #f (format "vm->c:bucket-name: no bucket for ~a" symbol))))))

(define (vm->c:SYMBOLS-name)
  (if compiler:multi-o-constant-pool?
      (format "SYMBOLS~a" compiler:setup-suffix)
      "SYMBOLS"))

(define (vm->c:INEXACTS-name)
  "INEXACTS")

(define (vm->c:make-symbol-const-string sc)
  (format "~a[~a]" (vm->c:SYMBOLS-name) (zodiac:varref-var sc)))

(define (vm->c:emit-list! port comma table counter -symbol->string)
  (let ([v (make-vector counter)])
    (hash-table-for-each
     table
     (lambda (sym b)
       (vector-set! v (string->number (symbol->string (zodiac:varref-var b))) sym)))
    (for-each
     (lambda (s)
       (fprintf port "  ~s~a~n" (-symbol->string s) comma))
     (vector->list v))))

(define (vm->c:emit-symbol-list! port comma)
  (vm->c:emit-list! port comma const:symbol-table const:symbol-counter symbol->string))

(define (vm->c:emit-symbol-declarations! port)
  (unless (zero? const:symbol-counter)
    (unless compiler:multi-o-constant-pool?
       (fprintf port "static const char *SYMBOL_STRS[~a] = {~n" const:symbol-counter)
       (vm->c:emit-symbol-list! port ",")
       (fprintf port "}; /* end of SYMBOL_STRS */~n~n"))

    (fprintf port "~aScheme_Object * ~a[~a];~n~n" 
	     (if compiler:multi-o-constant-pool? "" "static ")
	     (vm->c:SYMBOLS-name)
	     const:symbol-counter)))

(define (vm->c:emit-inexact-list! port comma)
  (vm->c:emit-list! port comma const:inexact-table const:inexact-counter 
		    (lambda (x) (string->number (symbol->string x)))))

(define (vm->c:emit-inexact-declarations! port)
  (unless (zero? const:inexact-counter)
    (fprintf port "static const double INEXACT_NUMBERS[~a] = {~n" const:inexact-counter)
    (vm->c:emit-inexact-list! port ",")
    (fprintf port "}; /* end of INEXACT_NUMBERS */~n~n")
    (fprintf port "static Scheme_Object * ~a[~a];~n~n" 
	     (vm->c:INEXACTS-name)
	     const:inexact-counter)))

(define (vm->c:emit-symbol-definitions! port)
  (unless (zero? const:symbol-counter)
    (fprintf port "  int i;~n")
    (fprintf port "  for (i = ~a; i--; )~n    SYMBOLS[i] = scheme_intern_exact_symbol(SYMBOL_STRS[i], mzc_strlen(SYMBOL_STRS[i]));~n"
	     const:symbol-counter)))

(define (vm->c:emit-inexact-definitions! port)
  (unless (zero? const:inexact-counter)
    (fprintf port "  int i;~n")
    (fprintf port "  for (i = ~a; i--; )~n    INEXACTS[i] = scheme_make_double(INEXACT_NUMBERS[i]);~n"
	     const:inexact-counter)))

(define vm->c:emit-export-symbol-definitions!
  (lambda (port)
    (let loop ([l (reverse compiler:total-unit-exports)][pos 0])
      (unless (null? l)
	  (fprintf port
		   "~aS.unit_exports[~a] = ~a;~n"
		   vm->c:indent-spaces
		   pos
		   (vm->c:make-symbol-const-string (car l)))
	  (loop (cdr l) (add1 pos))))))

(define vm->c:emit-prim-ref-declarations!
  (lambda (port)
    (unless (set-empty? compiler:primitive-refs)
       (fprintf port "/* primitives referenced by the code */~n")
       (fprintf port "static struct {~n")
       (for-each (lambda (a)
		   (fprintf port "  Scheme_Object * ~a;~n"
			    (vm->c:convert-symbol 
			     (vm->c:bucket-name
			      a))))
		 (set->list compiler:primitive-refs))
       (fprintf port "} P;~n")
       (newline port))))

(define vm->c:emit-prim-ref-definitions!
  (lambda (port)
    (unless (set-empty? compiler:primitive-refs)
      (fprintf port "   /* primitives referenced by the code */~n")
      (for-each (lambda (a)
		  (fprintf port "~aP.~a = scheme_global_bucket(~a, env)->val;~n"
			   vm->c:indent-spaces
			   (vm->c:convert-symbol (vm->c:bucket-name a))
			   (vm->c:make-symbol-const-string (compiler:get-symbol-const! #f a))))
		(set->list compiler:primitive-refs)))))

(define vm->c:emit-struct-definitions!
  (lambda (structs port)
    (fprintf port "/* compiler written structures */~n")
    (for-each (lambda (struct)
		(fprintf port "struct ~a~n{~n"
			 (vm->c:convert-symbol
			  (rep:struct-name struct)))
		(for-each
		 (lambda (field)
		   (fprintf port "~a~a ~a;~n"
			    vm->c:indent-spaces
			    (vm->c:convert-type-definition
			     (rep:struct-field-rep field))
			    (vm->c:convert-symbol
			     (rep:struct-field-name field)))) 
		 (rep:struct-fields struct))
		(fprintf port "};~n"))
	      (reverse structs))
    (newline port)))

(define (compiler:any-statics?)
  (not (and (null? compiler:static-list)
	    (null? compiler:case-lambdas)
	    (null? compiler:total-unit-exports)
	    (null? compiler:compounds)
	    (null? compiler:classes)
	    (null? compiler:interfaces))))

(define (emit-static-variable-fields! port l)
  (fprintf port "  /* Write fields as an array to help C comiplers */~n")
  (fprintf port "  /* that don't like really big records. */~n")
  (fprintf port "  Scheme_Object * _consts_[~a];~n" (length l))
  (let loop ([l l][n 0])
    (unless (null? l)
      (fprintf port "# define ~a _consts_[~a]~n"
	       (vm->c:convert-symbol (car l)) n)
      (loop (cdr l) (add1 n)))))

; when statics have binding information, this will look more like 
; emit-local-variable-declarations!
(define vm->c:emit-static-declarations!
  (lambda (port)
    (unless (not (compiler:any-statics?))
       (fprintf port "/* compiler written static variables */~n")
       (fprintf port "static struct {~n")
       (emit-static-variable-fields! port compiler:static-list)
       (unless (null? compiler:total-unit-exports)
               (fprintf port "  Scheme_Object *unit_exports[~a];~n" (length compiler:total-unit-exports)))
       (unless (null? compiler:case-lambdas)
	       (fprintf port "  short *casesArities[~a];~n"
			(length compiler:case-lambdas)))
       (unless (null? compiler:compounds)
	       (fprintf port "  Scheme_Object *compoundAssemblies[~a];~n"
			(length compiler:compounds)))
       (unless (null? compiler:classes)
	       (fprintf port "  struct Scheme_Class_Assembly *classAssemblies[~a];~n"
			(length compiler:classes)))
       (unless (null? compiler:interfaces)
	       (fprintf port "  struct Scheme_Interface_Assembly *interfaceAssemblies[~a];~n"
			(length compiler:interfaces)))
       (fprintf port "} S;~n~n"))

    (fprintf port "/* compiler written per-load static variables */~n")
    (fprintf port "typedef struct Scheme_Per_Load_Statics {~n")
    (if (null? compiler:per-load-static-list)
	(fprintf port "  int dummy;~n")
	(emit-static-variable-fields! port compiler:per-load-static-list))
    (fprintf port "} Scheme_Per_Load_Statics;~n")
    (newline port)))

; when statics have binding information, this need only register
; pointer declarations
(define vm->c:emit-registration!
  (lambda (port)
    (fprintf port "~a/* register compiler written static variables with GC */~n"
	     vm->c:indent-spaces)
    (let ([register
	   (lambda (v)
	     (fprintf port "~ascheme_register_extension_global(&~a, sizeof(~a));~n"
		      vm->c:indent-spaces v v))])
      (unless (or (zero? const:symbol-counter) compiler:multi-o-constant-pool?)
	  (register "SYMBOLS"))
      (unless (set-empty? compiler:primitive-refs)
	  (register "P"))
      (unless (not (compiler:any-statics?))
	  (register "S")))
    (newline port)))

(define (vm->c:emit-case-arities-definitions! port)
  (fprintf port "   /* arity information for compiled case-lambdas */~n")
  (let loop ([l (reverse compiler:case-lambdas)][pos 0])
    (unless (null? l)
       (let* ([ast (car l)]
	      [args (zodiac:case-lambda-form-args ast)])
	 (if (null? args)
	     (fprintf port "~aS.casesArities[~a] = NULL;~n"
		      vm->c:indent-spaces pos)
	     (begin
	       (fprintf port "~a{~n~a  short * arities;~n" 
			vm->c:indent-spaces vm->c:indent-spaces)
	       (fprintf port "~a  arities = (short *)scheme_malloc_atomic(~a * sizeof(short));~n"
			vm->c:indent-spaces
			(* 2 (length args)))
	       (let loop ([l args][n 0])
		 (unless (null? l)
			 (let-values ([(min-arity max-arity) (compiler:formals->arity (car l))])
			   (fprintf port "~a  arities[~a] = ~a;~n~a  arities[~a] = ~a;~n"
				    vm->c:indent-spaces (* 2 n) min-arity
				    vm->c:indent-spaces (add1 (* 2 n)) max-arity))
			 (loop (cdr l) (add1 n))))
	       (fprintf port "~a  S.casesArities[~a] = arities;~n"
			vm->c:indent-spaces pos)
	       (fprintf port "~a}~n" vm->c:indent-spaces))))
       (loop (cdr l) (add1 pos)))))

(define (vm->c:emit-compound-definitions! port)
  (let loop ([l (reverse compiler:compounds)][pos 0])
    (unless (null? l)
       (let* ([ast (car l)]
	      [info (get-annotation ast)])
	 (fprintf port "~a{~n~a  Scheme_Object * imports, * exports, * links;~n" 
		  vm->c:indent-spaces vm->c:indent-spaces)

	 (fprintf port "~a  imports = " vm->c:indent-spaces)
	 (vm->c-expression (compound-info-imports info) #f port vm->c:indent-by #t)
	 (fprintf port ";~n")

	 (fprintf port "~a  exports = " vm->c:indent-spaces)
	 (vm->c-expression (compound-info-exports info) #f port vm->c:indent-by #t)
	 (fprintf port ";~n")

	 (fprintf port "~a  links = " vm->c:indent-spaces)
	 (vm->c-expression (compound-info-links info) #f port vm->c:indent-by #t)
	 (fprintf port ";~n"))

       (fprintf
	port 
	"~a  S.compoundAssemblies[~a] = scheme_assemble_compound_unit(imports, links, exports);~n"
	vm->c:indent-spaces pos)
       (fprintf port "~a}~n" vm->c:indent-spaces)
       (loop (cdr l) (add1 pos)))))

(define (vm->c:emit-class-definitions! port)
  (let loop ([l (reverse compiler:classes)][pos 0])
    (unless (null? l)
       (let ([ast (car l)])
	 (let* ([code (get-annotation ast)]
		[public-lookup-bindings (class-code-public-lookup-bindings code)]
		[override-lookup-bindings (class-code-override-lookup-bindings code)]
		[inherit-bindings (class-code-inherit-bindings code)]
		[rename-bindings (class-code-rename-bindings code)]
		[declare (lambda (name bindings)
			   (unless (null? bindings)
			     (fprintf port "~a  Scheme_Object * ~a[~a];~n"
				      vm->c:indent-spaces
				      name
				      (length bindings))))])
	   (fprintf port "~a{~n" vm->c:indent-spaces)
	   (declare "pubs" public-lookup-bindings)
	   (declare "overs" override-lookup-bindings)
	   (declare "inherits" inherit-bindings)
	   (declare "renames" rename-bindings)
	   (let ([get-symbols
		  (lambda (var list)
		    (let loop ([l list][n 0])
		      (unless (null? l)
			      (fprintf port "~a  ~a[~a] = ~a;~n"
				       vm->c:indent-spaces var n
				       (vm->c:make-symbol-const-string
					(compiler:get-symbol-const! #f (zodiac:binding-orig-name (car l)))))
			      (loop (cdr l) (add1 n)))))])
	     (get-symbols "pubs" public-lookup-bindings)
	     (get-symbols "overs" override-lookup-bindings)
	     (get-symbols "inherits" inherit-bindings)
	     (get-symbols "renames" rename-bindings)
	     (let*-values ([(mina maxa)
			    (compiler:paroptformals->arity (zodiac:class*/names-form-init-vars ast))])
	     (fprintf port "~a  S.classAssemblies[~a] = scheme_make_class_assembly(~s, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a, vehicle_~a);~n~a}~n"
		      vm->c:indent-spaces pos
		      (vm->c:extract-inferred-name (closure-code-name code))
		      (length (zodiac:class*/names-form-interfaces ast))
		      (length public-lookup-bindings)
		      (if (null? public-lookup-bindings) "NULL" "pubs")
		      (length override-lookup-bindings)
		      (if (null? override-lookup-bindings) "NULL" "overs")
		      (length inherit-bindings)
		      (if (null? inherit-bindings) "NULL" "inherits")
		      (length rename-bindings)
		      (if (null? rename-bindings) "NULL" "renames")
		      mina  maxa
		      (closure-code-vehicle code)
		      vm->c:indent-spaces)))))
       (loop (cdr l) (add1 pos)))))

(define (vm->c:emit-interface-definitions! port)
  (let loop ([l (reverse compiler:interfaces)][pos 0])
    (unless (null? l)
       (let* ([ast (car l)]
	      [names (zodiac:interface-form-variables ast)])
	 (fprintf port "~a{~n" vm->c:indent-spaces)
	 (unless (null? names)
		 (fprintf port "~a  Scheme_Object * names[~a];~n" 
			  vm->c:indent-spaces
			  (length names)))
	 (let loop ([l names][n 0])
	   (unless (null? l)
	     (let ([s (symbol->string (zodiac:read-object (car l)))])
	       (fprintf port "~a  names[~a] = scheme_intern_exact_symbol(~s, ~a);~n"
			vm->c:indent-spaces n
			s (string-length s)))
	     (loop (cdr l) (add1 n))))
	 (fprintf 
	  port 
	  "~a  S.interfaceAssemblies[~a] = scheme_make_interface_assembly(~s, ~a, ~a, ~a);~n"
	  vm->c:indent-spaces pos
	  (vm->c:extract-inferred-name (interface-info-name (get-annotation ast)))
	  (length (zodiac:interface-form-super-exprs ast))
	  (length names)
	  (if (null? names) "NULL" "names"))
	 (fprintf port "~a}~n" vm->c:indent-spaces))
       (loop (cdr l) (add1 pos)))))

(define (vm->c:emit-top-levels! kind return? per-load? count vm-list locals-list globals-list max-arity c-port)
  ;; count == -1 => go to the end of the list
  (let tls-loop ([i 0]
		 [n 0]
		 [vml vm-list]
		 [ll locals-list]
		 [bl globals-list])
    (fprintf c-port
	     "static ~a ~a_~a(Scheme_Env * env~a)~n{~n"
	     (if return? "Scheme_Object *" "void")
	     kind i
	     (if per-load? ", Scheme_Per_Load_Statics *PLS" ""))
    (when (> max-arity 0)
	  (fprintf c-port
		   "~aScheme_Object * arg[~a];~n"
		   vm->c:indent-spaces
		   max-arity)
	  (fprintf c-port "~aScheme_Process * pr = scheme_current_process;~n"     
		   vm->c:indent-spaces)
	  (fprintf c-port "~aScheme_Object ** tail_buf;~n"
		   vm->c:indent-spaces))
    (let loop ([c (compiler:option:max-exprs-per-top-level-set)][n n][vml vml][ll ll][bl bl])
      (if (or (zero? c) (null? vml) (= n count))
	  (begin
	    (unless (or (null? vml) (= n count) (not return?))
		    (fprintf c-port "~areturn NULL;~n" vm->c:indent-spaces))
	    (fprintf c-port 
		     "} /* end of ~a_~a */~n~n" kind i)
	    (if (or (null? vml) (= n count))
		i
		(tls-loop (add1 i) n vml ll bl)))
	  (begin
	    (let ([start (zodiac:zodiac-start (car vml))])
	      (fprintf c-port "~a{ /* [~a,~a] */~n" vm->c:indent-spaces
		       (zodiac:location-line start)
		       (zodiac:location-column start)))
	    (vm->c:emit-local-variable-declarations! 
	     (car ll)
	     (string-append vm->c:indent-spaces vm->c:indent-spaces)
	     c-port)
	    (vm->c:emit-local-bucket-declarations! 
	     (car bl)
	     (string-append vm->c:indent-spaces vm->c:indent-spaces)
	     #t
	     c-port)
	    (vm->c:emit-bucket-lookups! 
	     (car bl)
	     (string-append vm->c:indent-spaces vm->c:indent-spaces)
	     c-port)
	    
	    (vm->c-expression (car vml) #f c-port vm->c:indent-by #t)
	    
	    (fprintf c-port "~a}~n" vm->c:indent-spaces)
	    
	    (loop (sub1 c) (add1 n) (cdr vml) (cdr ll) (cdr bl)))))))

(define vm->c:emit-vehicle-prototype
  (lambda (port number)
    (let ([v (get-vehicle number)])
      (fprintf port 
	       "static ~a vehicle_~a(~a)"
	       (if (class-vehicle? v)
		   "void"
		   "Scheme_Object *")
	       number
	       (cond
		[(procedure-vehicle? v)
		 "void * void_param, int argc, Scheme_Object *argv[]"]
		[(unit-vehicle? v)
		 (string-append
		  "Scheme_Object **boxes, Scheme_Object **anchors, "
		  "struct Scheme_Unit *u, void *dreq")]
		[(class-vehicle? v)
		 (string-append "Scheme_Object **init_boxes, "
				"Scheme_Object **extract_boxes, "
				"Scheme_Object *super_init, "
				"int argc, "
				"Scheme_Object **argv, "
				"Scheme_Object *instance, "
				"void *data")]
		[else
		 (compiler:internal-error
		  #f
		  "vm->c:emit-vehicle-prototype: unknown closure kind ~a"
		  v)])))))

(define vm->c:emit-vehicle-declaration
  (lambda (port number)
    (vm->c:emit-vehicle-prototype port number)
    (fprintf port "; /* ~a */ ~n"
	     (vehicle-total-labels (get-vehicle number)))))

(define vm->c:emit-vehicle-header
  (lambda (port number)
    (vm->c:emit-vehicle-prototype port number)
    (fprintf port "~n{~n")))

(define vm->c:emit-vehicle-prologue
  (lambda (port vehicle)
    (let ([max-arity (vehicle-max-arity vehicle)]
	  [max-args (if (procedure-vehicle? vehicle)
			(procedure-vehicle-max-args vehicle)
			0)])
      (when (> max-arity 0)
	; emit declaration of argument stack 
	(fprintf port "~aScheme_Object * arg[~a];~n" 
		 vm->c:indent-spaces 
		 max-arity))
      (when (> max-args 0)
	; emit declaration of global variables for argument passing
	(let loop ([n 0])
	  (unless (= n max-args)
	    (fprintf port "~aregister long reg~a;~n" vm->c:indent-spaces n)
	    (loop (+ n 1)))))
      (when (> max-arity 0)
	; tail-buffer-setup
	(fprintf port "~aScheme_Process * pr = scheme_current_process;~n"
		 vm->c:indent-spaces)
	(fprintf port "~aScheme_Object ** tail_buf;~n"
		 vm->c:indent-spaces)))

    (when local-vars-at-top?
      (for-each
       (lambda (L)
	 (let ([locals (code-local-vars (get-annotation L))])
	   (vm->c:emit-local-variable-declarations! locals vm->c:indent-spaces port)))
       (vehicle-lambdas vehicle)))
    
    ; emit jump to function...
    (when (> (vehicle-total-labels vehicle) 1)
      ; emit switch dispatcher
      (fprintf port "~aswitch(*(unsigned int*)void_param)~n~a{ " 
	       vm->c:indent-spaces
	       vm->c:indent-spaces )
      (let loop ([n 0])
	(when (and (zero? (modulo n 3))
		   (not (= n compiler:label-number)))
	  (fprintf port "~n~a~a" vm->c:indent-spaces vm->c:indent-spaces))
	(if (= n (sub1 (vehicle-total-labels vehicle)))
	    (fprintf port "default: goto FGN~a;" n)
	    (begin
	      (fprintf port "case ~a: goto FGN~a;" n n)
	      (loop (add1 n)))))
      (fprintf port "~n~a}~n" vm->c:indent-spaces))))

(define vm->c:emit-vehicle-epilogue
  (lambda (port number)
    (fprintf port "} /* end of vehicle # ~a */~n" number)))

;; Will be expanded to hold environments, perhaps, etc.
(define vm->c:convert-type-definition
  (lambda (rep)
    (cond
     [(rep:atomic? rep) (case (rep:atomic-type rep)
			  [(scheme-object) "Scheme_Object *"]
			  [(scheme-bucket) "Scheme_Bucket *"]
			  [(scheme-per-load-static) "struct Scheme_Per_Load_Statics *"]
			  [(label) "int"]
			  [(prim) "Scheme_Closed_Primitive_Proc"]
			  [(prim-case) "Scheme_Closed_Case_Primitive_Proc"]
			  [(unit) "Scheme_Unit"]
			  [(begin0-saver) "_Scheme_Begin0_Rec"]
			  [else (compiler:internal-error 
				 #f
				 (format
				  "vm->c:convert-type-definition: ~a not valid atomic type"
				  (rep:atomic-type rep)))])]
     [(rep:pointer? rep)
      (string-append (vm->c:convert-type-definition (rep:pointer-to rep))
		     " *")]
     [(rep:struct? rep)
      (format "struct ~a" (vm->c:convert-symbol (rep:struct-name rep)))]
     [else (compiler:internal-error 
	    #f
	    (format "vm->c:convert-type-definition: ~a not a valid representation" rep))])))

; must handle structs as well as atomic types
(define vm->c:type-definition->malloc
  (lambda (rep)
    (format "scheme_malloc(sizeof(~a))"
	    (if (rep:struct? rep)
		(string-append "struct " (vm->c:convert-symbol (rep:struct-name rep)))
		(vm->c:convert-type-definition rep)))))

(define vm->c:emit-local-variable-declarations!
  (lambda (locals indent port)
    (let loop ([locals (set->list locals)])
      (if (null? locals)
	  (void)
	  (let* ([bound (car locals)]
		 [rep (binding-rep (get-annotation bound))])
	    (fprintf port "~a~a ~a;~n"
		     indent
		     (vm->c:convert-type-definition rep)
		     (vm->c:convert-symbol (zodiac:binding-var bound)))
	    (loop (cdr locals)))))))

(define vm->c:emit-local-bucket-declarations!
  (lambda (globals indent top-level? port)
    (for-each 
     (lambda (var)
       (if (const:per-load-statics-table? var)
	   (unless top-level?
	      (fprintf port "~aScheme_Per_Load_Statics * PLS;~n"
		       indent))
	   (fprintf port "~aScheme_Bucket * G~a;~n"
		    indent
		    (vm->c:convert-symbol (vm->c:bucket-name var)))))
     (set->list globals))))

(define vm->c:emit-bucket-lookups!
  (lambda (globals indent port)
    (for-each 
     (lambda (var)
       (unless (const:per-load-statics-table? var)
	 (let ([name (vm->c:convert-symbol (vm->c:bucket-name var))])
	   (fprintf port "~aG~a = scheme_global_bucket(~a, SCHEME_CURRENT_ENV(pr));~n"
		    indent 
		    name 
		    (vm->c:make-symbol-const-string (compiler:get-symbol-const! #f var))))))
     (set->list globals))))

(define binding-boxed? (one-of binding-mutable? binding-unit-i/e?))

(define vm->c:extract-arguments-into-variables!
  (lambda (args normal? get-rep get-dest dest-boxed? get-src get-cast has-default? indent port)
    ; Reverse order for the sake of noticing default arguments
    (let loop ([args (reverse args)] [n (sub1 (length args))] [last? #t])
      (unless (null? args)
        (let* ([has-default? (has-default? n)]
	       [argv-n
		(lambda ()
		  (if has-default?
		      (format "((argc > ~a) ? ~a : (arg_set_level = ~a, scheme_undefined))" n (get-src n) n)
		      (get-src n)))])
	 (cond
	  [(or normal? (not last?))
	   (fprintf port "~a~a = " indent (get-dest n))
	   (if (dest-boxed? n)
	       ; if the binding is mutable, we need to make a box and fill it with
	       ; the correct value
	       (let ([rep (get-rep n)])
		 (fprintf port "~ascheme_malloc(sizeof(~a));~n" 
			  (get-cast n #f)
			  (vm->c:convert-type-definition
			   (rep:pointer-to rep)))
		 (fprintf port "~a*(~a)~a = (~a)~a;~n"
			  indent
			  (vm->c:convert-type-definition rep)
			  (get-dest n) 
			  (vm->c:convert-type-definition (rep:pointer-to rep))
			  (argv-n)))
	       
	       (fprintf port "~a~a;~n" (get-cast n #t) (argv-n)))
	   (loop (cdr args) (sub1 n) #f)]
	  
	  [else ; the rest get pulled into a list
	   (when (dest-boxed? n)
		 (fprintf port
			  "~a~a = ~ascheme_malloc(sizeof(Scheme_Object *));~n"
			  indent
			  (get-dest n)
			  (get-cast n #f)))
	   (fprintf port 
		    "~a~a~a = ~ascheme_build_list(argc-~a, argv+~a);~n"
		    indent
		    (if (dest-boxed? n)
			"*(Scheme_Object * *)" 
			"")
		    (get-dest n)
		    (if (dest-boxed? n)
			""
			(get-cast n #t))
		    n
		    n)

	   (loop (cdr args) (sub1 n) #f)]))))))

(define vm->c:pack-global-registers!
  (lambda (L which indent port)
    (let* ([arglist (list-ref (zodiac:case-lambda-form-args L) which)]
	   [args (zodiac:arglist-vars arglist)])
      (vm->c:extract-arguments-into-variables!
       args
       (zodiac:list-arglist? arglist)
       (lambda (n) "") ; rep not used since never boxed
       (lambda (n) (format "reg~a" n))
       (lambda (n) #f) ; never boxed
       (lambda (n) (format "argv[~a]" n))
       (lambda (n deref) "(long)")
       (lambda (n) #f)
       indent port))))

(define vm->c:emit-private-box-initializations
  ; Currently, each is filled with undefined, but specialized representations
  ;  will require something different
  (lambda (bindings indent port)
    (for-each
     (lambda (binding)
       (let* ([rep (binding-rep (get-annotation binding))]
	      [derep (rep:pointer-to rep)])
	 (fprintf port "~a~a = (~a)~a;~n~a*(~a) = scheme_undefined;~n"
		  indent
		  (vm->c:convert-symbol (zodiac:binding-var binding))
		  (vm->c:convert-type-definition rep)
		  (vm->c:type-definition->malloc derep)
		  
		  indent
		  (vm->c:convert-symbol (zodiac:binding-var binding)))))
     bindings)))

(define vm->c:emit-undefines
  (lambda (undefines indent port)
    (for-each
     (lambda (name)
       (fprintf port "#~aundef ~a~n"
		indent name))
     undefines)))

(define vm->c:emit-function-prologue
  (lambda (L port)
    (let* ([code (get-annotation L)]
	   [label (closure-code-label code)])
      (if (= 1 (length (zodiac:case-lambda-form-bodies L)))
	  (values 1 #f)
	  (begin
	    ; The foreign entry label      
	    (fprintf port "FGN~a:~n" label)
	    (let loop ([args (zodiac:case-lambda-form-args L)][i 0])
	      (if (null? args)
		  (begin
		    (fprintf port "~a~ascheme_case_lambda_wrong_count(~s, argc, argv, ~a"
			     vm->c:indent-spaces vm->c:indent-spaces
			     (vm->c:extract-inferred-name (closure-code-name code))
			     (length (zodiac:case-lambda-form-args L)))
		    (let loop ([l (zodiac:case-lambda-form-args L)])
		      (unless (null? l)
			 (let-values ([(min-arity max-arity)
				       (compiler:formals->arity (car l))])
			    (fprintf port ", ~a, ~a" min-arity max-arity)
			    (loop (cdr l)))))
		    (fprintf port ");~n")
		    (fprintf port "~a~areturn NULL;~n"
			     vm->c:indent-spaces vm->c:indent-spaces)
		    (values i #t))
		  (let ([a (car args)])
		    (cond
		     [(zodiac:sym-arglist? a)
		      (fprintf port "~a~agoto FGN~ac~a;~n" 
			       vm->c:indent-spaces vm->c:indent-spaces
			       label
			       i)
		      (values (add1 i) #t)]
		     [(zodiac:list-arglist? a)
		      (fprintf port "~a~aif (argc == ~a) goto FGN~ac~a;~n"
			       vm->c:indent-spaces vm->c:indent-spaces
			       (length (zodiac:arglist-vars a))
			       label
			       i)
		      (loop (cdr args) (add1 i))]
		     [else
		      (fprintf port "~a~aif (argc >= ~a) goto FGN~ac~a;~n"
			       vm->c:indent-spaces vm->c:indent-spaces
			       (sub1 (length (zodiac:arglist-vars a)))
			       label
			       i)
		      (loop (cdr args) (add1 i))])))))))))

(define vm->c:emit-extract-env-variables
  (lambda (code vars indent port)
    ; now pull environment variables into registers
    ; this is easy because of the way we've set up environments
    (let loop ([vars vars][undefines null])
      (if (null? vars)
	  undefines
	  (let* ([var (if (pair? vars) (car vars) vars)]
		 [vname (zodiac:binding-var var)]
		 [name (vm->c:convert-symbol vname)]
		 [fname (rep:find-field (closure-code-rep code) vname)])
	    (fprintf port (if (compiler:option:unpack-environments)
			      "~a~a = env->~a;~n"
			      "#~adefine ~a env->~a~n")
		     indent
		     name
		     fname)
	    (let ([undefines (if (compiler:option:unpack-environments)
				 undefines
				 (cons name undefines))])
	      (if (pair? vars)
		  (loop (cdr vars) undefines)
		  undefines)))))))

(define vm->c:emit-extract-bucket-variables
  (lambda (code vars indent port)
    ; pull bucket variables into registers
    (let loop ([vars vars][undefines null])
      (if (null? vars)
	  undefines
	  (let ([var (car vars)])
	    (if (const:per-load-statics-table? var)
		(begin
		  (fprintf port 
			   (if (compiler:option:unpack-environments)
			       "~aPLS = env->pls;~n"
			       "#~adefine PLS env->pls~n")
			   indent)
		  (loop (cdr vars)
			(if (compiler:option:unpack-environments)
			    undefines
			    (cons "PLS" undefines))))
		(let* ([vname var]
		       [name (vm->c:convert-symbol (vm->c:bucket-name vname))]
		       [fname (rep:find-field (closure-code-rep code) vname)])
		  (fprintf port 
			   (if (compiler:option:unpack-environments)
			       "~aG~a = env->~a;~n"
			       "#~adefine G~a env->~a~n")
			   indent
			   name
			   fname)
		  (loop (cdr vars)
			(if (compiler:option:unpack-environments)
			    undefines
			    (cons (string-append "G" name) undefines))))))))))
      
(define vm->c:emit-case-prologue
  (lambda (L which pre-decl lsuffix indent port)
    (let* ([code (get-annotation L)]
	   [case-code (list-ref (procedure-code-case-codes code) which)]
	   [label (closure-code-label code)]
	   [undefines null]
	   [used-free-set
	    ; Only unpack anchors if they're captured
	    (let* ([free-set (code-free-vars case-code)]
		   [free-list (set->list free-set)]
		   [captured-list (set->list (code-captured-vars code))]
		   [uncaptured-anchor-set
		    (list->set
		     (let loop ([l free-list])
		       (if (null? l)
			   null
			   (let ([zb (car l)])
			     (let ([a (binding-anchor (get-annotation zb))])
			       (if (and a (not (member zb captured-list)))
				   (cons a (loop (cdr l)))
				   (loop (cdr l))))))))])
	      (set-minus free-set uncaptured-anchor-set))])
      ; The foreign entry label      
      (fprintf port "FGN~a~a:~n" label lsuffix)
      ; Pull arguments to global registers      
      (vm->c:pack-global-registers! L which indent port)

      ; The local entry label
      (fprintf port "LOC~a~a:~n" label lsuffix)
      (pre-decl)
      (unless local-vars-at-top?
	(vm->c:emit-local-variable-declarations! (code-local-vars case-code) indent port))

      (when (compiler:option:unpack-environments)
	(vm->c:emit-local-variable-declarations! used-free-set indent port)
	(vm->c:emit-local-bucket-declarations! (code-global-vars case-code) indent #f port))
    
      (let ([r (closure-code-rep code)])
	(when r
	  ; (fprintf port "~aconst ~a * env;~n" indent (vm->c:convert-type-definition r))
	  (fprintf port "#~adefine env ((const ~a *)void_param)~n" indent (vm->c:convert-type-definition r))))
      
      ; Registers into local vars
      (let* ([args (zodiac:arglist-vars (list-ref (zodiac:case-lambda-form-args L) which))])
	(vm->c:extract-arguments-into-variables!
	 args
	 #t ; since regN already builds lists as appropriate
	 (lambda (n) (binding-rep (get-annotation (list-ref args n))))
	 (lambda (n) (vm->c:convert-symbol (zodiac:binding-var (list-ref args n))))
	 (lambda (n) (binding-boxed? (get-annotation (list-ref args n))))
	 (lambda (n) (format "reg~a" n))
	 (lambda (n deref) (format "(~a)"
				   (vm->c:convert-type-definition
				    (let* ([binding (get-annotation (list-ref args n))]
					   [rep (binding-rep binding)])
				      (if (and deref (binding-boxed? binding))
					  (rep:pointer-to rep)
					  rep)))))
	 (lambda (n) #f)
	 indent port))

      ; reduce register pressure by doing all the env calculations
      ; after the args have been done
      ; equate the local registers with the global argument registers
      ; starting with the env
#|
      (let ([r (closure-code-rep code)])
	(when r
	  (fprintf port "~aenv = (~a *)void_param;~n"
		   indent
		   (vm->c:convert-type-definition r))))
|#

      ; now pull environment variables into registers
      (set! undefines
	    (append (vm->c:emit-extract-env-variables
		     code
		     (set->list used-free-set)
		     indent port)
		    undefines))

      ; pull bucket variables into registers
      (set! undefines
	    (append (vm->c:emit-extract-bucket-variables
		     code
		     (set->list (code-global-vars case-code))
		     indent port)
		    undefines))

      (when (case-code-has-continue? case-code)
	(fprintf port "~awhile(1)~n" indent))

      undefines)))

(define vm->c:emit-case-epilogue
  (lambda (code which undefines indent port)
    (fprintf port "#~aundef env~n" indent)
    (vm->c:emit-undefines undefines indent port)))

(define vm->c:emit-function-epilogue
  (lambda (code close port)
    (fprintf port "~a~a /* end of function body ~a */~n" 
	     vm->c:indent-spaces close (closure-code-label code))))

(define vm->c:emit-unit-prologue
  (lambda (L indent port)
    (let* ([code (get-annotation L)]
	   [imports (zodiac:unit-form-imports L)]
	   [exports (unit-code-exports code)]
	   [import-anchors (unit-code-import-anchors code)]
	   [export-anchors (unit-code-export-anchors code)]
	   [defines (unit-code-defines code)]
	   [undefines null]
	   [import-set (set-union
			(list->set imports)
			(list->set import-anchors))]
	   [used-imports (set-intersect
			  (code-used-vars code)
			  import-set)])

      ; Only unpack import & import-anchors if they are used

      (vm->c:emit-local-variable-declarations! 
       (if local-vars-at-top?
	   used-imports
	   (set-union (set-minus (code-local-vars code) import-set) used-imports))
       indent port)
    
      (when (compiler:option:unpack-environments)
	 ; The locals are imports and internally-defined variables:
	 (vm->c:emit-local-variable-declarations! (code-free-vars code) indent port)
	 (vm->c:emit-local-bucket-declarations! (code-global-vars code) indent #f port))

      (let ([r (closure-code-rep code)])
	(when r
	   (fprintf port "~aconst ~a * env;~n"
		    indent (vm->c:convert-type-definition r))))
    
      ; map input boxes and anchors to locals 
      (let ([used (append (set->list used-imports) exports)])
	(let loop ([l (append imports exports)][al (append import-anchors export-anchors)][i 0])
	  (unless (null? l)
	    (when (memq (car l) used)
	      (fprintf port "~a~a = (Scheme_Object **)boxes[~a];~n" 
		       indent
		       (vm->c:convert-symbol (zodiac:binding-var (car l)))
		       i)
	      (fprintf port "~a~a = anchors[~a];~n" 
		       indent
		       (vm->c:convert-symbol (zodiac:binding-var (car al)))
		       i))
	    (loop (cdr l) (cdr al) (add1 i)))))

      ; create boxes for variables defined internally
      (vm->c:emit-private-box-initializations 
       (let loop ([l defines])
	 (if (null? l)
	     null
	     (if (memq (car l) exports)
		 (loop (cdr l))
		 (cons (car l) (loop (cdr l))))))
       indent port)

      (let ([r (closure-code-rep code)])
	(when r
	   (fprintf port "~aenv = (~a *)u->data;~n"
		    indent (vm->c:convert-type-definition r))))

      ; now pull environment variables into registers
      (set! undefines
	    (append (vm->c:emit-extract-env-variables
		     code
		     (set->list (code-free-vars code))
		     indent port)
		    undefines))

      ; pull bucket variables into registers
      (set! undefines
	    (append (vm->c:emit-extract-bucket-variables
		     code
		     (set->list (code-global-vars code))
		     indent port)
		    undefines))

      undefines)))

(define vm->c:emit-unit-epilogue
  (lambda (code undefines indent port)
    (vm->c:emit-undefines undefines indent port)))

(define vm->c:emit-class-prologue
  (lambda (L indent port)
    (let* ([code (get-annotation L)]
	   [public-define-bindings (class-code-public-define-bindings code)]
	   [public-lookup-bindings (class-code-public-lookup-bindings code)]
	   [override-define-bindings (class-code-override-define-bindings code)]
	   [override-lookup-bindings (class-code-override-lookup-bindings code)]
	   [inherit-bindings (class-code-inherit-bindings code)]
	   [rename-bindings (class-code-rename-bindings code)]
	   [private-bindings (class-code-private-bindings code)]
	   [arglist (zodiac:class*/names-form-init-vars L)]
	   [args (zodiac:paroptarglist-vars arglist)]
	   [any-defaults? (ormap pair? args)]
	   [undefines null])

      (unless local-vars-at-top?
	(vm->c:emit-local-variable-declarations! (code-local-vars code) indent port))
    
      (when (compiler:option:unpack-environments)
	 (vm->c:emit-local-variable-declarations! (code-free-vars code) indent port)
	 (vm->c:emit-local-bucket-declarations! (code-global-vars code) indent #f port))

      (let ([r (closure-code-rep code)])
	(when r
	  (fprintf port "~aconst ~a * env;~n"
		   indent (vm->c:convert-type-definition r))))

      (when any-defaults?
	    (fprintf port "~aint arg_set_level = ~a;~n" indent (length args)))

      (let ([r (closure-code-rep code)])
	(when r
	  (fprintf port "~aenv = (~a *)data;~n"
		   indent (vm->c:convert-type-definition r))))
      
      ; Map set-public, get-public, and inherits to boxes
      (let loop ([l (append public-define-bindings override-define-bindings)][pos 0])
	(unless (null? l)
	   (fprintf port "~a~a = (Scheme_Object * *)init_boxes[~a];~n"
		    indent
		    (vm->c:convert-symbol (zodiac:binding-var (car l)))
		    pos)
	   (loop (cdr l) (add1 pos))))
      (let loop ([l (append public-lookup-bindings override-lookup-bindings inherit-bindings rename-bindings)][pos 0])
	(unless (null? l)
	   (fprintf port "~a~a = (Scheme_Object * *)extract_boxes[~a];~n"
		    indent
		    (vm->c:convert-symbol (zodiac:binding-var (car l)))
		    pos)
	   (loop (cdr l) (add1 pos))))
      (fprintf port "~a~a = instance;~n"
	       indent (vm->c:convert-symbol (zodiac:binding-var (zodiac:class*/names-form-this L))))
      (fprintf port "~a~a = super_init;~n"
	       indent (vm->c:convert-symbol (zodiac:binding-var (zodiac:class*/names-form-super-init L))))

      ; Arguments into local vars
      (let ([get-binding (lambda (n)
			   (get-annotation (let ([arg (list-ref args n)])
					     (if (pair? arg)
						 (car arg)
						 arg))))])
	(vm->c:extract-arguments-into-variables!
	 args
	 (zodiac:list-paroptarglist? arglist)
	 (lambda (n) (binding-rep (get-binding n)))
	 (lambda (n) (vm->c:convert-symbol (zodiac:binding-var (let ([arg (list-ref args n)])
								 (if (pair? arg)
								     (car arg)
								     arg)))))
	 (lambda (n) (binding-boxed? (get-binding n)))
	 (lambda (n) (format "argv[~a]" n))
	 (lambda (n deref) "")
	 (lambda (n) (pair? (list-ref args n)))
	 indent port))
      
      ; now pull environment variables into registers
      (set! undefines
	    (append (vm->c:emit-extract-env-variables
		     code
		     (set->list (code-free-vars code))
		     indent port)
		    undefines))

      ; pull bucket variables into registers
      (set! undefines
	    (append (vm->c:emit-extract-bucket-variables
		     code
		     (set->list (code-global-vars code))
		     indent port)
		    undefines))

      ; allocate privates
      (vm->c:emit-private-box-initializations private-bindings indent port)

      ; If optional arguments were not provided, evaluate the default expressions
      (when any-defaults?
	 (fprintf port "~aswitch (arg_set_level) {~n" indent)
	 (let loop ([l args][pos 0])
	   (unless (null? l)
	      (when (pair? (car l))
		    (fprintf port "~a  case ~a:~n" indent pos)
		    (vm->c-expression (cdar l)
				      code
				      port
				      (+ (string-length indent) 3)
				      #f))
	      (loop (cdr l) (add1 pos))))
	 (fprintf port "~a}~n" indent))

      undefines)))

(define vm->c:emit-class-epilogue
  (lambda (code undefines indent port)
    (vm->c:emit-undefines undefines indent port)))

(define vm->c:convert-symbol
  (lambda (sym)
    (let ([text (symbol->string sym)])
      (let loop ([n 0])
	(if (= n (string-length text))
	    text
	    (let ([char (string-ref text n)])
	      (when (member char compiler:bad-chars)
		    (string-set! text n #\_))
	      (loop (add1 n))))))))


(define vm->c:convert-char
  (lambda (char)
    (cond 
      [(char=? char #\tab) "\\t"]
      [(char=? char #\newline) "\\n"]
      [(char=? char #\return) "\\r"]
      [(char=? char #\space) " "]
      [(or (char-alphabetic? char) (char-numeric? char)) (string char)]
      [else (let ([text (number->string (char->integer char) 8)])
	      (string-append "\\"
			     (make-string (- 3 (string-length text)) #\0)
			     text))])))

(define vm->c:convert-special-constant
  (lambda (ast)
    (cond
      [(zodiac:void? ast) "scheme_void"]
      [(zodiac:undefined? ast) "scheme_undefined"]
      [else (compiler:internal-error 
	     #f 
	     (format
	      "vm->c:convert-special-constant: ~a not correct" ast))])))
	

(define vm->c:block-statement?
  (one-of vm:if? vm:sequence?))

(define vm->c:extract-inferred-name
  (let ([nullsym (string->symbol "NULL")])
    (lambda (var)
      (cond
       [(list? var)
	(if (= (length var) 1)
	    (vm->c:extract-inferred-name (car var))
	    nullsym)]
       [(zodiac:binding? var)
	(symbol->string (zodiac:binding-orig-name var))]
       [(zodiac:bound-varref? var)
	(vm->c:extract-inferred-name (zodiac:bound-varref-binding var))]
       [(zodiac:varref? var)
	(symbol->string (zodiac:varref-var var))]
       [(not var) nullsym]
       [else (compiler:internal-error
	      #f
	      (format "vm->c:extract-inferred-name: bad var type: ~a"
		      var))]))))

(define vm->c-expression
  (lambda (ast code port indent-level no-seq-braces?)
    (let process ([ast ast] [indent-level indent-level] [own-line? #t] [braces? (not no-seq-braces?)])
      (letrec ([emit-indentation (lambda () (display
					     (make-string indent-level #\ )
					     port))]
	       [indent (lambda () (+ indent-level vm->c:indent-by))]
	       [emit (lambda s (apply fprintf (cons port s)))]
	       [emit-expr (lambda s
			    (when own-line? (emit-indentation))
			    (apply emit s))]
	       [emit-macro-application
		(lambda (ast)
		  (let ([args (vm:macro-apply-args ast)])
		    (emit "~a(" (vm:macro-apply-name ast))
		    (process (vm:macro-apply-primitive ast) indent-level #f #f)
		    (for-each (lambda (a) 
				(emit ", ~a" (vm->c:convert-symbol (zodiac:binding-var a))))
			      args)
		    (emit ")")))])

	(cond
	 
	 ;; (%sequence V ...) -> { M; ... }
	 [(vm:sequence? ast)
	  (let* ([seq (vm:sequence-vals ast)])
	    (when braces? (emit-indentation) (emit "{~n"))
	    (for-each (lambda (v)
			(process v (indent) #t #t)
			(unless (vm->c:block-statement? v) (emit ";~n")))
		      seq)
	    (when braces? (emit-indentation) (emit "}~n")))]
	 
	 ;; (if R (sequence V) (sequence V)) ->
	 ;;    if (!SCHEME_FALSEP(A)) { V ... } else { V ...}
	 [(vm:if? ast)
	  (emit-indentation)
	  (let loop ([ast ast])
	    (let ([test (vm:if-test ast)]
		  [then (vm:if-then ast)]
		  [else (vm:if-else ast)])
	      
	      (emit "if (")
	      (let ([direct? (and (vm:macro-apply? test)
				  (vm:macro-apply-bool? test))])
		(if direct?
		    (emit-macro-application test)
		    (begin
		      (emit "!SCHEME_FALSEP(")
		      (process test indent-level #f #t)
		      (emit ")"))))
	      (emit ")~n")
	      (process (vm:if-then ast) indent-level #t #t)
	      (let ([else-vals (vm:sequence-vals else)])
		(cond 
		 [(and (= 1 (length else-vals))
		       (vm:if? (car else-vals)))
		  (emit-indentation) (emit "else ")
		  (loop (car else-vals))]
		 [(not (null? else-vals))
		  (emit-indentation) (emit "else~n")
		  (process (vm:if-else ast) indent-level #f #t)]
		 [else (void)]))))]
	  
	 ;; begin0 stuff
	 [(vm:begin0-mark!? ast)
	  (let ([var (vm->c:convert-symbol
		      (vm:local-varref-var (vm:begin0-mark!-var ast)))])
	    (emit-indentation)
	    (emit "~a.val = " var)
	    (process (vm:begin0-mark!-val ast) indent-level #f #t))]
	 [(vm:begin0-setup!? ast)
	  (let ([var (vm->c:convert-symbol
		      (vm:local-varref-var (vm:begin0-setup!-var ast)))])
	    (emit-indentation)
	    (emit "if (~a.val == SCHEME_MULTIPLE_VALUES) {~n" var)
	    (emit-indentation)
	    (emit "  ~a.array = pr->ku.multiple.array;~n" var)
	    (emit-indentation)
	    (emit "  ~a.count = pr->ku.multiple.count;~n" var)
	    (emit-indentation)
	    (emit "} else ~a.array = NULL" var))]
	 [(vm:begin0-extract? ast)
	  (let ([var (vm->c:convert-symbol
		      (vm:local-varref-var (vm:begin0-extract-var ast)))])
	    (emit "(pr->ku.multiple.array = ~a.array," var)
	    (emit " pr->ku.multiple.count = ~a.count, " var)
	    (emit " ~a.val)" var))]

	 ;; single value: (set! L R) -> L = R;
	 ;; multiple value:
	 [(vm:set!? ast)
	  (let* ([process-target!
		  (lambda (target)
		    (let ([type (car target)]
			  [target (cdr target)])
		      (cond
			[(eq? type target-type:lexical) 
			 (process target indent-level #f #t)]
			[(eq? type target-type:global)
			 (let ([bucket-name (vm->c:convert-symbol
					     (vm->c:bucket-name
					      target))])
			   (emit "G~a->val" bucket-name))]
			[else (compiler:internal-error 
			       #f
			       (format "~a: bad set! target type" type))])))]
		 [process-set!
		  (lambda (target val process-val?)
		    (let ([mode (vm:set!-mode ast)])
		      (if mode
			  (begin
			    (emit "scheme_set_global_bucket(~s, " (car mode))
			    (emit "G~a, " (vm->c:convert-symbol
					   (vm->c:bucket-name (cdr target))))
			    (if process-val? 
				(process val indent-level #f #t)
				(emit val))
			    (emit ", ~a)" (cadr mode)))
			  (begin
			    (process-target! target)
			    (emit " = ")
			    (if process-val? 
				(process val indent-level #f #t)
				(emit val))))))]
		 [vars (vm:set!-vars ast)]
		 [val (vm:set!-val ast)]
		 [num-to-set (length vars)]
		 [return-arity
		  (or (and ((one-of vm:global-varref?
				    vm:local-varref?
				    vm:static-varref?
				    vm:primitive-varref?
				    vm:symbol-varref?
				    vm:struct-ref?
				    vm:deref?
				    vm:ref?
				    vm:cast?
				    vm:immediate?) ast)
			   1)
		      (and (vm:apply? val) 
			   (vm:apply-prim val)
			   (let ([proc (global-defined-value* (vm:apply-prim val))])
			     (and
			      ((one-of primitive? primitive-closure?) proc)
			      (primitive-result-arity proc))))
		      (and (vm:struct? val)
			   (length (vm:struct-fields val)))
			   
			   )])
	    (emit-indentation)
	    (cond
	      [(or (vm:apply? val) (vm:struct? val))
	       (let ([return-arity-ok?
		      (and return-arity
			   (number? return-arity)
			   (= return-arity num-to-set))])
		 (if (= num-to-set 1)		      
		     
		     (process-set! (car vars) val #t)
		     
		     (begin
		       (emit "{ Scheme_Object * res = ")
		       (process val indent-level #f #t)
		       (emit "; ")
		       (unless return-arity-ok?
			 (emit "CHECK_MULTIPLE_VALUES(res, ~a);" num-to-set))
		       (emit "}")
		       (if (not (null? vars))
			   (emit "~n"))
		       (let loop ([vars vars] [n 0])
			 (unless (null? vars)
			   (emit-indentation)
			   (process-set! (car vars) (format "scheme_multiple_array[~a]" n) #f)
			   (emit ";~n")
			   (loop (cdr vars) (+ n 1))))
		       )))]
	      
		;; not an application.
	      [else
	       (if (= num-to-set 1)
		   (process-set! (car vars) val #t)    
		   (begin
		     (emit "CHECK_MULTIPLE_VALUES(")
		     (process val indent-level #f #t)
		     (emit ", ~a)" num-to-set)))]))]
	   
	 
	 ;; (set-global! x R)
	 

	 ;; (%args A ...) -> arg[0] = A; ...
	 [(vm:args? ast)
	  ;; skip tail_buf setup if no args
	  (when (and (eq? arg-type:tail-arg (vm:args-type ast))
		     (not (null? (vm:args-vals ast))))
	    (emit-indentation)
	    (emit "tail_buf = scheme_tail_apply_buffer_wp(~a, pr);~n"
		  (length (vm:args-vals ast))))
	  (if (null? (vm:args-vals ast))
	      (emit-indentation)
	      (let loop ([n 0] [args (vm:args-vals ast)])
		(unless (null? args)
		  (emit-indentation)
		  (let ([argtype (vm:args-type ast)])
		    (cond
		      [(eq? arg-type:arg argtype) (emit "arg[~a] = " n)]
		      [(eq? arg-type:tail-arg argtype) (emit "tail_buf[~a] = " n)]
		      [(eq? arg-type:register argtype) (emit "reg~a = (long)" n)]
		      [else (compiler:internal-error 
			     #f (format "vm->c: ~a unknown arg type" (vm:args-type ast)))]))
		  ; (emit "DEBUG_CHECK(") ;; DEBUGGING
		  (process (car args) indent-level #f #t)
		  ; (emit ")") ;; DEBUGGING
		  (unless (null? (cdr args))
		    (emit ";~n"))
		  (loop (add1 n) (cdr args)))))]

	 [(vm:register-args? ast)
	  (let ([vars (vm:register-args-vars ast)]
		[vals (vm:register-args-vals ast)])
	    (let loop ([vars vars][vals vals])
	      (let ([var (car vars)]
		    [val (car vals)])
		(emit-indentation)
		(emit "~a = " (vm->c:convert-symbol (zodiac:binding-var var)))
		(process val indent-level #f #f)
		(unless (null? (cdr vars))
		  (emit ";~n")
		  (loop (cdr vars) (cdr vals))))))]

	 ;; (alloc ) -> malloc
	 ;; a bit complicated
	 [(vm:alloc? ast)
	  (emit (vm->c:type-definition->malloc (vm:alloc-type ast)))]

	 ;; (make-closure) -> _scheme_make_c_closure
	 [(vm:make-procedure-closure? ast)
	  (emit "_scheme_make_c_proc_closure~a(vehicle_~a, " 
		(if (vm:make-procedure-closure-empty? ast)
		    "_empty"
		    "")
		(vm:make-procedure-closure-vehicle ast))
	  (process (vm:make-closure-closure ast) indent-level #f #t)
	  (emit ", ~s, ~a, ~a)" 
		(vm->c:extract-inferred-name (vm:make-procedure-closure-name ast))
		(vm:make-procedure-closure-min-arity ast)
		(vm:make-procedure-closure-max-arity ast))]

	 [(vm:make-case-procedure-closure? ast)
	  (emit "_scheme_make_c_case_proc_closure~a(vehicle_~a, " 
		(if (vm:make-case-procedure-closure-empty? ast)
		    "_empty"
		    "")
		(vm:make-case-procedure-closure-vehicle ast))
	  (process (vm:make-closure-closure ast) indent-level #f #t)
	  (emit ", ~s, ~a, S.casesArities[~a])" 
		(vm->c:extract-inferred-name (vm:make-case-procedure-closure-name ast))
		(vm:make-case-procedure-closure-num-cases ast)
		(vm:make-case-procedure-closure-case-arities ast))]

	 [(vm:make-unit-closure? ast)
	  (emit "_scheme_make_c_unit_closure~a(vehicle_~a, " 
		(if (vm:make-unit-closure-empty? ast)
		    "_empty"
		    "")
		(vm:make-unit-closure-vehicle ast))
	  (process (vm:make-closure-closure ast) indent-level #f #t)
	  (emit ", ~s, ~a, ~a, ~a)" 
		(vm->c:extract-inferred-name (vm:make-unit-closure-name ast))
		(vm:make-unit-closure-num-imports ast)
		(vm:make-unit-closure-num-exports ast)
		(let ([offset (vm:make-unit-closure-exports-offset ast)])
		  (if (negative? offset)
		      "NULL"
		      (format "S.unit_exports + ~a" offset))))]
	 
	 [(vm:make-class-closure? ast)
	  (emit "_scheme_make_c_class_closure(S.classAssemblies[~a], " (vm:make-class-closure-assembly ast))
	  (let ([cc (vm:make-closure-closure ast)])
	    (if cc
		(process (vm:make-closure-closure ast) indent-level #f #t)
		(emit "NULL")))
	  (emit ", arg[0], arg + 1)")]

	 [(vm:deref? ast)
	  (emit "(*")
	  (process (vm:deref-var ast) indent-level #f #t)
	  (emit ")")]

	 [(vm:ref? ast)
	  (emit "(&")
	  (process (vm:ref-var ast) indent-level #f #t)
	  (emit ")")]
	 
	 ; optimize (*X).Y to X->Y
	 [(vm:struct-ref? ast)
	  (let ([var (vm:struct-ref-var ast)])
	    (if (vm:deref? var)
		(begin
		  (process (vm:deref-var var) indent-level #f #t)
		  (emit "->"))
		(begin
		  (process (vm:struct-ref-var ast) indent-level #f #t)
		  (emit ".")))
	    (emit "~a" (vm->c:convert-symbol (vm:struct-ref-field ast))))]

	 [(vm:cast? ast)
	  (emit "(")
	  (emit (vm->c:convert-type-definition (vm:cast-rep ast)))
	  (emit ")(")
	  (process (vm:cast-val ast) indent-level #f #t)
	  (emit ")")]

	 ;; Most of the work has been done by setting up a compound assembly,
	 ;; and the arguments are ready. Use the assembly with the constituent 
	 ;; values.
	 [(vm:compound? ast)
	  (emit-expr (format "scheme_make_compound_unit(S.compoundAssemblies[~a], arg)"
			     (vm:compound-assembly ast)))]

	 [(vm:invoke? ast)
	  (emit-expr (format "_scheme~a_invoke~a_unit~a(arg[0], ~a, arg + 1, arg + 1 + ~a~a)"
			     (if (vm:invoke-tail? ast) "_tail" "")
			     (if (vm:invoke-open? ast) "_open" "")
			     (if (and (vm:invoke-multi? ast)
				      (not (vm:invoke-tail? ast)))
				 "_multi" 
				 "")
			     (vm:invoke-num-variables ast)
			     (vm:invoke-num-variables ast)
			     (if (vm:invoke-open? ast) 
				 (format ", ~s" (if (vm:invoke-name-specifier ast)
						    (symbol->string (vm:invoke-name-specifier ast))
						    (string->symbol "NULL")))
				 "")))]

	 ;; Interfaces are similar to compound units: args are ready
	 [(vm:interface? ast)
	  (emit-expr (format "scheme_create_interface(S.interfaceAssemblies[~a], arg)"
			     (vm:interface-assembly ast)))]
	 
	 ;; (continue) -> continue;
	 [(vm:continue? ast)
	  (unless (compiler:option:disable-interrupts)
	    (emit-expr "_scheme_check_for_break_wp(1, pr);~n"))
	  (emit-expr "continue")]
	 
	 ;; use NULL instead of tail_buf if no args
	 ;; (tail-apply A <argc>) -> return _scheme_tail_apply(A, argc);
	 [(vm:tail-apply? ast)
	  (emit-expr "return _scheme_tail_apply_no_copy_wp(")
	  (process (vm:tail-apply-closure ast) indent-level #f #t)
	  (let ([c (vm:tail-apply-argc ast)])
	    (emit ", ~a, ~a, pr)" c (if (zero? c) "NULL" 'tail_buf)))]
	 
	 ;; (tail-call <label> <closure>) -> void_param = SCHEME_CLSD_PRIM_DATA(<closure>);
	 ;;                                  goto LOC<label>;
	 [(vm:tail-call? ast)
	  (when (vm:tail-call-set-env? ast)
	    (emit-indentation)
	    (emit "void_param = SCHEME_CLSD_PRIM_DATA(")
	    (process (vm:tail-call-closure ast) indent-level #f #t)
	    (emit ");~n"))
	  ;; be nice to threads & user breaks:
	  (unless (compiler:option:disable-interrupts)
	    (emit-indentation)
	    (emit "_scheme_check_for_break_wp(1, pr);~n"))
	  (emit-indentation)
	  ; unless its to a variable arity function! ARGH
	  (let* ([label (vm:tail-call-label ast)]
		 [l (if (number? label)
			label
			(format "~ac~a" (car label) (cdr label)))])
	    (emit "goto LOC~a" l))]

	 ;; (return R) -> return R
	 [(vm:return? ast)
	  (emit-indentation)
	  (emit "return ")
	  (process (vm:return-val ast) indent-level #f #t)]

	 ;; fortunately, void contexts can accept any number of values,
	 ;; so there's no need to check for return arity
	 [(vm:void? ast)
	  (emit-indentation)
	  (process (vm:void-val ast) indent-level #f #t)]
	 
	 ;; (global-varref x) --> GLOBAL_VARREF(x)
	 [(vm:global-varref? ast)
	  (emit-expr "GLOBAL_VARREF(G~a)"
		     (vm->c:convert-symbol
		      (vm->c:bucket-name
		       (vm:global-varref-var ast))))]

	 ;; (global-varref x) --> Gx
	 [(vm:bucket? ast)
	  (emit-expr "G~a"
		     (vm->c:convert-symbol
		      (vm->c:bucket-name
		       (vm:bucket-var ast))))]

	 [(vm:per-load-statics-table? ast)
	  (emit-expr "PLS")]

	 ;; use apply-known? flag
	 ;; 0 args => pass NULL for arg vector
	 ;; (apply A <argc>) --> _scheme_apply(A, argc, arg)
	 [(vm:apply? ast)
	  (emit-expr "")
	  (when (vm:apply-simple-tail-prim? ast)
	    (emit "return "))
	  (emit "_scheme_~a("
		(let ([v (global-defined-value* (vm:apply-prim ast))])
		  (cond
		   [(and (primitive-closure? v) (simple-return-primitive? v))
		    (if (or (vm:apply-multi? ast)
			    (primitive-result-arity v))
			"direct_apply_closed_primitive_multi"
			"direct_apply_closed_primitive")]
		   [(and (primitive? v) (simple-return-primitive? v))
		    (if (or (vm:apply-multi? ast)
			    (primitive-result-arity v))
			"direct_apply_primitive_multi"
			"direct_apply_primitive")]
		   [(vm:apply-known? ast) 
		    (if (vm:apply-multi? ast)
			(if (compiler:option:disable-interrupts)
			    "direct_apply_closed_primitive_multi_fv"
			    "apply_known_closed_prim_multi")
			(if (compiler:option:disable-interrupts)
			    (if (compiler:option:unsafe)
				"direct_apply_closed_primitive_multi_fv"
				"direct_apply_closed_primitive_fv")
			    "apply_known_closed_prim"))]
		   [(vm:apply-multi? ast) "apply_multi"]
		   [else "apply"])))
	  (process (vm:apply-closure ast) indent-level #f #t)
	  (let ([c (vm:apply-argc ast)])
	    (emit ", ~a, ~a)" c (if (zero? c) "NULL" 'arg)))]

	 ;; Inlined macro-based applications
	 [(vm:macro-apply? ast) 
	  (emit-expr "")
	  (when (vm:macro-apply-tail? ast)
	    (emit "return "))
	  (when (vm:macro-apply-bool? ast) (emit "(("))
	  (emit-macro-application ast)
	  (when (vm:macro-apply-bool? ast) (emit ") ? scheme_true : scheme_false)"))]
	 
	 [(vm:call? ast)
	  (emit-expr "_scheme_force_value(compiled(SCHEME_CLSD_PRIM_DATA(")
	  (process (vm:call-closure ast) indent-level #f #t)
	  (emit "), 0, arg))")]

	 ;; struct
	 ;; the hairy work has been moved into c_struct_imp, which is
	 ;; in compiled.h
	 ;;
	 [(vm:struct? ast)
	  (let* ([type (zodiac:read-object (vm:struct-type ast))]
		 [fields (map zodiac:read-object (vm:struct-fields ast))]
		 [type-name (symbol->string type)]
		 [field-names (map symbol->string fields)]
		 [c-type-name (vm->c:convert-symbol (compiler:gensym))]
		 [ids-created (+ 3 (* 2 (length fields)))]
		 [super (vm:struct-super ast)])
	    (emit-expr "c_struct_imp(~a, " (if (vm:struct-multi? ast) "1" "0"))
	    (if super (process super indent-level #f #t) (emit "NULL"))
	    (emit ", ~a, \"~a\", "
		  (length field-names)
		  type-name)
	    (for-each (lambda (f) (emit "\"~a\", " f)) field-names)
	    (emit "NULL)"))]
	 
	 ;; (bound-varref x) -> x
	 [(vm:local-varref? ast) 
	  (emit-expr (vm->c:convert-symbol
		      (vm:local-varref-var ast)))]
	 
	 ;; (primitive-varref x) -> x->val
	 [(vm:primitive-varref? ast)
	  (emit-expr "P.~a"
		     (vm->c:convert-symbol
		      (vm->c:bucket-name
		       (vm:primitive-varref-var ast))))]
	 
	 ;; (symbol-varref x) -> symbols[x]
	 [(vm:symbol-varref? ast)
	  (emit-expr "~a[~a]" 
		     (vm->c:SYMBOLS-name)
		     (vm:symbol-varref-var ast))]

	 ;; (inexact-varref x) -> inexacts[x]
	 [(vm:inexact-varref? ast)
	  (emit-expr "~a[~a]" 
		     (vm->c:INEXACTS-name)
		     (vm:inexact-varref-var ast))]

	 [(vm:per-load-static-varref? ast)
	  (emit-expr "PLS->~a" (vm->c:convert-symbol (vm:static-varref-var ast)))]
	 
	 [(vm:static-varref? ast)
	  (emit-expr "S.~a" (vm->c:convert-symbol (vm:static-varref-var ast)))]
	 
	 ;; (immediate x)
	 [(vm:immediate? ast)
	  (let ([ast (vm:immediate-text ast)])
	    (cond 
	     
	     ;;--------------------------------------------------------------
	     ;; CONSTANTS
	     ;;
	      ; labels
	     [(number? ast)
	      (emit-expr "~a" ast)]
	     
	     [(zodiac:boolean? ast)
	      (if (zodiac:read-object ast)
		  (emit-expr "scheme_true")
		  (emit-expr "scheme_false"))]
	     
	     [(zodiac:number? ast)
	      (emit-expr "scheme_make_integer(~a)" (zodiac:read-object ast))]
	     
	     [(zodiac:char? ast)
	      (emit-expr "scheme_make_character('~a')"
			 (vm->c:convert-char
			  (zodiac:read-object ast)))]
	     
	     [(zodiac:list? ast)
	      (unless (null? (zodiac:read-object ast))
		(compiler:internal-error ast "vm->c: immediate list that is not null"))
	      (emit-expr "scheme_null")]
	     
	     [(or (zodiac:void? ast) (zodiac:undefined? ast))
	      (emit-expr (vm->c:convert-special-constant ast))]
	     
	     [else (compiler:internal-error
		    ast
		    (format "vm->c-expression: ~a not an immediate" ast))]))]

	 [(vm:build-constant? ast)
	  (let ([ast (vm:build-constant-text ast)])
	    (cond
	      [(zodiac:string? ast) 
	       (fprintf port "scheme_make_string(~s)" (zodiac:read-object ast))]
	      [(zodiac:symbol? ast) 
	       (let ([s (symbol->string (zodiac:read-object ast))])
		 (emit-expr "scheme_intern_exact_symbol(~s, ~a)" s (string-length s)))]
	      [(zodiac:number? ast)
	       (let process ([num (zodiac:read-object ast)])
		 (cond
		   ; NaN, inf
		   [(member num  (list +NaN.0 +inf.0 -inf.0)) 
		    (emit-expr "scheme_eval_string(\"~a\", env)" num)]
		   ; complex numbers
		   [(and (complex? num) (not (zero? (imag-part num))))
		    (emit-expr "scheme_make_complex(")
		    (process (real-part num))
		    (emit ", ")
		    (process (imag-part num))
		    (emit ")")]
		   ; floating point numbers
		   [(inexact? num) (emit-expr "scheme_make_double(~a)" num)]
		   ; integers (fixnums & bignums)
		   [(integer? num)
		    (if (vm:fixnum? num) 
			(emit-expr "scheme_make_integer(~a)" num)
			(emit-expr "scheme_read_bignum(\"~a\", 10)" num))]
		   ; rational numbers
		   [else
		    (emit-expr "scheme_make_rational(")
		    (process (numerator num))
		    (emit ", ")
		    (process (denominator num))
		    (emit ")")]))]
		    
		
	      [else (compiler:internal-error
		     ast
		     (format "vm:build-constant: not supported ~a" ast))]))]
	 
	 [else (compiler:internal-error #f (format "vm2c: ~a not supported" ast))])))))
)
