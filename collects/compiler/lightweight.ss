; lightweight.ss
;; code for Steckler/Wand lightweight closures

(unit/sig compiler:lightweight^
 (import (compiler:option : compiler:option^)
	 compiler:library^
	 compiler:cstructs^
	 compiler:zlayer^
	 compiler:const^
	 compiler:top-level^
	 compiler:driver^
	 mzlib:function^
	 (zodiac : zodiac:system^))

 ; : symbols -> top-level-varref/bind's
 (define *top-level-var-table* (make-hash-table)) 

 (define (hash-table-lookup tbl key)
   (with-handlers 
    ([(lambda (_) #t)
      (lambda (_) #f)])
    (hash-table-get tbl key)))

 (define (top-var-lookup sym)
   (hash-table-lookup *top-level-var-table* sym))

 ; set operations, locally useful

 (define (fold-sets sets) ; fold union over a list of sets
   (foldl set-union empty-set sets))

 ; Zodiac annotations

 (define (add-annotation name init-fn) 
   (let-values
    ([(getter setter) (zodiac:register-client name init-fn)])
    (values
     (lambda (parsed) (getter (zodiac:parsed-back parsed)))
     (lambda (parsed val) (setter (zodiac:parsed-back parsed) val)))))

 ; scope-binders is set of Zodiac bindings

 (define-values
   (scope-binders set-scope-binders!)
   (add-annotation 'scope-binders (lambda () #f)))

 ; phi is set of lambdas for annotating an ast node e such that
 ; IF e evaluates to a lambda v
 ; THEN v is an element of phi

 (define-values
   (phi set-phi!)
   (add-annotation 'phi (lambda () empty-set)))

 ; phi-closure is set of lambdas closed under the rules:
 ; (l x . t) in (phi e)         => (l x . t) in (phi-closure e)
 ; (l x . t) in (phi-closure e) => (phi t) in (phi-closure e)

 (define-values
   (phi-closure set-phi-closure!)
   (add-annotation 'phi-closure (lambda () #f)))

 ; theta is a set of Zodiac lexical-binding's for annotating an ast node e such that:
 ;    IF   x bound to value v in the current environment
 ;    AND  e evaluates to a lambda l
 ;    THEN l was closed in an environment where l was bound to v

 (define-values
   (theta set-theta!)
   (add-annotation 'theta (lambda () #f)))

 ; pi is a sequence of variables (list of Zodiac lexical-bindings) that represent 
 ; an application protocol.  If in (e1 e2), e1 has the 
 ; protocol (x1 ... xn), we'll transform this to (clos-app e1 x1 ... xn e2)

 (define-values
   (pi set-pi!)
   (add-annotation 'pi (lambda () #f)))

 ; equiv-pi is an equivalence class of terms with the same
 ; application protocol

 (define-values
   (equiv-pi set-equiv-pi!)
   (add-annotation 'equiv-pi (lambda () #f)))

 ; candidate-vars is a set of Zodiac lexical-binding's for annotating pi-equivalence
 ; class representatives.  For a given pi-equivalence class C, candidate-vars is the 
 ; set of the binders of free variables of lambda's in the phi's of all application 
 ; operators in C.  The pi chosen for the equivalence class will be a subset of
 ; candidate-vars

 (define-values
   (candidate-vars set-candidate-vars!)
   (add-annotation 'candidate-vars (lambda () empty-set)))

 ; escape? is a predicate which is true iff a function may escape
 ; from a unit

 (define-values
   (escape? set-escape!)
   (add-annotation 'escape? (lambda () #f)))

 ; unknown? is a predicate which is true iff an expression may
 ; evaluate to an unknown procedure

 (define-values
   (unknown? set-unknown!)
   (add-annotation 'unknown? (lambda () #f)))

 ; disjoint set union-find

 ; an equivalence class is represented as a Scheme list
 ; the representative of the class is the last pair of the list
 ; there may be many lists culminating in the same representative

 (define (new-equiv-class elt) (list elt))

 (define (equiv-class-find c)
   (let ([rep (last-pair c)])
     (when (not (eq? c rep)) ; path compression for next lookup
	   (set-cdr! c rep))
     rep))

 (define (equiv-class-union elt1 elt2)
   (let ([c1 (equiv-class-find elt1)]
	 [c2 (equiv-class-find elt2)])

     ; merge lists
     ; pick c1 as the representative

     (unless (eq? c1 c2)
	     (set-cdr! c2 c1))
     c1))

 ; pi-equiv- specific procedures

 (define (equiv-pi-rep ast)
   (car (equiv-class-find (equiv-pi ast))))

 (define (merge-pi-classes! ast1 ast2)
   (let ([merged-class (equiv-class-union (equiv-pi ast1)
					  (equiv-pi ast2))])
     (set-equiv-pi! ast1 merged-class)
     (set-equiv-pi! ast2 merged-class)))

 ; zactor% is the class of objects that knows how to do things at 
 ; different kinds of Zodiac nodes

 ; unless otherwise specified, a zactor% object performs the
 ; default action at a given node

 (define zactor%
   (class object% ()
	  (public 

	   ; default

	   [default-action void]

	   ; core Scheme

	   [if-form-action default-action]
	   [set!-form-action default-action]
	   [define-values-form-action default-action]
	   [let-values-form-action default-action]
	   [letrec*-values-form-action default-action]
	   [top-level-varref/bind-action default-action]
	   [varref-action default-action]
	   [binding-action default-action]
	   [quote-form-action default-action]
	   [case-lambda-form-action default-action]
	   [app-action default-action]
	   [begin-form-action default-action]
	   [begin0-form-action default-action]
	   [struct-form-action default-action]

	   ; objects

	   [interface-form-action default-action]
	   [class*/names-form-action default-action]

	   ; object clauses

	   [public-clause-action default-action]
	   [override-clause-action default-action]
	   [private-clause-action default-action]
	   [inherit-clause-action default-action]
	   [rename-clause-action default-action]
	   [sequence-clause-action default-action]

	   ; units

	   [unit-form-action default-action]
	   [compound-unit-form-action default-action]
	   [invoke-unit-form-action default-action] 
	   [invoke-open-unit-form-action default-action])))

 (define generic-default-action (make-generic zactor% default-action))
 (define generic-if-form-action (make-generic zactor% if-form-action))
 (define generic-set!-form-action (make-generic zactor% set!-form-action))
 (define generic-define-values-form-action (make-generic zactor% define-values-form-action))
 (define generic-let-values-form-action (make-generic zactor% let-values-form-action))
 (define generic-letrec*-values-form-action (make-generic zactor% letrec*-values-form-action))
 (define generic-top-level-varref/bind-action (make-generic zactor% top-level-varref/bind-action))
 (define generic-varref-action (make-generic zactor% varref-action))
 (define generic-binding-action (make-generic zactor% binding-action))
 (define generic-quote-form-action (make-generic zactor% quote-form-action))
 (define generic-case-lambda-form-action (make-generic zactor% case-lambda-form-action))
 (define generic-app-action (make-generic zactor% app-action))
 (define generic-begin-form-action (make-generic zactor% begin-form-action))
 (define generic-begin0-form-action (make-generic zactor% begin0-form-action))
 (define generic-struct-form-action (make-generic zactor% struct-form-action))
 (define generic-interface-form-action (make-generic zactor% interface-form-action))
 (define generic-class*/names-form-action (make-generic zactor% class*/names-form-action))
 (define generic-public-clause-action (make-generic zactor% public-clause-action))
 (define generic-override-clause-action (make-generic zactor% override-clause-action))
 (define generic-private-clause-action (make-generic zactor% private-clause-action))
 (define generic-inherit-clause-action (make-generic zactor% inherit-clause-action))
 (define generic-rename-clause-action (make-generic zactor% rename-clause-action))
 (define generic-sequence-clause-action (make-generic zactor% sequence-clause-action))
 (define generic-unit-form-action (make-generic zactor% unit-form-action))
 (define generic-compound-unit-form-action (make-generic zactor% compound-unit-form-action))
 (define generic-invoke-unit-form-action (make-generic zactor% invoke-unit-form-action))
 (define generic-invoke-open-unit-form-action (make-generic zactor% invoke-open-unit-form-action))

 ; Zodiac structure walker
 ; an action is procedure that takes ast

 ; ast may be a Zodiac parsed object or an object clause

 (define (traverse-ast-with-zactor ast zactor)

   (let do-traverse
       ([ast ast])

     (cond

      [(zodiac:if-form? ast)

       ((generic-if-form-action zactor) ast)

       (do-traverse (zodiac:if-form-test ast))
       (do-traverse (zodiac:if-form-then ast))
       (do-traverse (zodiac:if-form-else ast))]
      
      [(zodiac:set!-form? ast)

       ((generic-set!-form-action zactor) ast)

       (do-traverse (zodiac:set!-form-var ast))
       (do-traverse (zodiac:set!-form-val ast))]
      
      [(zodiac:define-values-form? ast)

       ((generic-define-values-form-action zactor) ast)

       (let* ([the-vars (zodiac:define-values-form-vars ast)]
	      [the-val (zodiac:define-values-form-val ast)])

	 (for-each do-traverse the-vars)

	 ; if the-vars are lexical, they're already in scope
	 ; from the enclosing unit or class

	 (do-traverse the-val))]
      
      [(zodiac:let-values-form? ast)

       ((generic-let-values-form-action zactor) ast)

       (let* ([binderss (zodiac:let-values-form-vars ast)])

	 (for-each 
	  (lambda (binders)
	    (for-each do-traverse binders))
	  binderss)

	 (for-each do-traverse (zodiac:let-values-form-vals ast))

	 (do-traverse (zodiac:let-values-form-body ast)))]
      
      [(zodiac:letrec*-values-form? ast)

       ((generic-letrec*-values-form-action zactor) ast)

       (let* ([binderss (zodiac:letrec*-values-form-vars ast)])

	 ; the-vars in scope at all binders and vals

	 (for-each 
	  (lambda (binders)
	    (for-each 
	     (lambda (binder) 
	       (do-traverse binder))
	     binders))
	  binderss)

	 (for-each 
	  (lambda (val) 
	    (do-traverse val))
	  (zodiac:letrec*-values-form-vals ast))

	 ; as well as in the body

	 (do-traverse (zodiac:letrec*-values-form-body ast)))]
      
      [(zodiac:top-level-varref/bind? ast) ; must be before varref? test

       ((generic-top-level-varref/bind-action zactor) ast)]

      [(zodiac:varref? ast) 

       ((generic-varref-action zactor) ast)]
      
      [(zodiac:binding? ast) 

       ((generic-binding-action zactor) ast)]
      
      [(zodiac:quote-form? ast) 

       ((generic-quote-form-action zactor) ast)]
      
      [(zodiac:case-lambda-form? ast) 

       ((generic-case-lambda-form-action zactor) ast)

       (let* ([raw-args (zodiac:case-lambda-form-args ast)]

	      ; raw-args : arglist list

	      [binderss (map zodiac:arglist-vars raw-args)])

	 ; binderss : lexical-binding list list


	 (for-each
	  (lambda (bs)
	    (for-each
	     do-traverse
	     bs))
	  binderss)

	 (for-each 
	  do-traverse
	  (zodiac:case-lambda-form-bodies ast)))]

      [(zodiac:app? ast)

       ((generic-app-action zactor) ast)

       (do-traverse (zodiac:app-fun ast))
       (for-each do-traverse (zodiac:app-args ast))]

      [(zodiac:begin-form? ast)

       ((generic-begin-form-action zactor) ast)

       (for-each do-traverse (zodiac:begin-form-bodies ast))]
      
      [(zodiac:begin0-form? ast)

       ((generic-begin0-form-action zactor) ast)

       (for-each do-traverse (zodiac:begin0-form-bodies ast))]
      
      [(zodiac:struct-form? ast) 

       ((generic-struct-form-action zactor) ast)]

      [(zodiac:interface-form? ast) 

       ((generic-interface-form-action zactor) ast)

       (for-each do-traverse (zodiac:interface-form-super-exprs ast))]

      [(zodiac:class*/names-form? ast) 

       ((generic-class*/names-form-action zactor) ast)

       (let* ([this (zodiac:class*/names-form-this ast)]
	      [super-init (zodiac:class*/names-form-super-init ast)]
	      [super-expr (zodiac:class*/names-form-super-expr ast)]
	      [interfaces (zodiac:class*/names-form-interfaces ast)]
	      [init-vars (zodiac:class*/names-form-init-vars ast)]
	      [init-vars-vars (zodiac:paroptarglist-vars init-vars)]
	      [inst-clauses (zodiac:class*/names-form-inst-clauses ast)])

	 (do-traverse this)
	 (do-traverse super-init)
	 (do-traverse super-expr)

	 (for-each 
	  do-traverse
	  interfaces)

	 (for-each
	  (lambda (v)
	    (if (pair? v) 

		; v = binder . exp

		(for-each do-traverse v)

		; v = binder 

		(do-traverse v)))

	  init-vars-vars)

	 (for-each
	  do-traverse
	  inst-clauses))]

      ; object clauses

      [(zodiac:public-clause? ast)
       (let ([exprs (zodiac:public-clause-exprs ast)]
	     [internals (zodiac:public-clause-internals ast)])

	 ((generic-public-clause-action zactor) ast)
	 
	 (for-each do-traverse internals)
	 (for-each do-traverse exprs))]
      
      [(zodiac:override-clause? ast)
       (let ([exprs (zodiac:override-clause-exprs ast)]
	     [internals (zodiac:override-clause-internals ast)])

	 ((generic-override-clause-action zactor) ast)
	 
	 (for-each do-traverse internals)
	 (for-each do-traverse exprs))]
      
      [(zodiac:private-clause? ast)

       (let ([exprs (zodiac:private-clause-exprs ast)]
	     [internals (zodiac:private-clause-internals ast)])

	 ((generic-private-clause-action zactor) ast)
	 
	 (for-each do-traverse internals)
	 (for-each do-traverse exprs))]
      
      [(zodiac:inherit-clause? ast)
       (let ([internals (zodiac:inherit-clause-internals ast)])

	 ((generic-inherit-clause-action zactor) ast)
	 
	 (for-each do-traverse internals))]
      
      [(zodiac:rename-clause? ast)
       (let ([internals (zodiac:rename-clause-internals ast)])

	 ((generic-rename-clause-action zactor) ast)
	 
	 (for-each do-traverse internals))]

      [(zodiac:sequence-clause? ast)

       ((generic-sequence-clause-action zactor) ast)
       
       (for-each do-traverse (zodiac:sequence-clause-exprs ast))]

      ; units

      [(zodiac:unit-form? ast)

       (let* ([imports (zodiac:unit-form-imports ast)]
	      [clauses (zodiac:unit-form-clauses ast)]
	      [unit-code (get-annotation ast)]
	      [defines (unit-code-defines unit-code)]
	      [unit-traverse do-traverse])

	 ((generic-unit-form-action zactor) ast)

	 (for-each unit-traverse defines)
	 (for-each unit-traverse imports)
	 (for-each unit-traverse clauses))]

      [(zodiac:compound-unit-form? ast)

       (let* ([imports (zodiac:compound-unit-form-imports ast)]
	      [links (zodiac:compound-unit-form-links ast)]
	      [unit-args (filter zodiac:lexical-varref? (map cddr links))]
	      [linked-units (map cadr links)]
	      [compound-unit-traverse do-traverse])

	 ((generic-compound-unit-form-action zactor) ast)

	 (for-each do-traverse imports)
	 (for-each compound-unit-traverse unit-args)
	 (for-each compound-unit-traverse linked-units))]

      [(zodiac:invoke-unit-form? ast)

       (let ([unit (zodiac:invoke-unit-form-unit ast)]
	     [variables (zodiac:invoke-unit-form-variables ast)])

	 ((generic-invoke-unit-form-action zactor) ast)

	 (do-traverse unit)
	 (for-each do-traverse variables))]

      [(zodiac:invoke-open-unit-form? ast)

       (let ([unit (zodiac:invoke-open-unit-form-unit ast)]
	     [variables (zodiac:invoke-open-unit-form-variables ast)])

	 ((generic-invoke-open-unit-form-action zactor) ast)

	 (do-traverse unit)
	 (for-each do-traverse variables))])))

 ; alternate structure walker, passes scope variables

 (define (traverse-ast-with-scope-zactor ast zactor)

   (let ([add-binders-to-scope
	  (lambda (old-bindings new-bindings)
	    ;; Really a set-union, but old-bindings and new-bindings are always disjoint
	    (list->set (append (filter 
				(lambda (v)
				  (let ([anno (get-annotation v)])
				    (and anno
					 (binding-known? anno))))
				new-bindings)
			       (set->list old-bindings))))])
     (let do-scope-traverse
	 ([ast ast]
	  [binders empty-set])

       (let* ([rec-traverse 
	       (lambda (a) (do-scope-traverse a binders))]
	      [rec-traverse-with-scope-binders 
	       (lambda (a new-scope-binders) 
		 (do-scope-traverse a 
				    (add-binders-to-scope binders new-scope-binders)))])


	 (cond

	  [(zodiac:if-form? ast)

	   ((generic-if-form-action zactor) ast binders)

	   (rec-traverse (zodiac:if-form-test ast))
	   (rec-traverse (zodiac:if-form-then ast))
	   (rec-traverse (zodiac:if-form-else ast))]
	  
	  [(zodiac:set!-form? ast)

	   ((generic-set!-form-action zactor) ast binders)

	   (rec-traverse (zodiac:set!-form-var ast))
	   (rec-traverse (zodiac:set!-form-val ast))]
	  
	  [(zodiac:define-values-form? ast)

	   ((generic-define-values-form-action zactor) ast binders)

	   (let* ([the-vars (zodiac:define-values-form-vars ast)]
		  [the-val (zodiac:define-values-form-val ast)])

	     (for-each rec-traverse the-vars)

	     ; if the-vars are lexical, they're already in scope
	     ; from the enclosing unit or class

	     (rec-traverse the-val))]
	  
	  [(zodiac:let-values-form? ast)

	   ((generic-let-values-form-action zactor) ast binders)

	   (let* ([binderss (zodiac:let-values-form-vars ast)])

	     (for-each 
	      (lambda (binders)
		(for-each rec-traverse binders))
	      binderss)

	     (for-each rec-traverse (zodiac:let-values-form-vals ast))

	     (rec-traverse-with-scope-binders
	      (zodiac:let-values-form-body ast)
	      (apply append binderss)))]
	  
	  [(zodiac:letrec*-values-form? ast)

	   ((generic-letrec*-values-form-action zactor) ast binders)

	   (let* ([binderss (zodiac:letrec*-values-form-vars ast)]
		  [flat-binders (apply append binderss)]
		  [new-binders (add-binders-to-scope binders flat-binders)])

	     ; the-vars in scope at all binders and vals

	     (for-each 
	      (lambda (binders)
		(for-each 
		 (lambda (binder) 
		   (do-scope-traverse binder new-binders))
		 binders))
	      binderss)

	     (for-each 
	      (lambda (val) 
		(do-scope-traverse val new-binders))
	      (zodiac:letrec*-values-form-vals ast))

	     ; as well as in the body

	     (do-scope-traverse
	      (zodiac:letrec*-values-form-body ast) 
	      new-binders))]
	  
	  [(zodiac:top-level-varref/bind? ast) ; must be before varref? test

	   ((generic-top-level-varref/bind-action zactor) ast binders)]
	  
	  [(zodiac:varref? ast) 

	   ((generic-varref-action zactor) ast binders)]
	  
	  [(zodiac:binding? ast) 

	   ((generic-binding-action zactor) ast binders)]
	  
	  [(zodiac:quote-form? ast) 

	   ((generic-quote-form-action zactor) ast binders)]
	  
	  [(zodiac:case-lambda-form? ast) 

	   ((generic-case-lambda-form-action zactor) ast binders)

	   (let* ([raw-args (zodiac:case-lambda-form-args ast)]

		  ; raw-args : arglist list

		  [binderss (map zodiac:arglist-vars raw-args)])

	     ; binderss : lexical-binding list list


	     (for-each
	      (lambda (bs)
		(for-each
		 rec-traverse
		 bs))
	      binderss)

	     (for-each 
	      rec-traverse-with-scope-binders
	      (zodiac:case-lambda-form-bodies ast)
	      binderss))]

	  [(zodiac:app? ast)

	   ((generic-app-action zactor) ast binders)

	   (rec-traverse (zodiac:app-fun ast))
	   (for-each rec-traverse (zodiac:app-args ast))]

	  [(zodiac:begin-form? ast)

	   ((generic-begin-form-action zactor) ast binders)

	   (for-each rec-traverse (zodiac:begin-form-bodies ast))]
	  
	  [(zodiac:begin0-form? ast)

	   ((generic-begin0-form-action zactor) ast binders)

	   (for-each rec-traverse (zodiac:begin0-form-bodies ast))]
	  
	  [(zodiac:struct-form? ast) 

	   ((generic-struct-form-action zactor) ast binders)]

	  [(zodiac:interface-form? ast) 

	   ((generic-interface-form-action zactor) ast binders)

	   (for-each rec-traverse (zodiac:interface-form-super-exprs ast))]

	  [(zodiac:class*/names-form? ast) 

	   ((generic-class*/names-form-action zactor) ast binders)

	   (let* ([this (zodiac:class*/names-form-this ast)]
		  [super-init (zodiac:class*/names-form-super-init ast)]
		  [super-expr (zodiac:class*/names-form-super-expr ast)]
		  [interfaces (zodiac:class*/names-form-interfaces ast)]
		  [init-vars (zodiac:class*/names-form-init-vars ast)]
		  [init-vars-vars (zodiac:paroptarglist-vars init-vars)]
		  [init-vars-binders 
		   (map (lambda (t) (if (pair? t) (car t) t)) init-vars-vars)]
		  [inst-clauses (zodiac:class*/names-form-inst-clauses ast)]
		  [class-code (get-annotation ast)]
		  [flat-binders (list* this super-init
				       (append 
					init-vars-binders 
					(class-code-public-lookup-bindings class-code)
					(class-code-public-define-bindings class-code)
					(class-code-override-lookup-bindings class-code)
					(class-code-override-define-bindings class-code)
					(class-code-private-bindings class-code)
					(class-code-inherit-bindings class-code)
					(class-code-rename-bindings class-code)))]
		  [new-binders (add-binders-to-scope binders flat-binders)]
		  [class-traverse 
		   (lambda (t)
		     (do-scope-traverse
		      t 
		      new-binders))])

	     (rec-traverse this)
	     (rec-traverse super-init)
	     (rec-traverse super-expr)

	     (for-each 
	      rec-traverse
	      interfaces)

	     (for-each
	      (lambda (v)
		(if (pair? v) 

		    ; v = binder . exp

		    (for-each class-traverse v)

		    ; v = binder 

		    (class-traverse v)))

	      init-vars-vars)

	     (for-each
	      class-traverse
	      inst-clauses))]

	  ; object clauses

	  [(zodiac:public-clause? ast)
	   (let ([exprs (zodiac:public-clause-exprs ast)]
		 [internals (zodiac:public-clause-internals ast)])

	     ((generic-public-clause-action zactor) ast binders)
	     
	     (for-each rec-traverse internals)
	     (for-each rec-traverse exprs))]
	  
	  [(zodiac:override-clause? ast)
	   (let ([exprs (zodiac:override-clause-exprs ast)]
		 [internals (zodiac:override-clause-internals ast)])

	     ((generic-override-clause-action zactor) ast binders)
	     
	     (for-each rec-traverse internals)
	     (for-each rec-traverse exprs))]
	  
	  [(zodiac:private-clause? ast)

	   (let ([exprs (zodiac:private-clause-exprs ast)]
		 [internals (zodiac:private-clause-internals ast)])

	     ((generic-private-clause-action zactor) ast binders)
	     
	     (for-each rec-traverse internals)
	     (for-each rec-traverse exprs))]
	  
	  [(zodiac:inherit-clause? ast)
	   (let ([internals (zodiac:inherit-clause-internals ast)])

	     ((generic-inherit-clause-action zactor) ast binders)
	     
	     (for-each rec-traverse internals))]
	  
	  [(zodiac:rename-clause? ast)
	   (let ([internals (zodiac:rename-clause-internals ast)])

	     ((generic-rename-clause-action zactor) ast binders)
	     
	     (for-each rec-traverse internals))]

	  [(zodiac:sequence-clause? ast)

	   ((generic-sequence-clause-action zactor) ast binders)
	   
	   (for-each rec-traverse (zodiac:sequence-clause-exprs ast))]

	  ; units

	  [(zodiac:unit-form? ast)

	   (let* ([imports (zodiac:unit-form-imports ast)]
		  [clauses (zodiac:unit-form-clauses ast)]
		  [unit-code (get-annotation ast)]
		  [defines (unit-code-defines unit-code)]
		  [clause-binders (append defines imports)]
		  [new-binders (add-binders-to-scope binders clause-binders)]
		  [unit-traverse
		   (lambda (t)
		     (do-scope-traverse
		      t 
		      new-binders))])

	     ((generic-unit-form-action zactor) ast binders)

	     (for-each unit-traverse defines)
	     (for-each unit-traverse imports)
	     (for-each unit-traverse clauses))]

	  [(zodiac:compound-unit-form? ast)

	   (let* ([imports (zodiac:compound-unit-form-imports ast)]
		  [links (zodiac:compound-unit-form-links ast)]
		  [unit-args (filter zodiac:lexical-varref? (map cddr links))]
		  [linked-units (map cadr links)]
		  [compound-unit-traverse
		   (lambda (t)
		     (rec-traverse-with-scope-binders t imports))])

	     ((generic-compound-unit-form-action zactor) ast binders)

	     (for-each rec-traverse imports)
	     (for-each compound-unit-traverse unit-args)
	     (for-each compound-unit-traverse linked-units))]

	  [(zodiac:invoke-unit-form? ast)

	   (let ([unit (zodiac:invoke-unit-form-unit ast)]
		 [variables (zodiac:invoke-unit-form-variables ast)])

	     ((generic-invoke-unit-form-action zactor) ast binders)

	     (rec-traverse unit)
	     (for-each rec-traverse variables))]

	  [(zodiac:invoke-open-unit-form? ast)

	   (let ([unit (zodiac:invoke-open-unit-form-unit ast)]
		 [variables (zodiac:invoke-open-unit-form-variables ast)])

	     ((generic-invoke-open-unit-form-action zactor) ast binders)

	     (rec-traverse unit)
	     (for-each rec-traverse variables))])))))

 ; zolder% is the class of objects that knows how to combine information
 ; at different kinds of Zodiac nodes

 (define zolder%
   (class object% ()
	  (public 
	   [default-folder (lambda (ast . results-from-subterms) 
			     (compiler:internal-error 'zolder%:default-folder
						      "Used virtual base class method on ~a~n" 
						      (zodiac:parsed->raw ast)))]
	   ; core Scheme

	   [if-form-folder default-folder]
	   [set!-form-folder default-folder]
	   [define-values-form-folder default-folder]
	   [let-values-form-folder default-folder]
	   [letrec*-values-form-folder default-folder]
	   [top-level-varref/bind-folder default-folder]
	   [varref-folder default-folder]
	   [binding-folder default-folder]
	   [quote-form-folder default-folder]
	   [case-lambda-form-folder default-folder]
	   [app-folder default-folder]
	   [begin-form-folder default-folder]
	   [begin0-form-folder default-folder]
	   [struct-form-folder default-folder]

	   ; objects

	   [interface-form-folder default-folder]
	   [class*/names-form-folder default-folder]

	   ; object clauses

	   [public-clause-folder default-folder]
	   [override-clause-folder default-folder]
	   [private-clause-folder default-folder]
	   [inherit-clause-folder default-folder]
	   [rename-clause-folder default-folder]
	   [sequence-clause-folder default-folder]

	   ; units

	   [unit-form-folder default-folder]
	   [compound-unit-form-folder default-folder]
	   [invoke-unit-form-folder default-folder]
	   [invoke-open-unit-form-folder default-folder])))

 (define generic-default-folder (make-generic zolder% default-folder))
 (define generic-if-form-folder (make-generic zolder% if-form-folder))
 (define generic-set!-form-folder (make-generic zolder% set!-form-folder))
 (define generic-define-values-form-folder (make-generic zolder% define-values-form-folder))
 (define generic-let-values-form-folder (make-generic zolder% let-values-form-folder))
 (define generic-letrec*-values-form-folder (make-generic zolder% letrec*-values-form-folder))
 (define generic-top-level-varref/bind-folder (make-generic zolder% top-level-varref/bind-folder))
 (define generic-varref-folder (make-generic zolder% varref-folder))
 (define generic-binding-folder (make-generic zolder% binding-folder))
 (define generic-quote-form-folder (make-generic zolder% quote-form-folder))
 (define generic-case-lambda-form-folder (make-generic zolder% case-lambda-form-folder))
 (define generic-app-folder (make-generic zolder% app-folder))
 (define generic-begin-form-folder (make-generic zolder% begin-form-folder))
 (define generic-begin0-form-folder (make-generic zolder% begin0-form-folder))
 (define generic-struct-form-folder (make-generic zolder% struct-form-folder))
 (define generic-interface-form-folder (make-generic zolder% interface-form-folder))
 (define generic-class*/names-form-folder (make-generic zolder% class*/names-form-folder))
 (define generic-public-clause-folder (make-generic zolder% public-clause-folder))
 (define generic-override-clause-folder (make-generic zolder% override-clause-folder))
 (define generic-private-clause-folder (make-generic zolder% private-clause-folder))
 (define generic-inherit-clause-folder (make-generic zolder% inherit-clause-folder))
 (define generic-rename-clause-folder (make-generic zolder% rename-clause-folder))
 (define generic-sequence-clause-folder (make-generic zolder% sequence-clause-folder))
 (define generic-unit-form-folder (make-generic zolder% unit-form-folder))
 (define generic-compound-unit-form-folder (make-generic zolder% compound-unit-form-folder))
 (define generic-invoke-unit-form-folder (make-generic zolder% invoke-unit-form-folder))
 (define generic-invoke-open-unit-form-folder (make-generic zolder% invoke-open-unit-form-folder))

 (define (traverse-ast-with-zolder ast zolder)

   (let do-zolder-traverse
       ([ast ast])

     (cond

      [(zodiac:if-form? ast)

       ((generic-if-form-folder zolder)
	ast
	(do-zolder-traverse (zodiac:if-form-test ast))
	(do-zolder-traverse (zodiac:if-form-then ast))
	(do-zolder-traverse (zodiac:if-form-else ast)))]
      
      [(zodiac:set!-form? ast)

       ((generic-set!-form-folder zolder)
	ast
	(do-zolder-traverse (zodiac:set!-form-var ast))
	(do-zolder-traverse (zodiac:set!-form-val ast)))]
      
      [(zodiac:define-values-form? ast)

       (let* ([the-vars (zodiac:define-values-form-vars ast)]
	      [the-val (zodiac:define-values-form-val ast)])

	 ((generic-define-values-form-folder zolder)
	  ast
	  (map do-zolder-traverse the-vars)
	  (do-zolder-traverse the-val)))]
      
      [(zodiac:let-values-form? ast)

       ((generic-let-values-form-folder zolder)
	ast 
	(map (lambda (binders) 
	       (map do-zolder-traverse binders))
	     (zodiac:let-values-form-vars ast))
	(map do-zolder-traverse (zodiac:let-values-form-vals ast))
	(do-zolder-traverse (zodiac:let-values-form-body ast)))]

      [(zodiac:letrec*-values-form? ast)

       ((generic-letrec*-values-form-folder zolder)
	ast 
	(map (lambda (binders) 
	       (map do-zolder-traverse binders))
	     (zodiac:letrec*-values-form-vars ast))
	(map do-zolder-traverse (zodiac:letrec*-values-form-vals ast))
	(do-zolder-traverse (zodiac:letrec*-values-form-body ast)))]
      
      [(zodiac:top-level-varref/bind? ast) ; must be before varref? test

       ((generic-top-level-varref/bind-folder zolder) ast)]
      
      [(zodiac:varref? ast) 

       ((generic-varref-folder zolder) ast)]

      [(zodiac:binding? ast) 

       ((generic-binding-folder zolder) ast)]
      
      [(zodiac:quote-form? ast) 

       ((generic-quote-form-folder zolder) ast)]
      
      [(zodiac:case-lambda-form? ast) 

       (let* ([binderss (map zodiac:arglist-vars (zodiac:case-lambda-form-args ast))])

	 ; binderss : lexical-binding list list

	 ((generic-case-lambda-form-folder zolder)
	  ast
	  (map 
	   (lambda (binders) 
	     (map do-zolder-traverse binders))
	   binderss)
	  (map do-zolder-traverse (zodiac:case-lambda-form-bodies ast))))]

      [(zodiac:app? ast)

       ((generic-app-folder zolder)
	ast
	(do-zolder-traverse (zodiac:app-fun ast))
	(map do-zolder-traverse (zodiac:app-args ast)))]

      [(zodiac:begin-form? ast)

       ((generic-begin-form-folder zolder)
	ast
	(map do-zolder-traverse (zodiac:begin-form-bodies ast)))]
      
      [(zodiac:begin0-form? ast)

       ((generic-begin-form-folder zolder)
	ast
	(map do-zolder-traverse (zodiac:begin0-form-bodies ast)))]
      
      [(zodiac:struct-form? ast) 

       ((generic-struct-form-folder zolder) ast)]

      [(zodiac:interface-form? ast) 
       
       ((generic-interface-form-folder zolder)
	ast 
	(map do-zolder-traverse (zodiac:interface-form-super-exprs ast)))]

      [(zodiac:class*/names-form? ast) 

       (let* ([this (zodiac:class*/names-form-this ast)]
	      [super-init (zodiac:class*/names-form-super-init ast)]
	      [super-expr (zodiac:class*/names-form-super-expr ast)]
	      [interfaces (zodiac:class*/names-form-interfaces ast)]
	      [init-vars (zodiac:class*/names-form-init-vars ast)]
	      [init-vars-vars (zodiac:paroptarglist-vars init-vars)]
	      [init-binders 
	       (map car (filter pair? init-vars-vars))]
	      [inst-clauses (zodiac:class*/names-form-inst-clauses ast)])

	 ((generic-class*/names-form-folder zolder)
	  ast 
	  (do-zolder-traverse this)
	  (do-zolder-traverse super-init)
	  (do-zolder-traverse super-expr)
	  (map do-zolder-traverse interfaces)
	  (map
	   (lambda (v)
	     (if (pair? v) 

		 ; v = binder . exp

		 (map do-zolder-traverse v)

		 ; v = binder 

		 (do-zolder-traverse v)))

	   init-vars-vars)

	  (map do-zolder-traverse inst-clauses)))]

      [(zodiac:public-clause? ast)

       (let ([exprs (zodiac:public-clause-exprs ast)]
	     [internals (zodiac:public-clause-internals ast)])

	 ((generic-public-clause-folder zolder)
	  ast
	  (map do-zolder-traverse internals)
	  (map do-zolder-traverse exprs)))]

      [(zodiac:override-clause? ast)

       (let ([exprs (zodiac:override-clause-exprs ast)]
	     [internals (zodiac:override-clause-internals ast)])

	 ((generic-override-clause-folder zolder)
	  ast
	  (map do-zolder-traverse internals)
	  (map do-zolder-traverse exprs)))]
      
      [(zodiac:private-clause? ast)

       (let ([exprs (zodiac:private-clause-exprs ast)]
	     [internals (zodiac:private-clause-internals ast)])

	 ((generic-private-clause-folder zolder)
	  ast
	  (map do-zolder-traverse internals)
	  (map do-zolder-traverse exprs)))]
      
      [(zodiac:inherit-clause? ast)
       (let ([internals (zodiac:inherit-clause-internals ast)])

	 ((generic-inherit-clause-folder zolder)
	  ast
	  (map do-zolder-traverse internals)))]

      [(zodiac:rename-clause? ast)
       (let ([internals (zodiac:rename-clause-internals ast)])

	 ((generic-rename-clause-folder zolder)
	  ast
	  (map do-zolder-traverse internals)))]
      
      [(zodiac:sequence-clause? ast)

       ((generic-sequence-clause-folder zolder)
	ast
	(map do-zolder-traverse (zodiac:sequence-clause-exprs ast)))]

      ; units

      [(zodiac:unit-form? ast)

       ((generic-unit-form-folder zolder)
	ast
	(map do-zolder-traverse (zodiac:unit-form-imports ast))
	(map do-zolder-traverse (unit-code-defines (get-annotation ast)))
	(map do-zolder-traverse (zodiac:unit-form-clauses ast)))]

      [(zodiac:compound-unit-form? ast)

       (let* ([imports (zodiac:compound-unit-form-imports ast)]
	      [links (zodiac:compound-unit-form-links ast)]
	      [unit-args (filter zodiac:lexical-varref? (map cddr links))]
	      [linked-units (map cadr links)])

	 ((generic-compound-unit-form-folder zolder)
	  ast
	  (map do-zolder-traverse imports)
	  (map do-zolder-traverse unit-args)
	  (map do-zolder-traverse linked-units)))]

      [(zodiac:invoke-unit-form? ast)

       (let ([unit (zodiac:invoke-unit-form-unit ast)]
	     [variables (zodiac:invoke-unit-form-variables ast)])

	 ((generic-invoke-unit-form-folder zolder)
	  ast
	  (do-zolder-traverse unit)
	  (map do-zolder-traverse variables)))]

      [(zodiac:invoke-open-unit-form? ast)

       (let ([unit (zodiac:invoke-open-unit-form-unit ast)]
	     [variables (zodiac:invoke-open-unit-form-variables ast)])

	 ((generic-invoke-open-unit-form-folder zolder)
	  ast
	  (do-zolder-traverse unit)
	  (map do-zolder-traverse variables)))])))

 (define var-zolder% ; abstract class for collecting variable information
   (class zolder% ()

	  (public

	   [binder-set-minus 
	    (lambda (fvs bs)
	      (set-minus
	       fvs 
	       (set-filter zodiac:lexical-binding? bs)))])

	  (override

	   [if-form-folder
	    (lambda (_ test-fv then-fv else-fv)
	      (fold-sets (list test-fv then-fv else-fv)))]

	   [set!-form-folder 
	    (lambda (_ lhs-fvs rhs-fvs)
	      (fold-sets (list lhs-fvs rhs-fvs)))]

	   [define-values-form-folder 
	     (lambda (a _ rhs-fv)
	       (binder-set-minus 
		rhs-fv 
		(list->set (zodiac:define-values-form-vars a))))]
	   
	   [let-values-form-folder 
	    (lambda (a _ vals-fvs body-fv)
	      
	      ; binders in vals-fvs are free in the let

	      (set-union (fold-sets vals-fvs)
			 (binder-set-minus body-fv 
					   (fold-sets
					    (map list->set 
						 (zodiac:let-values-form-vars a))))))]


	   [letrec*-values-form-folder
	    (lambda (a _ vals-fvs body-fv)

	      ; binders in vals-fvs are free in the letrec*, unless
	      ;   they are among the vars

	      (binder-set-minus (set-union body-fv (fold-sets vals-fvs))
				(fold-sets
				 (map list->set (zodiac:letrec*-values-form-vars a)))))]


	   [top-level-varref/bind-folder 
	    (lambda (_) empty-set)]

	   [varref-folder 
	    (lambda (_) empty-set)]

	   [binding-folder 
	    (lambda (_) empty-set)]

	   [quote-form-folder 
	    (lambda (_) empty-set)]

	   [app-folder 
	    (lambda (_ rator-fv rands-fvs)
	      (fold-sets (cons rator-fv rands-fvs)))]

	   [begin-form-folder 
	    (lambda (_ bodies-fvs)
	      (fold-sets bodies-fvs))]

	   [begin0-form-folder 
	    (lambda (_ bodies-fvs)
	      (fold-sets bodies-fvs))]
	   
	   [struct-form-folder 
	    (lambda (_) empty-set)]

	   [class*/names-form-folder
	    (lambda (a this-fvs super-init-fvs super-expr-fvs 
		       interface-fvss init-vars-fvss inst-clause-pairs)
	      (let* ([interface-fvs (fold-sets interface-fvss)]

		     ; each elt in init-vars-fvss is either
		     ;
		     ;    a singleton set containing a binder, or
		     ;
		     ;    a pair whose car is such a singleton set, and
		     ;    whose cdr is the set of free vars in an initialization expression

		     [init-vars-fvs 
		      (fold-sets (map cdr (filter pair? init-vars-fvss)))]
		     [inst-clause-varrefs
		      (fold-sets (map cdr inst-clause-pairs))]
		     [class-code (get-annotation a)]
		     [binders (set-union (set-union this-fvs super-init-fvs)
					 (list->set
					  (append
					   (class-code-public-lookup-bindings class-code)
					   (class-code-public-define-bindings class-code)
					   (class-code-override-lookup-bindings class-code)
					   (class-code-override-define-bindings class-code)
					   (class-code-private-bindings class-code)
					   (class-code-inherit-bindings class-code)
					   (class-code-rename-bindings class-code))))])

		(binder-set-minus
		 (fold-sets 
		  (list super-expr-fvs interface-fvs init-vars-fvs inst-clause-varrefs))
		 binders)))]
	   
	   [interface-form-folder
	    (lambda (_ super-exprs-fvs)
	      (fold-sets super-exprs-fvs))]

	   ; for each object clauses, return a pair where
	   ;     the car is a list of binders
	   ;     the cdr is a list of varrefs
	   ; whether vars are free or not is not determinable locally
	   ; we make that calculation in the containing class*/name

	   [public-clause-folder
	    (lambda (_ internal-fvs exprs-fvs)
	      (cons 
	       (fold-sets internal-fvs)
	       (fold-sets exprs-fvs)))]

	   [override-clause-folder
	    (lambda (_ internal-fvs exprs-fvs)
	      (cons 
	       (fold-sets internal-fvs)
	       (fold-sets exprs-fvs)))]

	   [private-clause-folder
	    (lambda (_ internal-fvs exprs-fvs)
	      (cons
	       (fold-sets internal-fvs)
	       (fold-sets exprs-fvs)))]

	   [inherit-clause-folder
	    (lambda (_ internal-fvs)
	      (cons (fold-sets internal-fvs)
		    empty-set))]

	   [rename-clause-folder
	    (lambda (_ internal-fvs)
	      (cons (fold-sets internal-fvs)
		    empty-set))]

	   [sequence-clause-folder
	    (lambda (_ exprs-fvs)
	      (cons empty-set
		    (fold-sets exprs-fvs)))]

	   ; units

	   [unit-form-folder
	    (lambda (a imports-fv defines-fv clauses-fv)
	      (let* ([unit-code (get-annotation a)]
		     [binders ; lexical-binding set list
		      (cons (list->set (unit-code-defines unit-code))
			    imports-fv)])
		(binder-set-minus
		 (fold-sets clauses-fv)
		 (fold-sets binders))))]

	   [compound-unit-form-folder
	    (lambda (_ __ ___ linked-units-fvs)
	      (fold-sets linked-units-fvs))]

	   [invoke-unit-form-folder
	    (lambda (_ unit-fv variables-fvs)
	      (fold-sets (cons unit-fv variables-fvs)))]

	   [invoke-open-unit-form-folder
	    (lambda (_ unit-fv variables-fvs)
	      (fold-sets (cons unit-fv variables-fvs)))])

	  (sequence (super-init))))

 (define fv-zolder%
   (class var-zolder% ()
	  (inherit binder-set-minus)
	  (override

	   [varref-folder 
	    (lambda (a) 
	      (if (zodiac:bound-varref? a)
		  (let ([b (zodiac:bound-varref-binding a)])
		    (if b
			(make-singleton-set b)
			empty-set))
		  empty-set))]

	   [binding-folder 
	    (lambda (a) (make-singleton-set a))]
	   
	   [case-lambda-form-folder 
	    (lambda (a _ bodies-fvs)
	      (let* ([argss           ; lexical-binding list list
		      (map zodiac:arglist-vars 
			   (zodiac:case-lambda-form-args a))]
		     [arg-setss       ; lexical-binding set list
		      (map list->set argss)]
		     [cases-fvs 
		      (map binder-set-minus
			   bodies-fvs arg-setss)])

		(fold-sets cases-fvs)))])

	  (sequence (super-init))))

 (define (make-global-tables-and-set-scopes ast)
   (let* ([add-entries-zactor 
	   (make-object
	    (class zactor% ()
		   (override
		    [default-action
		      (lambda (a vs)
			(when (zodiac:parsed? a)
			      (set-scope-binders! a vs)))]
		    [case-lambda-form-action
		     (lambda (a vs)  
		       (default-action a vs)
		       (set-phi! a (make-singleton-set a)))]
		    [top-level-varref/bind-action
		     (lambda (a vs)  ; pick a representative
		       (set-scope-binders! a vs)
		       (let ([var (zodiac:varref-var a)])
			 (unless (top-var-lookup var)
				 (hash-table-put! *top-level-var-table* var a))))])
		   (sequence (super-init))))])
     
     (traverse-ast-with-scope-zactor ast add-entries-zactor)))

 ;; Put varref's phi in the binding.
 (define (get-phi a)
   (if (zodiac:bound-varref? a)
       (get-phi (zodiac:bound-varref-binding a))
       (phi a)))

 (define (closure-analyze asts)
   (let* ([inner-done #f]
	  [outer-done #f]
	  [prop-phi!
	   (lambda (t1 t2)
	     (let ([t1-phi (get-phi t1)]
		   [t2-phi (get-phi t2)])
	       (unless (set-subset? t1-phi t2-phi)
		 (let ([dest (if (zodiac:bound-varref? t2)
				 (zodiac:bound-varref-binding t2)
				 t2)])
		   (set-phi! dest (set-union t1-phi t2-phi))
		   (set! inner-done #f)))))]
	  [do-set-zactor 
	   (make-object
	    (class zactor% ()

		   (override

		    [if-form-action
		     (lambda (a)
		       (prop-phi! (zodiac:if-form-then a) a)
		       (prop-phi! (zodiac:if-form-else a) a))]

		    [set!-form-action
		     (lambda (a)
		       (prop-phi! (zodiac:set!-form-val a)
				  (zodiac:set!-form-var a)))]

		    [define-values-form-action
		      (lambda (a)
			(let ([the-val (zodiac:define-values-form-val a)]
			      [the-vars (zodiac:define-values-form-vars a)])
			  (for-each 
			   (lambda (v) (prop-phi! the-val v))
			   the-vars)))]

		    [let-values-form-action
		     (lambda (a)
		       (let* ([varss (zodiac:let-values-form-vars a)]
			      [vals (zodiac:let-values-form-vals a)]
			      [body (zodiac:let-values-form-body a)])

			 (for-each
			  (lambda (val vars)
			    (for-each
			     (lambda (var)
			       (prop-phi! val var))
			     vars))
			  vals
			  varss)

			 (prop-phi! body a)))]

		    [letrec*-values-form-action
		     (lambda (a)
		       (let* ([varss (zodiac:letrec*-values-form-vars a)]
			      [vals (zodiac:letrec*-values-form-vals a)]
			      [body (zodiac:letrec*-values-form-body a)])

			 (for-each
			  (lambda (val vars)
			    (for-each
			     (lambda (var)
			       (prop-phi! val var))
			     vars))
			  vals
			  varss)

			 (prop-phi! body a)))]

		    [top-level-varref/bind-action
		     (lambda (a) 
		       (let ([rep (top-var-lookup (zodiac:varref-var a))])
			 (prop-phi! a rep)
			 (prop-phi! rep a)))]
		    
		    ; varref-action is default

		    ; binding-action is default

		    ; quote-form-action is default

		    ; case-lambda-form-action is default

		    [app-action
		     (lambda (a)
		       (let* ([fun (zodiac:app-fun a)]
			      [fun-phi (get-phi fun)]
			      [rands (zodiac:app-args a)]
			      [rands-len (length rands)])
			 
			 (for-each

			  (lambda (f) ; f is a case-lambda

			    (let* ([arglists (zodiac:case-lambda-form-args f)]

				   ; arglists : arglist list

				   [bodies (zodiac:case-lambda-form-bodies f)])


			      (for-each
			       
			       (lambda (arglist body)
				 
				 (let* ([arglist-vars (zodiac:arglist-vars arglist)])

				   (cond

				    [(and (zodiac:list-arglist? arglist)
					  (= (length arglist-vars) rands-len))

				     ; prop info from args to binders

				     (for-each
				      (lambda (rand arg)
					(prop-phi! rand arg))
				      rands
				      arglist-vars)]
				    
				    [(or (zodiac:sym-arglist? arglist)
					 (zodiac:ilist-arglist? arglist))

				     ; unesthetic hack

				     (set-unknown! f #t)]) 

				   ; prop info from lambda bodies to app

				   (prop-phi! body a)))

			       arglists bodies)))

			  (set->list fun-phi))))]

		    [begin-form-action
		     (lambda (a)
		       (prop-phi!
			(car (last-pair (zodiac:begin-form-bodies a)))
			a))]

		    [begin0-form-action
		     (lambda (a)
		       (prop-phi!
			(car (zodiac:begin0-form-bodies a))
			a))])

		   ; everything else is default

		   (sequence (super-init))))])

     (let outer-closure-loop ()
       (unless outer-done
	       (set! outer-done #t)
	       (for-each 
		(lambda (ast)
		  (let inner-closure-loop ()
		    (set! inner-done #t)
		    (traverse-ast-with-zactor ast do-set-zactor)
		    (unless inner-done
		      (set! outer-done #f)
		      (inner-closure-loop))))
		asts)
	       (outer-closure-loop)))))

 (define (escape-analyze ast)
   (letrec ([done #f]
	    [unit-init #f]
	    [set-unknown-and-set-flag! 
	     (lambda (a)
	       (unless (unknown? a)
		       (set-unknown! a #t)
		       (set! done #f)))]
	    [set-escape-and-set-flag! 
	     (lambda (a)
	       (unless (escape? a)
		       (set-escape! a #t)
		       (set! done #f)))]
	    [pending-phi-closures '()]
	    [compute-phi-closure-as-list!
	     (lambda (a)
	       (let* ([closed #f]
		      [result (set->list (get-phi a))])
		 (let phi-closure-loop ([new-ones result])
		   (let ([body-lambdas
			  (set->list
			   (fold-sets
			    (apply append 
				   (map (lambda (bodies) 
					  (map 
					   (lambda (body)
					     (cond
					      [(phi-closure body)
					       (phi-closure body)]
					      [(memq body pending-phi-closures)
					       (phi body)]
					      [else 
					       (set! pending-phi-closures
						     (cons body pending-phi-closures))
					       (compute-phi-closure-as-list! body)
					       (phi-closure body)]))
					   bodies))
					(map zodiac:case-lambda-form-bodies 
					     new-ones)))))]

			 [new-this-iter '()])
		     
		     (set! closed #t)

		     (for-each
		      (lambda (bl)
			(when (not (memq bl result))
			      (set! closed #f)
			      (set! new-this-iter (cons bl new-this-iter))))
		      body-lambdas)
		     
		     (set! result (append new-this-iter result))

		     (if closed

			 (begin 
			   (set-phi-closure! a (list->set result))
			   result)

			 (phi-closure-loop new-this-iter))))))]
	    [mark-as-escaping!
	     (lambda (a)
	       (let ([arglists (zodiac:case-lambda-form-args a)])
		 (set-escape-and-set-flag! a)
		 (for-each
		  (lambda (arglist)
		    (for-each
		     set-unknown-and-set-flag! 
		     (zodiac:arglist-vars arglist)))
		  arglists)))]
	    [init-unknown!
	     (lambda (term)
	       (unless (unknown? term)
		       (set-unknown! term #t)
		       (set! done #f)))]
	    [prop-unknown 
	     (lambda (from to)
	       (let ([unknown-from (unknown? from)]
		     [unknown-to (unknown? to)])
		 (when (and unknown-from
			    (not unknown-to))
		       (set-unknown-and-set-flag! to))))]
	    [escape-zolder
	     (make-object
	      (class zolder% ()
		     (override
		      [default-folder void]
		      [if-form-folder
		       (lambda (a _ __ ___)
			 (prop-unknown (zodiac:if-form-then a) a)
			 (prop-unknown (zodiac:if-form-else a) a))]
		      [set!-form-folder
		       (lambda (a _ __)
			 (prop-unknown (zodiac:set!-form-val a) 
				       (zodiac:set!-form-var a)))]
		      [define-values-form-folder
			(lambda (a _ __)
			  (let ([val (zodiac:define-values-form-val a)]) 
			    (for-each
			     (lambda (var)
			       (prop-unknown val var))
			     (zodiac:define-values-form-vars a))))]
		      [let-values-form-folder
		       (lambda (a _ __ ___)
			 (let ([binderss (zodiac:let-values-form-vars a)]
			       [vals (zodiac:let-values-form-vals a)]) 
			   (for-each
			    (lambda (val)
			      (for-each
			       (lambda (binders)
				 (for-each 
				  (lambda (b)
				    (prop-unknown val b))
				  binders))
			       binderss))
			    vals)
			   (prop-unknown (zodiac:let-values-form-body a)
					 a)))]
		      [letrec*-values-form-folder
		       (lambda (a _ __ ___)
			 (let ([binderss (zodiac:letrec*-values-form-vars a)]
			       [vals (zodiac:letrec*-values-form-vals a)]) 
			   (for-each
			    (lambda (val)
			      (for-each
			       (lambda (binders)
				 (for-each
				  (lambda (b)
				    (prop-unknown val b))
				  binders))
			       binderss))
			    vals)
			   (prop-unknown (zodiac:letrec*-values-form-body a)
					 a)))]
		      [top-level-varref/bind-folder
		       (lambda (a)
			 (when (varref:has-attribute? a varref:primitive)
			       (set-unknown-and-set-flag! a)))]
		      [varref-folder 
		       (lambda (a)
			 (when (zodiac:bound-varref? a)
			       (let ([binder (zodiac:bound-varref-binding a)])
				 (when binder 
				       (prop-unknown binder a)
				       (when (escape? binder)
					     (for-each
					      mark-as-escaping! 
					      (set->list (get-phi a))))))))]
		      [case-lambda-form-folder
		       (lambda (a _ __)
			 (when (escape? a)
			       (for-each
				(lambda (body)
				  (unless (phi-closure body)
					  (let ([phi-cs (compute-phi-closure-as-list! body)])
					    (for-each
					     mark-as-escaping!
					     phi-cs))))
				(zodiac:case-lambda-form-bodies a))))]
		      [app-folder
		       (lambda (a _ __)
			 (let ([rator (zodiac:app-fun a)]
			       [rands (zodiac:app-args a)])

			   ; anything passed to unknown rator is escaping

			   (when (unknown? rator)
				 (for-each
				  (lambda (rand)
				    (for-each 
				     mark-as-escaping! 
				     (set->list (get-phi rand))))
				  rands))

			   ; if the rator is unknown, so is the app

			   (prop-unknown rator a)

			   ; if any function returns an unknown,
			   ; the app is unknown

			   (for-each
			    (lambda (bodies)
			      (for-each
			       (lambda (body)
				 (prop-unknown body a))
			       bodies))
			    (map zodiac:case-lambda-form-bodies 
				 (set->list (get-phi rator))))))]
		      [begin0-form-folder
		       (lambda (a _)
			 (prop-unknown (car (zodiac:begin0-form-bodies a))
				       a))]
		      [begin-form-folder
		       (lambda (a _)
			 (prop-unknown (car (last-pair 
					     (zodiac:begin-form-bodies a)))
				       a))]

		      [unit-form-folder
		       (lambda (a _ __ ___)
			 (unless unit-init
				 (let* ([imports (zodiac:unit-form-imports a)]
					[unit-code (get-annotation a)]
					[export-vs (unit-code-exports unit-code)])
				   (for-each
				    init-unknown!
				    imports)
				   (for-each
				    set-escape-and-set-flag!
				    export-vs))
				 (set! unit-init #t)))]
		      [compound-unit-form-folder
		       (lambda (a . args)
			 (let* ([imports (zodiac:compound-unit-form-imports a)])
			   (for-each
			    init-unknown!
			    imports)))]

		      [invoke-unit-form-folder
		       (lambda (a . args)
			 (let ([vars (zodiac:invoke-unit-form-variables a)])
			   (for-each
			    (lambda (v)
			      (when (zodiac:bound-varref? v)
				    (let ([binder (zodiac:bound-varref-binding v)])
				      (if binder
					  (set-escape-and-set-flag! binder)
					  (set-escape-and-set-flag! v)))))
			    vars)))]
		      [invoke-open-unit-form-folder
		       (lambda (a . args)
			 (let ([vars (zodiac:invoke-open-unit-form-variables a)])
			   (for-each
			    (lambda (v)
			      (when (zodiac:bound-varref? v)
				    (let ([binder (zodiac:bound-varref-binding v)])
				      (if binder
					  (set-escape-and-set-flag! binder)
					  (set-escape-and-set-flag! v)))))
			    vars)))]
		      )		    (sequence (super-init))))])
     (let escape-loop ()
       (set! done #t)
       (traverse-ast-with-zolder ast escape-zolder)
       (unless done 
	       (escape-loop)))))

 (define (initialize-invariance-sets ast)

   (let* ([immutable-only
	   (lambda (binder-set)
	     (list->set
	      (filter 
	       (lambda (b)
		 (let ([anno (get-annotation b)])
		   (not (or
			 (binding-mutable? anno)
			 (and (binding-unit-i/e? anno)
			      (not (binding-known? anno)))))))
	       (set->list binder-set))))]
	  [init-theta-zactor
	   (make-object
	    (class zactor% ()

		   (override

		    [default-action
		      (lambda (a)
			(when (zodiac:parsed? a)
			      (unless (theta a) ; may have been already set 
				      (set-theta! a (scope-binders a)))))]

		    [varref-action
		     (lambda (a)
		       (let* ([binder (and (zodiac:bound-varref? a)
					   (zodiac:bound-varref-binding a))])

			 ; if the binder exists, snarf its theta 

			 (if (and binder (theta binder))
			     (set-theta! a (theta binder))
			     (set-theta! a (scope-binders a)))))]
		    
		    [case-lambda-form-action

		     (lambda (a)

		       (let* ([argss (zodiac:case-lambda-form-args a)]
			      
			      ; argss : arglist list
			      
			      [binderss (map zodiac:arglist-vars argss)]

			      ; binderss : lexical-binding list list

			      [code (get-annotation a)]
			      [fvs (code-free-vars code)]
			      [sbs (scope-binders a)])

			 ; rule proc-inv
			 ; also, exclude mutable variables as possible parameters

			 (if (escape? a)

			     (set-theta! a empty-set)

			     (set-theta! a 
					 (set-intersect (immutable-only sbs)
							fvs)))

			 ; handles rule app-inv-bv
			 ; do this at lambdas, so only done once
			 ; each binder is excluded from its own theta 

			 (for-each ; for each lexical-binding list
			  (lambda (binders)
			    (for-each ; for each binder
			     (lambda (b)
			       (set-theta! b sbs))
			     binders))
			  binderss)))]

		    [app-action
		     (lambda (a)
		       (let ([rator (zodiac:app-fun a)]) 
			 (default-action a)
			 (when (unknown? rator)
			       (set-theta! rator empty-set))))]

		    [class*/names-form-action

		     (lambda (a)
		       (let* ([init-vars (zodiac:class*/names-form-init-vars a)]
			      [init-vars-vars (zodiac:paroptarglist-vars init-vars)]
			      [init-vars-binders 
			       (map (lambda (v) (if (pair? v) (car v) v)) init-vars-vars)]
			      [sbs (scope-binders a)])

			 ; by analogy with letrec*

			 (set-theta! a sbs)

			 (for-each
			  (lambda (t) (set-theta! t sbs))
			  init-vars-binders)))]
		    
		    [unit-form-action
		     (lambda (a)
		       (let* ([unit-code (get-annotation a)]
			      [exports (unit-code-exports unit-code)]
			      [sbs (scope-binders a)])

			 (set-theta! a sbs)

			 ; exported procedures obey default protocol
			 
			 (for-each 
			  (lambda (v)
			    (set-theta! v empty-set))
			  exports)))]

		    [compound-unit-form-action
		     (lambda (a)
		       (let* ([imports (zodiac:compound-unit-form-imports a)]
			      [sbs (scope-binders a)])

			 (set-theta! a sbs)

			 ; like case-lambda

			 (for-each 
			  (lambda (v)
			    (set-theta! v sbs))
			  imports)))]

		    [invoke-unit-form-action
		     (lambda (a)

		       ; for simplicity, disregard functions returned by units

		       (set-theta! a empty-set))]

		    [invoke-open-unit-form-action
		     (lambda (a)
		       (set-theta! a empty-set))])

		   (sequence (super-init))))]

	  [one-time-theta-zactor 
	   (make-object
	    (class zactor% ()

		   (override

		    [app-action

		     (lambda (a)

		       (let* ([rator (zodiac:app-fun a)]
			      [rator-lambdas (set->list (get-phi rator))]
			      [rands (zodiac:app-args a)])

			 ; app-inv-bv-app

			 (for-each ; for each lambda j

			  (lambda (j) 

			    (let ([argss (zodiac:case-lambda-form-args j)])
			      
			      ; argss : arglist list
			      
			      (for-each ; for each j.bv
			       (lambda (arg)
				 (set-theta! a (set-remove arg (theta a))))
			       argss)))

			  rator-lambdas)

			 ; lambda's passed to prims use their default protocol

			 (when (unknown? rator)

			       (for-each
				(lambda (rand) 
				  (let ([rand-lambdas (set->list (get-phi rand))])
				    (for-each
				     (lambda (j) 
				       (set-theta! j empty-set))
				     rand-lambdas)))
				rands))))])

		   (sequence (super-init))))])

     (traverse-ast-with-zactor ast init-theta-zactor)
     (traverse-ast-with-zactor ast one-time-theta-zactor)))

 (define (invariance-analyze ast)

   (let* ([done #f] ; fixpoint flag

	  [deduce-theta-zactor

	   (let* ([prop-theta-bar-at-ast
		   (lambda (a some-theta)
		     (let ([curr-theta (theta a)])
		       (if curr-theta
			   (unless (set-subset? curr-theta some-theta)
				   (set-theta! 
				    a 
				    (set-intersect curr-theta some-theta))
				   (set! done #f))

			   ; curr-theta is #f for set!'s whose
			   ; lhs is lexical-binding, but whose
			   ; binder hasn't been seen yet

			   (set-theta! a some-theta))))]

		  [prop-top-level-binder-theta
		   (lambda (binder)
		     (let ([v-theta (theta binder)]
			   [rep (top-var-lookup (zodiac:varref-var binder))])

		       ; all top-level binders get same theta

		       (prop-theta-bar-at-ast rep v-theta)
		       (set-theta! binder (theta rep))))]

		  [prop-bound-varref-theta
		   (lambda (v)
		     (prop-theta-bar-at-ast (zodiac:bound-varref-binding v)
					    (theta v)))])

	     (make-object
	      (class zactor% ()
		     (override

		      [if-form-action
		       
		       (lambda (a)
			 
			 (let* ([then-part (zodiac:if-form-then a)]
				[else-part (zodiac:if-form-else a)])
			   
			   ; x \not\in theta_if.then => x \not\in theta_if
			   
			   (prop-theta-bar-at-ast a (theta then-part))
			   
			   ; x \not\in theta_if.else => x \not\in theta_if
			   
			   (prop-theta-bar-at-ast a (theta else-part))))]
		      
		      ; set! doesn't change bindings of variables to locations
		      ; nothing special to do here
		      ; not a lambda
		      
		      
		      [let-values-form-action

		       (lambda (a)

			 (let ([the-vals (zodiac:let-values-form-vals a)]
			       [raw-vars (zodiac:let-values-form-vars a)]
			       [the-body (zodiac:let-values-form-body a)])
			   
			   ; x \not\in theta_e-i => x \not\in theta_x-i
			   
			   (for-each 
			    (lambda (vs exp)
			      (for-each 
			       (lambda (v) 
				 (prop-theta-bar-at-ast 
				  v
				  (theta exp)))
			       vs))
			    raw-vars
			    the-vals)

			   ; x \not\in theta_body => x \not\in theta_ast 

			   (prop-theta-bar-at-ast a
						  (theta the-body))))]
		      
		      [letrec*-values-form-action

		       (lambda (a)
			 
			 (let ([the-vals (zodiac:letrec*-values-form-vals a)]
			       [raw-vars (zodiac:letrec*-values-form-vars a)]
			       [the-body (zodiac:letrec*-values-form-body a)])
			   
			   ; x \not\in theta_e-i => x \not\in theta_x-i
			   ; same as in plain let
			   
			   (for-each 
			    (lambda (vs exp)
			      (for-each 
			       (lambda (v) 
				 (prop-theta-bar-at-ast 
				  v
				  (theta exp)))
			       vs))
			    raw-vars
			    the-vals)

			   ; x \not\in theta_body => x \not\in theta_ast 

			   (prop-theta-bar-at-ast a
						  (theta the-body))))]
		      
		      [app-action

		       (lambda (a)

			 (let* ([rator (zodiac:app-fun a)]
				[rands (zodiac:app-args a)]
				[rator-lambdas (set->list (get-phi rator))]
				[rator-theta (theta rator)]
				[rands-thetas (map theta rands)])
			   
			   ; app-inv-rator

			   (prop-theta-bar-at-ast a (theta rator))

			   ; app-inv-rator-bv

			   (for-each ; for each lambda j

			    (lambda (j)

			      (let ([argss (zodiac:case-lambda-form-args j)])
				
				; argss : arglist list

				(for-each ; for each arglist args

				 (lambda (args) 

				   (let ([binders (zodiac:arglist-vars args)])

				     ; binders : lexical-binding list

				     (for-each ; for each binder b 

				      (lambda (b) 
					(prop-theta-bar-at-ast b rator-theta))

				      binders)))

				 argss)))

			    rator-lambdas)

			   ; app-inv-rand-bv 
			   
			   (for-each ; for each lambda j

			    (lambda (j)

			      (let ([argss (zodiac:case-lambda-form-args j)])
				
				; argss : arglist list

				(for-each ; for each arglist args

				 (lambda (args) 

				   (let ([binders (zodiac:arglist-vars args)])

				     ; binders : lexical-binding list

				     ; but consider form of arglist

				     (cond

				      [(zodiac:list-arglist? args)

				       (when (= (length rands) 
						(length binders)) 

					     ; differing lengths will raise runtime error
					     ; so can ignore alleged data flow

					     (for-each 

					      (lambda (b rt) 
						(prop-theta-bar-at-ast b rt))

					      binders rands-thetas))]

				      [(zodiac:sym-arglist? args)
				       
				       ; binders is list of length one
				       ; sole binder is bound to a list,
				       ; so can never be a lambda, so 
				       ; theta set should be empty

				       (set-theta! (car binders) empty-set)]

				      [(zodiac:ilist-arglist? args)

				       (when (> (length rands)
						(length binders))

					     ; otherwise runtime error

					     (let ilist-loop ([rts rands-thetas]
							[bs binders])

					       (if (null? (cdr bs))
						   (set-theta! (car bs) empty-set)
						   (begin 
						     (prop-theta-bar-at-ast (car bs) (car rts))
						     (ilist-loop (cdr rts) (cdr bs))))))]

				      [else

				       (compiler:internal-error 'deduce-theta-zactor 
								"Unknown arglist in: ~a~n" 
								(zodiac:parsed->raw j))])))

				 argss)))

			    rator-lambdas)

			   ; app-inv-body-app
			   
			   (for-each 
			    (lambda (j)
			      (let ([bodies (zodiac:case-lambda-form-bodies j)])
				(for-each
				 (lambda (body)
				   (prop-theta-bar-at-ast a (theta body)))
				 bodies)))
			    rator-lambdas)

			   ; app-inv-bv, app-inv-bv-app done during initialization
			   
			   ))]
		      
		      [set!-form-action

		       (lambda (a)

			 (let ([lhs (zodiac:set!-form-var a)]
			       [rhs (zodiac:set!-form-val a)])

			   (when (prop-theta-bar-at-ast lhs (theta rhs))

				 (cond

				  [(zodiac:top-level-varref/bind? lhs)
				   
				   (prop-top-level-binder-theta lhs)]

				  [(zodiac:bound-varref? lhs)

				   (prop-bound-varref-theta lhs)]

				  [else 

				   (compiler:internal-error 'init-theta-zactor 
							    "Unknown varref in ~a~n" 
							    (zodiac:parsed->raw a))]))))]

		      [varref-action

		       (lambda (a)

			 (when (zodiac:bound-varref? a)
			       (prop-bound-varref-theta a)))]

		      [define-values-form-action

			(lambda (a)

			  (let* ([the-vars (zodiac:define-values-form-vars a)]
				 [lhs (car the-vars)]
				 [rhs (zodiac:define-values-form-val a)]
				 [rhs-theta (theta rhs)])

			    (when (= (length the-vars) 1)
				  (prop-theta-bar-at-ast lhs rhs-theta)
				  (if (zodiac:bound-varref? lhs)
				      (prop-bound-varref-theta lhs)
				      (prop-top-level-binder-theta lhs)))))]

		      [begin-form-action

		       (lambda (a)

			 (let* ([forms (zodiac:begin-form-bodies a)]
				[last-one (car (last-pair forms))])
			   
			   ; theta for the begin is theta for the last form
			   
			   (set-theta! a (theta last-one))))]
		      
		      [begin0-form-action

		       (lambda (a)

			 (let* ([forms (zodiac:begin0-form-bodies a)]
				[first-one (list-ref forms 0)])
			   
			   ; theta for the begin is theta for the first form
			   
			   (set-theta! a (theta first-one))))])

		     (sequence (super-init)))))])
     
     ; at let and letrec*, apply rules
     ; theta_let \subseteq (theta_body \ {x1 ... xn})
     ; x \not\in theta_x (extended to multiple bindings)
     
     ; at case-lambda:
     ; limit thetas to variables in scope
     ; for each bv, bv not its own theta set (rule app-bv-inv-bv)
     ; in paper : rule only used when lambda is applied
     ; here     : do it once, don't need to do it again
     
     (traverse-ast-with-zactor ast deduce-theta-zactor)))

 (define (initialize-protocol-eq-classes ast) 
   (let ([init-equiv-classes-zactor
	  (make-object
	   (class zactor% ()
		  (override
		   [top-level-varref/bind-action
		    (lambda (a) 
		      (let ([rep (top-var-lookup (zodiac:varref-var a))])
			(unless (equiv-pi rep)
				(set-equiv-pi! rep (new-equiv-class rep)))
			(set-equiv-pi! a (equiv-pi rep))))]

		   [varref-action
		    (lambda (a)

		      (unless (equiv-pi a)

			      (let* ([binder (zodiac:bound-varref-binding a)])
				
				; snarf equiv-pi from binder

				; some "bound" variables appear not to have
				; binders
				
				(if binder  		
				    (let ([binder-pi (equiv-pi binder)])
				      (if binder-pi
					  (set-equiv-pi! a binder-pi)
					  (begin
					    (let ([new-pi (new-equiv-class binder)])
					      (set-equiv-pi! a new-pi)
					      (set-equiv-pi! binder new-pi)))))
				    (set-equiv-pi! a (new-equiv-class a))))))]

		   [app-action
		    (lambda (a)
		      (let* ([rator (zodiac:app-fun a)]
			     [rator-lambdas (set->list (get-phi rator))])
			
			(set-equiv-pi! a (new-equiv-class a))

			(set-equiv-pi! rator (new-equiv-class rator))

			; the paper says we shouldn't have to do this
			; but we prefer sanity to proofs

			(for-each
			 
			 (lambda (j) 

			   (unless (equiv-pi j)
				   (set-equiv-pi! j (new-equiv-class j)))

			   (merge-pi-classes! rator j))

			 rator-lambdas)))]

		   [default-action
		     (lambda (a)
		       (when (and (zodiac:parsed? a)
				  (not (equiv-pi a)))
			     (set-equiv-pi! a (new-equiv-class a))))])

		  (sequence (super-init))))])

     (traverse-ast-with-zactor ast init-equiv-classes-zactor)))

 (define (set-protocol-eq-classes ast) 
   (let ([build-classes-zactor
	  (make-object
	   (class zactor% ()
		  (override
		   [set!-form-action
		    (lambda (a)
		      (let* ([var (zodiac:set!-form-var a)]
			     [val (zodiac:set!-form-val a)])
			(merge-pi-classes! var val)))]
		   
		   [define-values-form-action
		     (lambda (a)
		       (let* ([vars (zodiac:define-values-form-vars a)]
			      [val (zodiac:define-values-form-val a)])
			 
			 (when (= (length vars) 1)
			       (merge-pi-classes! (car vars) val))))]
		   
		   [begin-form-action
		    (lambda (a)
		      (let* ([bodies (zodiac:begin-form-bodies a)])
			(merge-pi-classes! (car (last-pair bodies)) a)))]
		   
		   [begin0-form-action
		    (lambda (a)
		      (let* ([bodies (zodiac:begin0-form-bodies a)])
			(merge-pi-classes! (car bodies) a)))]
		   
		   [let-values-form-action
		    (lambda (a)
		      (let* ([binderss (zodiac:let-values-form-vars a)]
			     [vals (zodiac:let-values-form-vals a)]
			     [body (zodiac:let-values-form-body a)])
			
			; binderss : binding list list
			; vals     : parsed list
			
			; use rules analogous to app case here
			; in case of multiple values, merge information
			
			(for-each 
			 (lambda (bs v)
			   (for-each 
			    (lambda (b)
			      (merge-pi-classes! b v))
			    bs))
			 binderss   
			 vals)
			
			(merge-pi-classes! body a)))]
		   
		   [letrec*-values-form-action
		    (lambda (a)
		      (let* ([binderss (zodiac:letrec*-values-form-vars a)]
			     [vals (zodiac:letrec*-values-form-vals a)]
			     [body (zodiac:letrec*-values-form-body a)])
			
			; binderss : binding list list
			; vals     : parsed list
			
			; just like let 
			
			(for-each 
			 (lambda (bs v)
			   (for-each 
			    (lambda (b)
			      (merge-pi-classes! b v))
			    bs))
			 binderss   
			 vals)
			
			(merge-pi-classes! body a)))]
		   
		   [if-form-action
		    (lambda (a)
		      
		      ; rule cond-tag
		      
		      (let* ([then-part (zodiac:if-form-then a)]
			     [else-part (zodiac:if-form-else a)])
			
			(merge-pi-classes! a then-part)
			(merge-pi-classes! a else-part)))]

		   [app-action

		    (lambda (a) 
		      (let ([rator-lambdas (set->list (get-phi (zodiac:app-fun a)))]
			    [rands (zodiac:app-args a)]) 
			
			(for-each ; for each lambda for the rator 

			 (lambda (fun)  
			   (let ([fun-argss (zodiac:case-lambda-form-args fun)]
				 [fun-bodies (zodiac:case-lambda-form-bodies fun)])
			     
			     ; fun-args   : arglist list
			     ; fun-bodies : parsed list
			     
			     ; rule app-tag-bv-rand
			     
			     (for-each

			      (lambda (args)

				(let ([binders (zodiac:arglist-vars args)])

				  ; dispatch based on arglist class

				  (cond

				   [(zodiac:list-arglist? args)

				    (when (= (length rands)
					     (length binders))

					  (for-each
					   (lambda (binder rand)
					     (merge-pi-classes! binder rand))
					   binders rands))]

				   [(zodiac:sym-arglist? args)

				    ; nothing to do here

				    (void)]

				   [(zodiac:ilist-arglist? args)

				    (when (> (length rands)
					     (length binders))

					  (let ilist-pi-loop ([bs binders]
						     [rs rands])

					    (unless (null? (cdr bs))
						    (merge-pi-classes! (car bs) (car rs))
						    (ilist-pi-loop (cdr bs) (cdr rs)))))])))

			      fun-argss)
			     
			     ; rule app-tag-body-app
			     
			     (for-each
			      (lambda (fun-body)
				(merge-pi-classes! fun-body a))
			      fun-bodies)))
			 
			 rator-lambdas)))]

		   [class*/names-form-action
		    (lambda (a)
		      (let* ([init-vars (zodiac:class*/names-form-init-vars a)]
			     [init-vars-vars (zodiac:paroptarglist-vars init-vars)])

			; for initialized variables, merge classes for binder and expression

			(for-each
			 (lambda (v)
			   (when (pair? v)
				 (merge-pi-classes! (car v) (cdr v))))
			 init-vars-vars)))]

		   ; object clauses

		   [public-clause-action
		    (lambda (a)
		      (for-each
		       (lambda (v exp)
			 (merge-pi-classes! v exp))
		       (zodiac:public-clause-internals a)
		       (zodiac:public-clause-exprs a)))]

		   [override-clause-action
		    (lambda (a)
		      (for-each
		       (lambda (v exp)
			 (merge-pi-classes! v exp))
		       (zodiac:override-clause-internals a)
		       (zodiac:override-clause-exprs a)))]

		   [private-clause-action
		    (lambda (a)
		      (for-each
		       (lambda (v exp)
			 (merge-pi-classes! v exp))
		       (zodiac:private-clause-internals a)
		       (zodiac:private-clause-exprs a)))])

		  (sequence (super-init))))])
     
     ; apply equality rules for pi's
     
     (traverse-ast-with-zactor ast build-classes-zactor)))
 
 (define (compute-protocols ast)
   (let ([compute-candidate-vars-zactor
	  (make-object
	   (class zactor% ()
		  (override
		   [case-lambda-form-action
		    (lambda (a) 
		      (let* ([class-rep (equiv-pi-rep a)]

			     ; can't use code-free-vars here
			     ; because may contain anchor variable
			     ; not actually in scope

			     [fvs (get-fvs a)])

			(set-candidate-vars! class-rep
					     (set-union 
					      (candidate-vars class-rep)
					      fvs))))])

		  (sequence (super-init))))]

	 [take-intersects-zactor
	  (make-object
	   (class zactor% ()
		  (override
		   [default-action 
		     (lambda (a)
		       (when (zodiac:parsed? a)

			     (let* ([class-rep (equiv-pi-rep a)]
				    [class-rep-pi (pi class-rep)])

			       (cond 
				[(unknown? a)
				 (set-pi! class-rep '())]
				[class-rep-pi
				 (set-pi! class-rep
					  (set->list 
					   (set-intersect 
					    (list->set class-rep-pi) 
					    (theta a))))]
				[else
				 (set-pi! class-rep 
					  (set->list 
					   (set-intersect
					    (theta class-rep)
					    (theta a))))]))))]
		   
		   [app-action
		    (lambda (a) 
		      (let* ([rator (zodiac:app-fun a)]
			     [rator-theta (theta rator)]
			     [class-rep (equiv-pi-rep rator)]
			     [class-rep-pi (pi class-rep)])
			
			(cond
			 [(or (unknown? rator) 
			      (set-empty? (get-phi rator)))
			  (set-pi! class-rep '())]

			 ; rule app-tag-cons

			 [class-rep-pi

			  (set-pi! class-rep 
				   (set->list ; maintain pi as list
				    (set-intersect 
				     (list->set class-rep-pi) 
				     (set-intersect rator-theta
						    (candidate-vars class-rep)))))]
			 [else

			  (set-pi! class-rep 
				   (set->list 
				    (set-intersect rator-theta
						   (candidate-vars class-rep))))])))])
		  (sequence (super-init))))]

	 [eliminate-light-nonempty-closures-zactor
	  (make-object
	   (class zactor% ()
		  (override
		   [case-lambda-form-action
		    (lambda (a) 
		      (let* ([code (get-annotation a)]
			     [fvs (code-free-vars code)]
			     [class-rep (equiv-pi-rep a)]
			     [class-rep-pi (pi class-rep)])
			(unless (and class-rep-pi 
				     (set-subset? fvs (list->set class-rep-pi)))
				(set-pi! class-rep '()))))])
		  (sequence (super-init))))]

	 [assign-equiv-pis-zactor
	  (make-object
	   (class zactor% ()
		  (override
		   [default-action
		     (lambda (a) ; ignore scope-vars
		       (when (zodiac:parsed? a)
			     (let ([class-rep-pi (pi (equiv-pi-rep a))])
			       (if class-rep-pi
				   (set-pi! a class-rep-pi)
				   (set-pi! a '())))))])
		  (sequence (super-init))))])
     

     (traverse-ast-with-zactor ast compute-candidate-vars-zactor)

     ; rule app-tag-cons
     
     (traverse-ast-with-zactor ast take-intersects-zactor) 
     
     ; for each lambda, see if all its FV are in the current protocol
     ; if not, protocol becomes empty list
     ; no lightweightness for the sake of lightweightness

     ; (traverse-ast-with-zactor ast 
     ;                           eliminate-light-nonempty-closures-zactor) 
     
     ; each elt of equivalence class gets same protocol
     
     (traverse-ast-with-zactor ast 
			       assign-equiv-pis-zactor)))

 (define (make-fresh-binder-at binding ast)
   (let ([binder (car (zodiac:create-lexical-binding+marks
		       (zodiac:structurize-syntax 
			(zodiac:binding-var binding)
			ast)))])
     ; (zodiac:set-binding-var! binder (zodiac:binding-var binding)) ; override gensym
     binder))

 ; compute set of fv's (without reliance on code-free-vars)

 (define (get-fvs ast)
   (let ([fv-zolder (make-object fv-zolder%)])
     (traverse-ast-with-zolder ast fv-zolder)))

 ; update-variable-sets!
 ; sets free-var, global, and captured sets

 (define (update-variable-sets! ast) 
   (let* ([get-binder-anchors 
	   (lambda (bs)  ; : lexical-binding set -> lexical-binding set
	     (list->set 
	      (filter identity (map (compose binding-anchor get-annotation)
				    (filter zodiac:lexical-binding?
					    (set->list bs))))))]
	  [fv-zolder
	   (make-object
	    (class fv-zolder% ()

		   (rename 

		    [super-class*/names-form-folder class*/names-form-folder]
		    [super-unit-form-folder unit-form-folder])

		   (override

		    [binder-set-minus ; throw out anchors
		     (lambda (fvs bs)
		       (set-minus fvs (set-union bs (get-binder-anchors bs))))])

		   (override

		    [varref-folder 
		     (lambda (a) 
		       (if (zodiac:bound-varref? a)
			   (let ([b (zodiac:bound-varref-binding a)])
			     (if b
				 (let* ([anchor (binding-anchor (get-annotation b))])
				   (if anchor
				       (list->set (list b anchor))
				       (make-singleton-set b)))
				 empty-set))
			   empty-set))]

		    [case-lambda-form-folder 
		     (lambda (a _ bodies-fvs)
		       (let* ([argss           ; lexical-binding list list
			       (map zodiac:arglist-vars 
				    (zodiac:case-lambda-form-args a))]
			      [arg-setss       ; lexical-binding set list
			       (map list->set argss)]
			      [cases-fvs 
			       (map binder-set-minus bodies-fvs arg-setss)]
			      [code (get-annotation a)]
			      [case-codes (procedure-code-case-codes code)]
			      [all-cases-fvs (fold-sets cases-fvs)])

			 (for-each
			  (lambda (cc fv)
			    (set-code-free-vars! cc fv))
			  case-codes
			  cases-fvs)

			 (set-code-free-vars! code all-cases-fvs)
			 
			 all-cases-fvs))]

		    [class*/names-form-folder
		     (lambda (a this-fvs super-init-fvs super-expr-fvs 
				interface-fvss init-vars-fvss inst-clause-pairs)
		       (let* ([super-result
			       (super-class*/names-form-folder
				a
				this-fvs super-init-fvs super-expr-fvs
				interface-fvss init-vars-fvss inst-clause-pairs)]
			      [code (get-annotation a)])
			 (set-code-free-vars! code super-result)
			 super-result))]

		    [unit-form-folder
		     (lambda (a imports-fv defines-fv clauses-fv)
		       (let ([super-result
			      (super-unit-form-folder
			       a
			       imports-fv defines-fv clauses-fv)]
			     [code (get-annotation a)])
			 (set-code-free-vars! code super-result)
			 super-result))])

		   (sequence (super-init))))]

	  [cv-zactor
	   (make-object
	    (class zactor% ()
		   (override
		    [case-lambda-form-action
		     (lambda (a)
		       (let* ([code (get-annotation a)]
			      [case-codes (procedure-code-case-codes code)])

			 (set-code-captured-vars! 
			  code 
			  (set-union (code-free-vars code)
				     (code-local-vars code)))

			 (for-each
			  (lambda (cc)
			    (set-code-captured-vars! cc
						     (set-union (code-free-vars cc)
								(code-local-vars cc))))
			  case-codes)))]

		    [class*/names-form-action
		     (lambda (a)
		       (let ([code (get-annotation a)])
			 (set-code-captured-vars! 
			  code 
			  (set-union (code-free-vars code)
				     (code-local-vars code)))))]

		    [unit-form-action
		     (lambda (a)
		       (let* ([unit-code (get-annotation a)]
			      [fvs (code-free-vars unit-code)]
			      [lvs (code-local-vars unit-code)])

			 (set-code-captured-vars! unit-code
						  (fold-sets (list fvs lvs 
								   (list->set 
								    (append (unit-code-import-anchors unit-code)
									    (unit-code-export-anchors unit-code))))))))])

		   (sequence (super-init))))])
     
     ; order here is critical

     (traverse-ast-with-zolder ast fv-zolder)
     (traverse-ast-with-zactor ast cv-zactor)))

 ; update max-arity annotations

 (define (update-max-arities! ast) 

   (let* ([max-over-list 
	   ; works with empty list, (apply max ...) doesn't
	   (lambda (lst)
	     (apply max (cons 0 lst)))]

	  [max-arity-zolder

	   (make-object

	    (class zolder% ()

		   (override

		    [default-folder
		      (lambda (ast . maxes) 0)]

		    [if-form-folder
		     (lambda (_ test-max then-max else-max)
		       (max test-max then-max else-max))]
		    
		    [case-lambda-form-folder 
		     (lambda (a _ bodies-maxs)
		       (let* ([code (get-annotation a)]
			      [curr-max (closure-code-max-arity code)]
			      [lambda-max (max-over-list bodies-maxs)])

			 ; don't allow shrinkage

			 (set-closure-code-max-arity! 
			  code 
			  (max curr-max lambda-max))

			 ; stop propagation at lambda boundary

			 0))]

		    [set!-form-folder 
		     (lambda (_ __ rhs-max)
		       rhs-max)]

		    [define-values-form-folder 
		      (lambda (_ __ rhs-max)
			rhs-max)]
		    
		    [let-values-form-folder 
		     (lambda (_ __ vals-maxs body-max)
		       (max (max-over-list vals-maxs) body-max))]

		    [letrec*-values-form-folder
		     (lambda (_ __ vals-maxs body-max)
		       (max (max-over-list vals-maxs) body-max))]

		    [app-folder 
		     (lambda (a rator-max rands-maxs)
		       (let ([rands (zodiac:app-args a)])
			 (max rator-max 
			      (max-over-list rands-maxs) 
			      (length rands))))]

		    [begin-form-folder 
		     (lambda (_ bodies-max)
		       (max-over-list bodies-max))]

		    [begin0-form-folder 
		     (lambda (_ bodies-max)
		       (max-over-list bodies-max))]

		    [class*/names-form-folder
		     (lambda (a this-max super-init-max super-expr-max
				interface-maxs init-vars-maxs inst-clause-maxs)
		       (let* ([code (get-annotation a)]
			      [curr-max (closure-code-max-arity code)]
			      [interfaces (zodiac:class*/names-form-interfaces a)]
			      
			      [class-max (max super-init-max 
					      super-expr-max 
					      (length interfaces)
					      (max-over-list (map cdr (filter pair? init-vars-maxs)))
					      (max-over-list inst-clause-maxs))])

			 ; no shrinkage

			 (set-closure-code-max-arity! 
			  code 
			  (max curr-max class-max))

			 ; stop propagation at class boundary

			 0))]
		    
		    [interface-form-folder
		     (lambda (_ super-exprs-maxs)
		       (max-over-list super-exprs-maxs))]

		    [public-clause-folder
		     (lambda (_ internal-maxs exprs-maxs)
		       (max (max-over-list internal-maxs) 
			    (max-over-list exprs-maxs)))]

		    [override-clause-folder
		     (lambda (_ internal-maxs exprs-maxs)
		       (max (max-over-list internal-maxs) 
			    (max-over-list exprs-maxs)))]

		    [private-clause-folder
		     (lambda (_ internal-maxs exprs-maxs)
		       (max (max-over-list internal-maxs)
			    (max-over-list exprs-maxs)))]

		    [inherit-clause-folder
		     (lambda (_ internal-maxs)
		       (max-over-list internal-maxs))]

		    [rename-clause-folder
		     (lambda (_ internal-maxs)
		       (max-over-list internal-maxs))]

		    [sequence-clause-folder
		     (lambda (_ exprs-maxs)
		       (max-over-list exprs-maxs))]

		    ; units

		    [unit-form-folder
		     (lambda (a _ __ clauses-maxs)
		       (let* ([code (get-annotation a)]
			      [curr-max (closure-code-max-arity code)]
			      [unit-max (max-over-list clauses-maxs)])

			 ; no shrinkage

			 (set-closure-code-max-arity! 
			  code 
			  (max curr-max unit-max))
			 
			 ; stop propagation at unit boundary

			 0))]

		    [compound-unit-form-folder
		     (lambda (a _ __ ___)
		       (let ([links (zodiac:compound-unit-form-links a)])
			 (length links)))]

		    [invoke-unit-form-folder
		     (lambda (a _ __)
		       (let ([vars (zodiac:invoke-unit-form-variables a)])
			 (add1 (* 2 (length vars)))))]

		    [invoke-open-unit-form-folder
		     (lambda (_ unit-max variables-maxs)
		       (max unit-max (max-over-list variables-maxs)))])

		   (sequence (super-init))))])

     (traverse-ast-with-zolder ast max-arity-zolder)))

 (define (get-varrefs ast)
   (let* ([varrefs '()]
	  [get-varrefs-zactor
	   (make-object
	    (class zactor% ()
		   (override
		    [varref-action
		     (lambda (a)
		       (when (zodiac:lexical-varref? a)
			     (set! varrefs (cons a varrefs))))])
		   (sequence (super-init))))])
     
     ; get all bound variable subterms
     
     (traverse-ast-with-zactor ast 
			       get-varrefs-zactor) 
     
     varrefs))

 (define (lightweight-transform ast)
   (let* ([get-arg-rep-len
	   (lambda (a)
	     (length (zodiac:arglist-vars
		      (car (zodiac:case-lambda-form-args a)))))]
	  [transform-zolder
	   (make-object
	    (class zolder% ()
		   (override
		    [default-folder void]
		    [case-lambda-form-folder 
		     (lambda (a _ __)
		       (let* ([old-arg-rep-len (get-arg-rep-len a)]
			      [code (get-annotation a)]
			      [class-rep (equiv-pi-rep a)]
			      [pi-list (pi class-rep)]
			      [case-codes (procedure-code-case-codes code)] 
			      [case-fv-binder-sets (map code-free-vars case-codes)]
			      [new-binder-lists '()]
			      [transform-lambda-case
			       (lambda (arglist body fv-binder-set) 
				 (let ([this-case-body-varrefs (get-varrefs body)]
				       [this-case-new-binders 
					(map (lambda (v) 
					       (make-fresh-binder-at v a))
					     pi-list)])
				   
				   ; save new-binder info in reverse order
				   
				   (set! new-binder-lists 
					 (cons this-case-new-binders new-binder-lists))
				   ; tentatively give all binders dummy annotations
				   ; in case they don't bind anything

				   (for-each
				    (lambda (b)
				      (set-annotation! b binder:empty-anno))
				    this-case-new-binders)

				   ; add new parameters

				   (unless (null? this-case-new-binders)

					   (zodiac:set-arglist-vars! 
					    arglist
					    (append this-case-new-binders
						    (zodiac:arglist-vars arglist))))
				   
				   ; wire varrefs to new parameters
				   ; Note: applications within this body have already been
				   ;  transformed, adding varrefs wired to the old bindings;
				   ;  The following re-wiring adjust those varrefs, too.

				   (for-each
				    (lambda (v) 
				      (let ([old-binder (zodiac:bound-varref-binding v)])
					(ormap (lambda (pi-elt new-binder)

						 (if (eq? old-binder pi-elt)
						     
						     (begin

						       ; grab annotation from old binder, copy bits to new

						       (set-annotation! new-binder
									(copy-binding-for-light-closures
									 (get-annotation old-binder)))
						       
						       ; wire in new binder
						       
						       (zodiac:set-bound-varref-binding! v new-binder)
						       (zodiac:set-varref-var! v (zodiac:binding-var new-binder)))

						     ; else
						     
						     #f))
					       pi-list
					       this-case-new-binders)))
				    this-case-body-varrefs)))]
			      [sym-arglist->ilist-arglist
			       (lambda (as)
				 (let ([the-vars (zodiac:arglist-vars as)])
				   (if (and (zodiac:sym-arglist? as)
					    (> (length the-vars) 1))
				       (zodiac:make-ilist-arglist the-vars)
				       as)))])

			 ; for each case in the case-lambda
			 
			 (for-each transform-lambda-case
				   (zodiac:case-lambda-form-args a)
				   (zodiac:case-lambda-form-bodies a)
				   case-fv-binder-sets)

			 (zodiac:set-case-lambda-form-args! 
			  a
			  (map sym-arglist->ilist-arglist (zodiac:case-lambda-form-args a))) 

			 (when (compiler:option:verbose)
			       (let ([new-arg-rep-len (get-arg-rep-len a)]) 
				 (when (< old-arg-rep-len new-arg-rep-len)
				       (compiler:warning 
					a 
					(format 
					 "added formal arguments (~a) to procedure"
					 (- new-arg-rep-len old-arg-rep-len)
					 ; " ~n   free: ~a~n   added: ~a"
					 ; (map zodiac:binding-var (set->list (code-free-vars code)))
					 ; (map zodiac:binding-var (car new-binder-lists))
					 )))))
			 
			 ; update local var information

			 ; free variable information to be updated post-transformation

			 (set! new-binder-lists (reverse new-binder-lists))

			 (for-each
			  (lambda (case-code new-binder-list)
			    (add-code-local+used-vars! 
			     case-code
			     (list->set new-binder-list)))
			  case-codes
			  new-binder-lists)))]

		    [app-folder
		     (lambda (a _ __)
		       (let* ([rator (zodiac:app-fun a)]
			      [rands (zodiac:app-args a)]
			      [class-rep (equiv-pi-rep rator)]
			      [pi-list (pi class-rep)]
			      [new-vars
			       (map 
				(lambda (binder)
				  (let* ([new-v (zodiac:make-lexical-varref
						 (zodiac:zodiac-origin a)
						 (zodiac:zodiac-start a)
						 (zodiac:zodiac-finish a)
						 (make-empty-box)
						 (zodiac:binding-var binder)
						 binder)])

				    (set-annotation! new-v 
						     (varref:empty-attributes))

				    (varref:add-attribute! new-v varref:env)

				    new-v))
				pi-list)])

			 (zodiac:set-app-args! a (append new-vars rands))

			 (when (and (compiler:option:verbose)
				    (> (length new-vars) 0))
			       (compiler:warning 
				a 
				"added arguments to application"))))])
		   
		   (sequence 
		     (super-init))))])

     (traverse-ast-with-zolder ast transform-zolder)
     (update-max-arities! ast)
     (update-variable-sets! ast)))

 (define (lightweight-analyze-and-transform asts)

   (for-each 
    make-global-tables-and-set-scopes 
    asts)

   ; fixed point is over entire file

   (closure-analyze asts)

   (for-each 
    (lambda (ast)
      (escape-analyze ast)
      (initialize-invariance-sets ast)
      (initialize-protocol-eq-classes ast))
    asts)
   
   (for-each
    (lambda (ast)
      (invariance-analyze ast)
      (set-protocol-eq-classes ast)
      (compute-protocols ast))
    asts)

   (for-each
    lightweight-transform
    asts))

 ) ; end unit/sig

