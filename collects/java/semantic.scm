;; Mario Latendresse, 14 May 2000.
;;
;; The section numbers refer to Gosling, Joy, Steele ``The Java
;; language specification'' (JLS), 1996.  An attribute grammar for
;; semantic analyzes can also be found in ``Formal Syntax and
;; Semantics of Java'', LNCS 1523.
;;

;; Some semantic verification was done using Sun's compiler.
;; Semantic verification has been identified by a section number from
;; JLS book.  

;; Here are some explanations about the techniques used to verify
;; semantic and type checking, but more information is available in
;; the file docCompiler.tex.
;;
;; It is assumed that parsing has been done using the parser generated
;; by the grammar described in the file `java.lal'. Therefore, the
;; whole compilation unit is a structure n-program.
;;
;; To make forward references easy to handle, three consecutive walks
;; are done.
;;
;; A first walk registers the names of classes and interfaces only,
;; and verifies their modifiers. An empty table is created for each
;; class and interface, which is part of their `type'. This type, is
;; put in the type field of the mt structure. Therefore, it
;; creates a second level table for each class and interface.  That
;; table will contain the field declarations found in each class and
;; interface.
;;
;; A second walk registers all field declarations and verifies extends
;; and implements. These fields are put inside each individual table
;; of each class and interface.
;;
;; A third walk goes inside every method body and verifies the
;; semantic including type checking.
;;
;;
;; The general table *global-table* has several levels.
;;
;; The first level contains the references from the import
;; statements. An import statement without a star should refers to an
;; existing type, even though it is not refer to by the code. These
;; are searched in the file system as soon as they are met. It is a
;; compile-time error if the corresponding class file cannot be
;; accessed. The import statements with a star have a lazy
;; semantic. The implicit directory structure of the import statement
;; may not exist nor be accessible. When a type cannot be found these
;; import statements are consulted. The directory structure is checked
;; to see if it is accessible. If it is not, it is not an error, but
;; the import statement is removed from the table. If it is, all
;; accessible class file names are put in the table in two forms: the
;; simple name and its full qualified name. The type itself is not
;; read yet, except the one that was searched. In that case the whole
;; class file is read and decoded to form a type. It can be an
;; interface or a class.


;; The following table contains all types from the file and all types
;; that will be read from the libraries (classes from lazy imports and
;; the file system)

(define *global-table* #f)

;; I : p, a structure n-program
;; O : verifies semantic errors of program p and return a table or #f.

(define (java-ver-sem p)
  ;; The top level table (the global table).
  (let* ((Gtable (make-Gtable '() '())))
    (set! *global-table* Gtable)
    (first-pass p Gtable)
    (and (second-pass p Gtable) (third-pass Gtable p))))

;; Creates the top level table: each class and interface identifier is
;; put in the top level table with a type containing an empty second
;; level table.
;;
;; I: p, (s n-program)
;; O: (s table)
(define (first-pass p Gtable)
    
  ;; I: type, (s n-type); O: void
  (define (add-type type)
    (match type (($ n-type header members src1)
      (match header (($ n-header mdfs classWord id extends implements src2)

	;; Verify that an interface or class of the same name does
	;; not already exist in this file or is accessible via
	;; a single-type-import. (9.1). 
	
	(cond
	 ((lupL Gtable id 'type)
	  (sem-err #f src1 "Class or interface "id" is multiply defined. Skipping."))
	 ((and (eqv? classWord 'interface) (not (null? implements)))
	  ;; If it is an interface it may have several extends, but it
	  ;; may not have implements; 
	  (sem-err #f src2 "Interface may not have implements. Skipping."))
	 ;; whereas if it is a class it may
	 ;; have one extend with several implements. (8.1.3) (9.1.3)
	 ((and (eqv? classWord 'class) (> (length extends) 1))
	  (sem-err #f src2 "Cannot extend more than one class. Skipping."))
	 ((intersect? '(char byte short int long float double)
		      (append implements extends))
	  (sem-err #f src2 "Cannot implement or extend a primitive type. Skipping."))
	 (else 
	  (verify-type-mdfs mdfs classWord id src2)
	  
	  ;; If a class is not extending a class or interface
	  ;; it is extending java.lang.Object by default. (8.1.3)
	  ;; Remove extend and implement nodes that are not n-qName
	  ;; since they are primitive types.
	  
	  ;; Important: extends become interfaces for an interface
	  ;;            but it stays the same for a class.
	  (let* ((extends2      (filter n-qname? extends))
		 (implements2   (filter n-qname? implements))
		 (Ttable        (make-Ttable '() Gtable))
		 (idType (if (eqv? classWord 'interface)
			     ;; For interface, there is no default Object
			     ;; extension (9.1.3)
			     (make-type #f id (union '(abstract) mdfs) 
					;; No extend for an interface.
					#f
					;; The extends become the interfaces
					extends2 Ttable #t)
			     (make-type #t id mdfs 
					(if (null? extends2)
					    (make-n-qName '("java" "lang" "Object")
							     src2 #f)
					    (car extends2))
					implements2 Ttable #t))
			 ))
	    ;; Add the type to the global table.
	    (add-name-table! Gtable id idType 'type mdfs src1))
	    
	  ;; Add the list of member declarations as a value to the type.
	  (table-value-set! Gtable id members)
	  )))))))
  
   (match p (($ n-program package imports types src)

    ;; A package redeclaration should not occurred as the token
    ;; package is not recognised after one has been seen. It is just
    ;; in case the grammar is modified.
    (if (and package (package? Gtable))
	(sem-err #f (n-qName-src package)"Invalid multiple package declaration.")
	;; Add package name to the global table.
	(add-package-name! Gtable package))
    
    ;; Add implicit import `import java.lang.*;' to the global table.
    (add-imports-names! 
     Gtable (list (make-n-import (make-n-qName '("java" "lang") #f #f) #t #f)))
    
    ;; Add explicit imports to the global table.
    (add-imports-names! Gtable imports)
    
    ;; Add all types to the global table.
    (for-each add-type (reverse types))
    
    ;; Return global table.
    Gtable
    )))

;; Add package name to global table. There should be only one such addition
;; per global table.
;; 
;; I: Gtable, a (s Gtable). 
;;    package, (u #f (s n-qName)).
;; O: void.
;;
(define (add-package-name! Gtable package)
  (if package
      (let* ((Pname (n-qName-name package))
	     (src   (n-qName-src package)))
	(add-name-table! Gtable (if (pair? Pname) Pname (list Pname)) 
			 'package 'package '() src))))

;; I: T, (u (s Gtable) (s Ltable) (s Ttable) (s type))
;; O: [str]
(define (get-pack-name T)
  (let ((p (get-pack (get-Gtable T))))
    (if p (mt-id p) '())))

(define (package? T) (if (get-pack (get-Gtable T)) #t #f))

;; I: Gtable, (s Gtable)
;; O: (u #f (s mt))
(define (get-pack Gtable)
  (and Gtable
       (let ((package (filter (lambda (x) (eqv? 'package (mt-kind (cdr x))))
			      (table-names Gtable))))
	 (if (pair? package)
	     (cdar package)
	     #f))))
      
;; -------------------------------------------------------------------------
;; Imports
;;
;; There are two kinds of import: specific and lazy. A specific import
;; is completly qualified. A lazy import uses the star as the last identifier
;; of a qualified name.
;;
;; A specific import must exist even though there might be no
;; reference to it in the code. Therefore, such imports are verified
;; before being used. In this implementation, the class file is read
;; and transformed in internal form. If the class file does not exist
;; an error message is given. The full name as well is inserted in the table.
;; It is the only way to refer to this type.
;;
;; A lazy import might specify a non existing directory path. This is
;; not a semantic error. In that case, it does not import any type. We
;; use a lazy strategy, that is the class files, even the directory is
;; not consulted until necessary. It is necessary to consult the
;; directory when a type cannot be found. In that case the directory
;; is searched for the corresponding class file. When it is found it
;; is read and transformed into internal form. Two names are inserted
;; into the table: the fully qualified name and the type name without
;; qualification.
;; 
;; The extends and implements parts of a type might contain a name.
;; These are also handled lazily. They are searched when necessary.
;;
 
;; Add all imports declaration to the global table.
;;
;; I: table, (s Gtable)
;;    imports, [(s n-import)]
;; O: void.
;;
(define (add-imports-names! Gtable imports)
  (if (pair? imports) (for-each (c.1 add-import-name! Gtable) imports)))

;; Add a type-import-on-demand declaration or a single-type-import to
;; the global table.
;; I: Gtable, (s Gtable).
;;    import, (s n-import)
;; O: void.
;;
(define (add-import-name! Gtable import)
  (let* ((qname (n-qName-name  (n-import-qName import)))
	 (src   (n-qName-src   (n-import-qName import)))
	 (lazy  (n-import-star import))
	 (sname (list-last qname)))
    (if lazy
	;; A type-import-on-demand declaration (7.5.2)
	(set-Gtable-imports! Gtable (: qname (Gtable-imports Gtable)))
	;; A single-type-import (7.5.1)
	(let* ((class-file-name      (++ (qName->classfName qName) ".class"))
	       (full-class-file-name (search-classpath-file CLASSPATH "" 
							    class-file-name)))
	  (if (not full-class-file-name)
	      (sem-err #f src "Imported class file " class-file-name
		       " cannot be accessed. CLASSPATH is " CLASSPATH)
	      (let* ((class  (read-class-dir "" full-class-file-name))
		     (type   (class->ast class Gtable)))
		;; TBF. Should verify that it is public?
		;; Add the imported type using its full name.
		(add-name-table! Gtable qname type 'type (type-mdfs type) src)
		;; The type is also known through its simple name.
		(add-name-table! Gtable sname type 'type (type-mdfs type) src)
		)))
	)))

;; Report semantic errors for type modifiers.
;;
;; I: mdfs, [sym]
;;    classWord, (u 'class 'interface)
;;    id, str. The name of this class or interface.
;; O: void.
;;
(define (verify-type-mdfs mdfs classWord id src)
  ;; At this point, it is not possible to verify the following due to
  ;; possible cycle in the class definitions, this is verified by the
  ;; third walk: If the class contains at least one method without a
  ;; body, it must be declared abstract. This method may be inherited.
  (case classWord
    ((class)
     (if (not (included? mdfs '(public abstract final)))
	 (sem-err #f src "Possible class modifiers are public, abstract, and final; "
		  " found "mdfs" on class "id"."))
     (if (duplicates? mdfs)
	 (sem-err #f src "Duplicate modifiers "mdfs" for class "id"."))
     ;; A class cannot be declared final and abstract. (8.1.2.2)
     (if (and (member 'final mdfs) (member 'abstract mdfs))
	 (sem-err #f src "A class cannot be final and abstract: "id".")))
    ((interface)
     ;; (9.1.2)
     (if (not (included? mdfs '(public abstract)))
	 (sem-err #f src "Possible interface modifiers are public and abstract; "
		    " found "mdfs" on interface "id"."))
     (if (duplicates? mdfs)
	 (sem-err #f src "Duplicate modifiers " mdfs " for interface "id"."))
     ;; (9.1.2.1)
     (if (member 'abstract mdfs)
	 (sem-err #f src "Redundant modifier since interfaces are always abstract."))
     )))

;; ==========================================================================
;; Second pass to register members of types.
;;
;; I: p, (s n-program)
;;    table, (s Gtable) 
;;    
;; O: (u #f (s Gtable)). If #f compilation should stop.
;;
(define (second-pass p table)
  ;; I: nodeType, (s n-type)
  ;;    cName, str
  ;; O: #t iff there is at least one constructor in the type.
  (define (constructor-in-class? nodeType cName)
    (p-exists? (lambda (m) (and (n-method? m) (string=? (n-method-id m) cName)
				(not (n-method-type m))))
	       (n-type-members nodeType)))

  ;; I: T, (s type)
  ;; O: void
  (define (add-members T types)
    (let* ((id   (list-last (type-name T)))
	   (node (:~-s id types (c* n-header-id n-type-header))))
      ;; Add an <init> method, if none exists. 
      (if (and (type-class? T) (not (constructor-in-class? node id)))
	  (set-n-type-members! node (: (default-constructor (n-type-src node))
				       (n-type-members node))))
      ;; Add all method and constructor names first. (for block initializers)
      (for-each (c.2 add-method-declaration T) 
		(filter n-method? (n-type-members node)))
      ;; Analyze all the rest.
      (for-each (c.1 add-member! T) (filter (c* not n-method?) (n-type-members node)))
      ))
  
  (match p (($ n-program package imports types)
    (let* ((inTypes (filter (lambda (x) (and (type? x) (type-inside? x)))
			    (map (c* mt-type cdr) (table-names table)))))
      ;; Replace extend names by their structure.
      ;; TBF verify duplication of extends and interfaces?
      (for-each (lambda (type) 
		  (extName->type! table type) (iNames->type! table type)) inTypes)

      ;; If there is a cycle in types, the compilation should stop now.
      (if (cycle-in-types? table)
	  #f
	  (begin
	    ;; Verify that overriding of methods is properly done.
	    (ver-methods-overHiding inTypes)    
	    ;; No cycle in types should exists for the rest of the compilation.
	    (for-each (c.2 add-members types) inTypes)
	    table))
      ))))

;; Verify that extends do exist, and replace the name by its type
;; node.  I: type, (s type) O: void.
(define (extName->type! table type)
  
  ;; I: extend, (u qName (s n-qName))
  ;; O: (u #f (s type))
  (define (qName->type  extend)
    (let* ((type-name (or (and (qName? extend) extend) (n-qName-name extend)))
	   (src       (or (and (qName? extend) LIB-SRC) (n-qName-src extend)))
	   (mt        (lup table type-name #f 'type src))
	   (type      (mt-type mt)))
      (cond 
       ((typeUndef? type)    #f)
       ((not (type-class? type))
	(sem-err #f src "Extended type is not a class. Its type is " (T->s type)))
       ((final-class? type) ;; 8.1.3
	(sem-err #f src "Cannot extend final class "(T->s type-name)"."))
       (else type))))

  (if (or (qName? (type-extend type)) (n-qName? (type-extend type)))
      (set-type-extend! type (qName->type (type-extend type)))))

;; Verify that interfaces do exist, and replace the name by its
;; type node.  I: type, (s type) O: void.
(define (iNames->type! table type)
  ;; I: interface, (u qName (s n-qName))
  (define (iName->type iName)
    (let* ((qName (or (and (qName? iName) iName) (n-qName-name iName)))
	   (src   (or (and (qName? iName) LIB-SRC) (n-qName-src iName)))
	   (mt    (lup table qName #f 'type src))
	   (Itype (mt-type mt)))
      (if (and (not (typeUndef? Itype)) (not (type-interface? Itype)))
	  ;; 9.1.3
	  (sem-err 'undefined src (qName->s qName)" is not an interface. Its type is: " 
		   (T->s Itype))
	  Itype)))
  (if (pair? (type-interfaces type))
      (set-type-interfaces! type (filter (c* not typeUndef?) 
					 (map iName->type (type-interfaces type)))
			    )))

;; An AST for a constructor with no parameter.
;; I: src, (s n-src)
(define (default-constructor src)
  ;; <init> () { super(); }
  (make-n-method '(public) #f "<init>" '() #f '()
   (make-n-block
    (list (make-n-call (make-n-specialName 'super src) '() src
		       ;; The method type information will be added by
		       ;; ver-call.
		       #f))
    src) src #f #f))

;; Verify that no cycles exist due to the extends of classes and
;; interfaces. 
;;
;; I: (s table) 
;; O: bool, #t => there is a cycle.
;;
(define (cycle-in-types? table)
  (let* ((cycle? #f))
    (for-each (lambda (mt)
		(if (and (type? (mt-type mt)) (type-inside? (mt-type mt)))
		    (let* ((one-cycle? (verify-cycle! (mt-type mt) (mt-src mt))))
		      (remove-all-marks! table)
		      (set! cycle? one-cycle?)
		      )))
	      (map cdr (table-names table)))
    cycle?))

;; Remove all cycle detection marks in the extend of types
(define (remove-all-marks! table)
  (define (remove-marks mt)
    (let* ((type    (mt-type mt))
	   (extend  (type-extend type)))
      (if (and (pair? extend) (integer? (car extend)))
	  (set-type-extend!  type (cdr extend)))))
  
  (for-each remove-marks (filter (lambda (x) (type? (mt-type x)))
				 (map cdr (table-names table)))))


;; I: type, (s type)
;;    no, int. This is the mark to identify the root of the path.
;; O: bool
;;
;; A cycle exists iff by marking the extended types, a mark type is
;; encountered.
;;
(define (verify-cycle! type src)
  (let* ((extend (type-extend type)))
    (cond 
     ;; If it has been visited.
     ((and (pair? extend) (integer? (car extend)))
      (sem-err #t src "Cyclic type definition found at "(T->s type)"."))
     ;; If it has some extension and has not been visited.
     ((and (not (pair? extend)) (or extend (pair? (type-interfaces type))))
      ;; Mark the extension.
      (set-type-extend!  type (: 1 extend))
      (let loop ((l (if extend (: extend (type-interfaces type)) 
			(type-interfaces type))))
	(if (pair? l)
	    (or (verify-cycle! (car l) src) (loop (cdr l)))
	    (begin
	      (set-type-extend! type extend)
	      #f))))
     (else ;; It has no extension: Give up.
      #f))))

;; Called by second walk. It adds the member of a type to its table.
;; A member is a field or a block of code.
;;
;; I: member, a member of a class declaration. This can be a n-method,
;;	      n-fieldDecl, n-type, or n-block.  
;;    type, (s type).
;; O: change meaning of names when appropriate. Verifies modifiers
;;    of fieldVariableDeclaration.
;;
(define (add-member! type member)
  (let* ((table (type-table type)))
    (cond
     ((n-fieldDecl? member)
      (add-fields table member (type-name type)	(type-interface? type)))
     ;; This is an inner class.
     ((n-type? member)
      (error "TBF, add-names-in-field, inner class not implemented " member))
     ;; can it occurs in an interface? TBF
     ((n-initz? member)   
      (if (type-interface? type)
	  (sem-err #f (n-initz-src member) 
		   "A block initializer can only occur in a class, not an interface.")
	  (add-init-block member type)))
     (else
      (error "TBF, unrecognised declaration of a type member " member))
     )))

;; Analyse a method declaration appearing in an interface with contracts.
(define (ver-contractMethodInt mInterface T)
  #t
  )

;; Verify static and non static initializer block. Static initializer
;; blocks are presented in 8.5
;;
;; I: initz (s n-initz)
;;    T, (s type). The type where the init block occurs.
;; O: void
(define (add-init-block initz T)
  ;; TBF: 0 should be replaced by the cummulative of previous init blocks.
  ;; TBF: should verify that there is no abrupt completion of the code.
  (let* ((Ltable (make-Ltable '() (type-table T) 0 (type-table T)))
	 (Mtype  (if (n-initz-static initz)
		     (make-tyM "<clinit>" '(static) #f #f #f Ltable T 0 #f #f)
		     (make-tyM "<init>"   '() #f #f #f Ltable T 0 #f #f))))

    (ver-block  Ltable (n-initz-block initz) Mtype T #f #f '() #f #t #f)
    ))

;; This function is called in the second walk to handle method
;; declarations.
;;
;; A constructorDeclaration and methodDeclaration are very
;; similar: constructorDeclaration has no type, so no Dim either.
;; For more information consult (8.6).
;;
;; It does:
;;
;;      - Verify modifiers of the method.
;;      - Verify the presence or absence of method body according to 
;;        the modifiers.
;;      - Construct the type of the method: parameter list, result type, 
;;        modifiers, throws.
;;      - Verify that no duplicate names occur in the parameter list.
;;
;; Note: It is not yet possible to verify if overriding is correct
;; since not all types have been processed yet, and it has not be
;; verified if the type hierarchies are without cycles.
;;
;; I: Ttable, (s Ttable). This is the table for the class or interface.
;;    node, (s n-method)
;;    T, (s type). The type where the method occurs. 
;; O: void.
;;
(define (add-method-declaration node T)
  (match node (($ n-method mdfs rType mName parmList dim throws block src mt prepost)

    ;; If it is a constructor the rType is modified to 'void and
    ;; the name is modified to "<init>". In all cases a rType
    ;; #f is modified to 'void by create-tyM.
    (let* ((Ttable     (type-table T))
	   (Ltable     (make-Ltable '() #f 0 Ttable))
	   (className  (list-last (type-name T)))
	   ;; Three conditions to be a constructor:
	   ;; defined in a class, no result type, and the method name
	   ;; is identical to the class name. (8.4.3.2)
	   (storedmName (if (and (string=? mName className) (not rType)
				 (not (type-interface? T)))
			    "<init>"  mName))
	   ;; In interfaces, methods are abstract and public.(8.4.5 & 9.4).
	   (mdfs2  (if (type-interface? T) (union '(public abstract) mdfs) mdfs))
	   (Mtype (create-tyM storedmName mdfs2 rType parmList throws Ltable T prepost src))
	   (method-mts (lupL Ttable storedmName 'method)))

      (ver-basic-mdfs mdfs2 src)      
      ;; 8.4.5
      (if (and (n-block? block) (or (member 'abstract mdfs2) (member 'native mdfs2)))
	  (sem-err #f src "An abstract or native method must not have a body."))
      (if (and (n-block? block) (eqv? (n-block-declStmts block) 'none)
	       (not (or (member 'abstract mdfs2) (member 'native mdfs2))))
	  (sem-err #f src "A non-abstract or non-native method must have a body."))
      ;; 8.4.3.2 & 9.4
      (if (included? '(abstract static) mdfs2)
	  (sem-err #f src "A method cannot be static and abstract: "mName))
      ;; 8.4.3.3 & 9.4
      (if (included? '(abstract final) mdfs2)
	  (sem-err #f src "A method cannot be final and abstract: " mName))
      ;; 8.4.3.4 & 9.4
      (if (included? '(abstract native) mdfs2)
	  (sem-err #f src "A method cannot be native and abstract: " mName))
      ;; 8.6.3
      (if (and (string=? storedmName "<init>")
	       (intersect? '(abstract static final native synchronized) mdfs2))
	  (sem-err #f src "A constructor cannot be abstract, static, final, native, or synchronized."))
      (if (and (not rType) (not (string=? storedmName "<init>")))
	  ;; It has no type but it does not have a name equal to the
	  ;; class name, or it is in an interface. It is a badly
	  ;; formed method or constructor.
	  (sem-err #f src "Method "mName" has no return type."))

      ;; Two methods (or constructors) may not have the same signature.
      (let* ((methods (overHides Mtype method-mts)))
	(if (pair? methods)
	    (sem-err #f src (or (and (string=? storedmName "<init>")"Constructor ")
				"Method ")
		     (T->s Mtype)" is re-defined.")))
		
      ;; Add the method to the table and insert it into the node.
      ;; This must always be done to have a value for the mt field.
      (set-n-method-mt!
       node (add-name-table! Ttable storedmName Mtype 'method mdfs2 src))
      ))))

;; Verifies the rules of multiple inherited methods (8.4.6.4). TBF
;;
(define (ver-methodsInheriting types)
  #t)

;; Return inherited methods considering access modifiers, overriding,
;; and hiding, AND satisfying predicate equ?, from type T and its up
;; hierarchy. Access control is discussed in 6.6. This is very similar
;; to field access but slightly different for constructor.
;;
;; I: T, (s type). TBF should include type-array
;;    local?, #t means consider local methods as being inherited.
;;    equ?, predicate of one argument (s tyM) indicating which methods
;;          to keep.
;;    fromType (s type), the class type where the method call occurs.
;;    preT (u #f #t (s type)), the type of the prefix, if one exists.
;;            #f, no prefix, #t means it is a new operation.
;;    init?, #t => returns local <init> methods.
;; O: [(s mt)]. 
(define (lupT-m T equ? local? fromT preT init?)

  (define (theLocals)
    (filter (c* equ? mt-type) (rem-mName "<clinit>" (type->local-method-types T))))

  ;; Returns methods of mt2s that are neither overriden nor hidden by a
  ;; method of mt1s.
  (define (rem-overHides mt1s mt2s)
    (filter (lambda (x) 
	      (not (p-exists? (lambda (y) (method-sign=? (mt-type y) (mt-type x))) mt1s)))
	    mt2s))

  (define (ver-access-method mt) (ver-access mt T fromT preT))
  (cond
   ((tyA? T)	  ;; TBF test this carefully.
    (let* ((object     '("java" "lang" "Object"))
	   (typeObject (res-type! object T src)))
      ;; Not fully correct for method clone. TBF
      (if (typeUndef? typeObject)
	  (sem-err '() src "Cannot access java.lang.Object.")
	  (lupT-m typeObject equ? #t fromT preT #f))))
   ((type? T)
    (let* ((mt1s (or (and local? (theLocals)) '()))
	   (mt2s (or (and init? mt1s) (rem-mName "<init>" mt1s)))
	   (mt3s (filter ver-access-method mt2s))
	   (ext  (begin
		   (resolve-extend-implements! T)
		   (if (type-extend T)
		       (: (type-extend T) (type-interfaces T))
		       (type-interfaces T))))
	   (mt4s  (list-append 
		   (map (lambda (x) (lupT-m x equ? #t fromT preT #f)) ext)))
	   (mt5s  (append mt3s (rem-overHides mt2s mt4s))))

      mt5s))))

;; I: ms, [(s mt)]
;;    mName, str.
(define (rem-mName mName ms)
  (filter (lambda (x) (not (string=? (mt-id x) mName))) ms))

;; Return all methods overriden or hidden by method m1.
;; I: parm-types, [Type]
;;    m1, (s tyM)
;; O: [(s mt)]
(define (overHides m1 mts)
  (filter (lambda (mt) (method-sign=? m1 (mt-type mt))) mts))

;; Verify that overriding and hiding methods are correctly
;; done. (8.4.6) All methods declaration verified are extracted from
;; the parameter types.
;;
;; I: types [(s type)]. They are all types defined in the source.
;; O: void. Write out error messages, if errors are detected.
;;
(define (ver-methods-overHiding types)
  (for-each (lambda (T)	(for-each (c.2 ver-method-over T)
				  (map mt-type 
				       (rem-mName "<init>" 
						  (type->local-method-types T)))))
	    types))

;;
;; I: Mtype, (s tyM)
;;    Type, (s type)
;; O: void.
(define (ver-method-over Mtype Type)

  (define (same-sign) (lambda (method) (method-sign=? Mtype method)))

  (let* ((src          (tyM-src Mtype))
	 (returnMtype  (tyM-rType Mtype))
	 ;; These are all methods overriden or hidden by the method.
	 (overHideMs   (lupT-m Type (same-sign) #f Type #f #f)))

    (for-each
     (lambda (mt)
       ;; Verifies that throws do match (8.4.4). TBF

       ;; Verify return type.
       (if (not (T==? (tyM-rType (mt-type mt)) returnMtype))
	   (sem-err #f
	    src "Method "(T->s Mtype)" overrides, in class "
	    (T->s (tyM-classType (mt-type mt))) ", with a different return type."
	    #\newline
	    "Return type of method: " (T->s returnMtype) #\newline
	    "Return type of overriden method: " 
	    (T->s (tyM-rType (mt-type m)))))
       ;; 8.4.6.1 
       (if (and (var-static? mt) (instance-method? Mtype))
	   (sem-err #f src
           "Instance method "(T->s Mtype)" overrides a static method in class "
	    (T->s (tyM-classType (mt-type mt)))"."))
       ;; 8.4.6.2 
       (if (and (not (var-static? mt)) (static-method? Mtype))
	   (sem-err #f src
	    "Static method "(T->s Mtype)" overrides an instance method in class "
	    (T->s (tyM-classType m))"."))
       ;; 8.4.6.3
       (if (less-accessible? (tyM-mdfs Mtype) (mt-mdfs mt))
	   (sem-err #f src
	    "Method "(T->s Mtype)" overrides a more accessible method from"
	    " class "(T->s (tyM-classType (mt-type mt)))".")))
     overHideMs)))

;; See 8.4.6.3 for a definition of less accessible.
;;
;; O: #t iff modifiers mdfs1 describes less accessibility than mdfs2.
(define (less-accessible? mdfs1 mdfs2)
  ;; In the first case there is a test that `private' is not in mdfs2.
  ;; Actually this should not occur when this function is called
  ;; to verify method overriding and hiding.
  (or (and (member 'private mdfs1)      (not (member 'private mdfs2)))
      (and (not (member 'public mdfs1)) (member 'public mdfs2))
      (and (not (or (member 'protected mdfs1) (member 'public mdfs1))) 
	   (member 'protected mdfs2))))

;; TBF check that the modifier static is always added to declaration
;; when it comes from an interface.
;;
;; I: M, (s tyM)
(define (static-method? M) (if (member 'static (tyM-mdfs M)) #t #f))
(define (instance-method? M)  (not (static-method? M)))

;; I: mName, str. The method name.
;;    mdfs, [sym]. The modifiers on the method or constructor declaration.
;;    rType, (u #f Type). #f means it is a constructor.
;;    parmList, [n-parm]
;;    throws, [n-qName]
;;    Ltable, (s Ltable)
;;    type, (s type). The type where the declaration occurs.
;;    prepost (s n-prepost).
;; O: (s tyM)
;;
(define (create-tyM mName mdfs rType parmList throws Ltable type prepost src)

  ;; I: parm, (s n-parm)
  ;;    n, the next available position on the stack for this parameter.
  ;; O: Type
  ;;
  (define (add-parm! parm parm-type n)
    (match parm (($ n-parm typeSpec declarator src)
      (let* ((id      (n-declaratorName-id declarator))
	     (mdfs    (if (n-typeSpecifier-final typeSpec) '(final) '()))
	     (mt      (add-name-table! Ltable id parm-type 'local mdfs src))
	     (access  (make-varAccess n (T->jvmType parm-type) #f #f #f)))
	(set-mt-access! mt access)
	parm-type))))

  (define (ver-repetition ids parmList)
    (if (pair? parmList)
	(match (car parmList) (($ n-parm type declarator src)    
	  (let* ((id   (n-declaratorName-id  declarator))
		 (src2 (n-declaratorName-src declarator)))
	    ;; A parameter name cannot be repeated
	    (if (member id ids)
		(sem-err #f src2 "There are more than one parameter named " 
			 id" for method "mName"."))
	    (ver-repetition (: id ids) (cdr parmList)))
	  ))))

  ;; Verifies repetition of parameter names.
  (ver-repetition '() parmList)

  (let* ((firstLocal   (if (member 'static mdfs) 0 1))
	 (parms-a-type (map (lambda (parm)
			      (match parm (($ n-parm typeSpec declarator src)
				(create-tyA 
				 (+ (n-declaratorName-dim declarator)
				    (n-typeSpecifier-dim typeSpec))
				 (n-typeSpecifier-typeName typeSpec)
				 (get-Gtable Ltable)
				 src))))
			    parmList))
	 (nbWords     (map type->nbWords parms-a-type))
	 (positions   (list-scan2 + firstLocal nbWords))
	 (nbWordsUsed (foldl + firstLocal nbWords))
	 (parms-type  (map add-parm! parmList parms-a-type positions))
	 (prepost?    (and (n-prepost? prepost) 'contract))
	 ;; TBF: where is the dim of the function as in declaratorName?
	 ;; When rType is #f this is a constructor.
	 (rType2 (or (and (not rType) 'void)
		     (create-tyA
		      (n-typeSpecifier-dim rType)
		      (n-typeSpecifier-typeName rType)
		      (get-Gtable Ltable)
		      (n-typeSpecifier-src rType)))))
    (set-Ltable-nbWords! Ltable nbWordsUsed)
    (make-tyM mName mdfs rType2 parms-type throws Ltable type nbWordsUsed prepost? src)
    ))


;; Add the names of a field variable declaration, of a class or
;; interface.  Called during second walk.
;;
;; I: table, (s Ttable)
;;            The table of field names and methods of this class or
;;            interface.
;;    field, (s n-fieldDecl)
;;    typeName, str. The name of the type where this field is defined.
;;    interface?, #t iff the type is an interface.
;; O: void.
;;
(define (add-fields table field typeName interface?)
  (match field (($ n-fieldDecl mdfs type variables src)
    ;; In an interface fields are implicitly public, static, and
    ;; final. see 9.3
    (let* ((mdfs2 (if interface? (union '(public static final)  mdfs) mdfs)))

      ;; I: var, (u (s n-varInit) (s n-declaratorName))
      (define (add-name-var! var)
	(let* ((final? (member 'final mdfs2))
	       (declaratorName (if (n-varInit? var)
				   (n-varInit-declaratorName var)
				   var))
	       (id   (n-declaratorName-id declaratorName))
	       (src  (n-declaratorName-src declaratorName))
	       (dim  (n-declaratorName-dim declaratorName))
	       (var2 (lupL table id 'field)))
	  
	  (if var2
	      (sem-err #f src "Field "id" is re-declared. It had type: "
		       (T->s (mt-type var2)))) 
	  (let* ((type-var (create-tyA 
			    (+ dim (n-typeSpecifier-dim type))
			    ;; If the field is final and the type is
			    ;; one of char, byte, or short, it is
			    ;; promoted to int.
			    (if final?
				(unary-prom (n-typeSpecifier-typeName type))
				(n-typeSpecifier-typeName type))
			    (get-Gtable table)  src))
		 ;; Add this name to the table. If already defined, it
		 ;; does change its type.
		 (mt      (add-name-table! table id type-var 'field mdfs2 src))
		 (access  (make-varAccess #f (T->jvmPool type-var 'noClass)
					  (append (get-pack-name table) (list typeName))
					  id (var-static? mt))))

	    ;; If the field is final it must have an initializer.
	    ;; (Not true under Java 1.1).
	    ;; We cannot process all initializers now since we have not finished
	    ;; the second walk.

	    (if (and final? (not (n-varInit? var)))
		(sem-err #f src "Final field "id" must have an initializer."))
	    
	    ;; Adding access information to the field.
	    (if (n-varInit? var) (set-n-varInit-access! var access))
	    (set-mt-access! mt access)
	    )))
    
      (if (not (included? mdfs2 '(public protected private final 
					 static transient volatile)))
	  (sem-err #f src "Unknown modifier."))
      
      (ver-basic-mdfs mdfs2 src)
      
      ;; Fields cannot be declared final and volatile. (8.3.1.4)
      (if (included? '(final volatile) mdfs2)
	  (sem-err #f src "A field cannot be declared final and volatile."))
      
      ;; Add to the table all field names occuring on this declaration.
      (for-each add-name-var! variables)))))
      

(define (ver-basic-mdfs mdfs src)
  (if (duplicates? mdfs)
      (sem-err #f src "Modifiers should not be repeated: "mdfs"."))
  ;; A modifier cannot be repeated (8.1.2)
  (if (> (length (intersection mdfs '(protected private public)))  1)
      (sem-err #f src "Conflicting access modifiers: " 
	       (intersection mdfs '(protected private public)))))

;; End of second pass
;; =======================================================================


;; =======================================================================
;; Third pass, this is the major pass where all statements are analyze.
;;
;; In this pass all method bodies are traversed.  All initializers of
;; fields are also checked, and computed if they are constant
;; expressions.
;;

;; I: table, (s Gtable) the table of the types.
;;    p, (s n-program)
;; O: void
;;
(define (third-pass table p)

  ;;
  (define (ver-fieldInitializer field Ctype)
    (let* ((vars   (n-fieldDecl-vars field))
	   (Ttable (type-table Ctype)))
      (for-each 
       (lambda (var)
	 (if (n-varInit? var)
	     (let* ((id (n-declaratorName-id (n-varInit-declaratorName var)))
		    (mt (lupL Ttable id 'field)))
	       (ver-initializer! Ttable var mt Ctype #f))))
       vars)))

  ;; 
  (define (ver-members-declarations n-type)
    (match n-type (($ n-type header members)
      (match header (($ n-header mdfs classWord id extends implements)
	(let* ((mt     (lupL table id 'type))
	       ;; Type id has been recorded by the first pass
	       (Ctype  (mt-type mt)))

	  ;; TBF: If the class contains at least one method without a
	  ;; body, it must be declared abstract. This method may be
	  ;; inherited. Yet to be checked.

	  ;; Verify all methods and all fields.
	  (for-each 
	   (lambda (m)
	     (cond
	      ((n-fieldDecl? m) (ver-fieldInitializer m Ctype))
	      ((n-method? m)    (ver-method m id classWord Ctype))
	      ((n-initz? m) #t) ; verified in second pass
	      (else (error "TBF, ver-members-declarations " m))))
	   members)))))))

  (match p (($ n-program package imports types)
    (for-each ver-members-declarations types)
    )))

;; I: Ttable, (s Ttable). Table of a class or interface.
;;    node, (s n-method)
;;    classIntName, str. The name of the class or interface.
;;    classWord, (u 'interface 'class).
;;    type, (s type)
;; O: void.
;;    The insertion of the method type and the verification that it is
;;    not multiply defined has been done in the second walk. 
;;
(define (ver-method node classIntName classWord type)
  (match node (($ n-method mdfs rType mName parmList dim throws block src mt prepost)
    (let* ((Mtype  (mt-type mt))
	   (Ttable (type-table type))
	   (Ltable (tyM-table Mtype)))
      ;; 1) Insert super constructor call, if not present in constructor.
      (if (and (method-constructor? Mtype) (n-block? block) 
	       (not (first-superCall? block)))
	  (let* ((stmts (and block (n-block-declStmts block)))
		 (call-super (make-n-call (make-n-specialName 'super src) '() src #f)))
	    ;; Add super() at constructor body.
	    (if (not block)
		(set-n-method-block! node (make-n-block (list call-super) src))
		(set-n-block-declStmts! block (: call-super stmts)))))

      ;; 2) Contract Java pre and post-condition.
      (if (n-prepost? prepost)
	  (begin
	    (if (n-prepost-pre prepost)
		(ver-expr Ltable (n-prepost-pre prepost) Mtype type #f))
	    (if (n-prepost-post prepost) 
		(ver-expr Ltable (n-prepost-post prepost) Mtype type #t))))
      
      ;; 3) Process the method body 
      (ver-method-body Ltable block Mtype type)
      (if (n-block? block) (set-mt-value! mt block))

      ;; 4) Add methods if there is a contract.
      (if (n-prepost? prepost)
	  (let* ((mNameC      (++ mName "<c>"))
		 (mNamePostB  (++ mName "_post_blame<c>"))
		 (mNamePreB   (++ mName "_pre_blame<c>"))
		 (mdfsW       (tyM-mdfs Mtype))
		 (rType       (tyM-rtype Mtype))
		 (LtableW     (create-LtableW Ltable mNameC rType type src))
		 (MtypeW      (make-tyM mNameC mdfsW rtype (tyM-parm-types Mtype)
					(tyM-throws Mtype) LtableW type 0 
					'wrapper src))
		 ;; wrapper
		 (Wblock  (method-wrapper-ast mName mNameC mNamePreB mNamePostB 
					      prepost parmList Mtype src))
	         (Wmt     (add-name-table! Ttable mNameC MtypeW 'method mdfsW src)))
	    (add-methods-pre-post-blame Ttable mNamePreB mNamePostB type src)
	    (ver-method-body LtableW Wblock Mtype type)
	    (set-mt-value! Wmt Wblock)
	    ))
      ))))

;; O: (s Ltable)
(define (create-LtableW Ltable varName rType type src)
  (let* ((LtableW (make-Ltable '() Ltable 2 type))
	 (mt      (add-name-table! LtableW varName rType 'local '() src))
	 (access  (make-varAccess 1 (T->jvmType rType) #f #f #f)))
    (trace! "adding " varName)
    (set-mt-access! mt access)
    LtableW))

;; O: void
(define (add-methods-pre-post-blame Ttable mNamePreB mNamePostB type src)
  (let* ((mdfs      '(private static))
	 (Ltable2    (make-Ltable '() #f 0 Ttable))
	 (MtypePreB  (make-tyM mNamePreB mdfs 'void '() '() Ltable2 type 0 'blame src))
	 (MtypePostB (make-tyM mNamePostB mdfs 'void '() '() Ltable2 type 0 'blame src))
	 (PreBmt     (add-name-table! Ttable mNamePreB MtypePreB 'method mdfs src))
	 (PostBmt    (add-name-table! Ttable mNamePostB MtypePostB 'method mdfs src))
	 (PreBlameBlock
	  (make-n-block
	   `(,(make-n-call (make-n-qName '("System" "out" "println") src #f)
			   `(,(make-n-literal '("java" "lang" "String") 
					      "Pre-condition Blame" src))
			   src #f))
	   src))
	 (PostBlameBlock
	  (make-n-block
	   `(,(make-n-call (make-n-qName '("System" "out" "println") src #f)
			   `(,(make-n-literal '("java" "lang" "String") 
					      "Post-condition Blame" src))
			   src #f))
	   src)))

    (ver-method-body Ltable2 PreBlameBlock MtypePreB type)
    (ver-method-body Ltable2 PostBlameBlock MtypePostB type)
    (set-mt-value! PreBmt PreBlameBlock)
    (set-mt-value! PostBmt PostBlameBlock)
    ))

;; I: mName, str. The name of the original method.
;;    mNameW, str. The name of the wrapper method.
;;    Mtype, (s tyM). The type of the method having a contract.
;;    parmList, [(s n-parm)]
;; O: (s n-block)
(define (method-wrapper-ast mName mNameW mNamePreB mNamePostB prepost parmList Mtype src)

  ;; Construct the argument list to call original method.
  (define (parmList->argList)
    (map (lambda (x) 
	   (make-n-qName (n-declaratorName-id (n-parm-declaratorName x)) src #f))
	 parmList))
  
  (let* ((pre             (n-prepost-pre prepost))
	 (post            (n-prepost-post prepost))
	 (qmNameW         (make-n-qName mNameW src #f))
	 (qmName          (make-n-qName mName src #f))
	 (argList         (parmList->argList))
	 (throw-pre       (make-n-throw (make-n-alloc #f
					 (make-n-classAlloc 
					  (make-n-qName "RuntimeException" src #f) 
					  (list (make-n-literal 
						 '("java" "lang" "String")
						 "Pre-condition failed." 
						 src))
					  #f src #f)
					 src) src))
	 (throw-post      (make-n-throw (make-n-alloc #f
					 (make-n-classAlloc 
					  (make-n-qName "RuntimeException" src #f) 
					  (list (make-n-literal 
						 '("java" "lang" "String")
						 "Post-condition failed." 
						 src))
					  #f src #f)
					 src) src))
	 (rVal?           (not (T==? (tyM-rtype Mtype) 'void)))
	 (rexpr           #f))

    (make-n-block 
     `(;; Verify pre-condition.
       ,@(if pre
	     (list (make-n-if (make-n-unary '! pre src) throw-pre #f src)) '())
       ;; Call original method.
       ,(if rVal?
	    (make-n-assignment qmNameW '= (make-n-call qmName argList src Mtype) src)
	    (make-n-call qmName argList src Mtype))
       ;; Verify post-condition.
       ,@(if post
	     (list (make-n-if (make-n-unary '! post src) throw-post #f src)) 
	     '())
       ,@(if rVal? (list (make-n-return qmNameW src #f)) '()))
     src)))

;; I: block, (s n-block)
;; O: #t, iff Mtype is a constructor and the first node of the its
;;        body is a super() call.
(define (first-superCall? block)
  (let* ((stmts (and block (n-block-declStmts block)))
	 (first (and (pair? stmts) (car stmts)))
	 (call  (and (n-call? first) (n-call-method first))))
    (and (n-specialName? call) (eqv? 'super (n-specialName-name call)))))

;; This method makes sense only after the second walk, or at least
;; after calling det-names-in-method-header where the name of constructor
;; is replaced by <init>.
;;
;; I: Mtype, (s tyM)
;; O: #t iff the method is a valid constructor method.
;;
(define (method-constructor? Mtype) (string=? "<init>" (tyM-mName Mtype)))

;; I: table, (s Ltable).
;;    block, (s n-block)
;;    Mtype,  (s tyM)
;; O: boolean.
(define (ver-method-body table block Mtype Ctype)
  (ver-block table block Mtype Ctype #f #f '() (method-constructor? Mtype) #f #f))

;; I: table, (s Ltable)
;;    block, (s n-block)
;;    Mtype, (s tyM)
;;    lbls, [(str stmt1 stmt2)]
;;    cBody?, #t iff this block is the body of a constructor.
;;    init?, #t iff it is an initializer block.
;; O: (throws cmpl? brk?)
(define (ver-block table block Mtype Ctype brk? cnt? lbls cBody? init? iRet)

  (define (ver-stmts newTable stmts)
    (let loop ((l stmts) (throws '()) (cmpl? #t) (exists-brk? #f) (all-cmpl? #t))
      (if (pair? l)
	  (let* ((cmpls (ver-stmt newTable (car l) Mtype Ctype brk? cnt? lbls cmpl? init? iRet)))
	    (loop (cdr l) (append (car cmpls) throws) (cadr cmpls) 
		  (or exists-brk? (caddr cmpls))
		  (and all-cmpl? (cadr cmpls))))
	  `(,throws ,all-cmpl? ,exists-brk?))))
  
  (if (n-block? block)
      (if (n-block-declStmts block)
	  (let* ((stmts    (n-block-declStmts block))
		 (newTable (createNestLtable table)))
	    ;; A super() call is allowed only at the beginning of a constructor body.
	    (if (and cBody? (first-superCall?  block))
		(begin
		  (ver-call table (car stmts) Mtype Ctype #t #f)
		  (ver-stmts newTable (cdr stmts)))
		(ver-stmts newTable stmts)))
	  '(() #t #f))
      '(() #t #f)))

(define (createNestLtable Ltable)
  (make-Ltable '() Ltable (Ltable-nbwords Ltable) (Ltable-typeTable Ltable)))

;; I: table, (s Ltable).
;;    stmt, Statement.
;;    brk?, #t iff break statements (without a label) are allowed.
;;    cnt?, #t iff continue statements (without a label) are allowed.
;;    lbls, 
;;    Stmts, [Inst]. The rest of the statements to analyse.
;;    rch?, #t iff stmt can be reached or an error msg has been reported that
;;             it is not reachable.
;;    init?, #t iff stmt is in an initializer block. Used to reject return.
;;    iRet, int. Local variable position to store a return expression 
;;               value occuring in a try.
;; O: ([types thrown] boolean boolean), 
;;    1) The types that are thrown from reachable statements and expressions.
;;    2) #t iff the statement completes normally.
;;    3) #t iff there is a reachable break without label out of that statement.
(define (ver-stmt table stmt Mtype Ctype brk? cnt? lbls rch? init? iRet)
  (if (and (not (n-emptyStatement? stmt)) (not rch?)) 
      (sem-err #f (n-stmt-src stmt) "Statement is unreachable."))
  (if (and init? (n-return? stmt)) 
      (sem-err #f (n-return-src stmt) "No return statement allowed in an initializer block."))
  (cond
   ((n-emptyStatement? stmt) `(() ,rch? #f))
   ((n-label? stmt)    (ver-label    table stmt Mtype Ctype brk? cnt? lbls init?))
   ((n-localVarDecl? stmt)(ver-localVarDecl! table stmt Mtype Ctype))
   ((n-if?       stmt) (ver-if       table stmt Mtype Ctype brk? cnt? lbls init? iRet))
   ((n-switch?   stmt) (ver-switch   table stmt Mtype Ctype cnt? lbls init? iRet))
   ((n-while?    stmt) (ver-while    table stmt Mtype Ctype lbls init? iRet))
   ((n-do?       stmt) (ver-do       table stmt Mtype Ctype lbls init? iRet))
   ((n-for?      stmt) (ver-for      table stmt Mtype Ctype lbls init? iRet))
   ((n-break?    stmt) (ver-break    table stmt brk? lbls))
   ((n-continue? stmt) (ver-continue table stmt cnt? lbls))
   ((n-return?   stmt) (ver-return   table stmt Mtype Ctype iRet))
   ((n-throw?    stmt) (ver-throw    table stmt Mtype Ctype))
   ((n-syn?      stmt) (ver-syn      table stmt Mtype Ctype brk? cnt? lbls init? iRet))
   ((n-try?      stmt) (ver-try      table stmt Mtype Ctype brk? cnt? lbls init? iRet))
   ((n-block?    stmt) (ver-block    table stmt Mtype Ctype brk? cnt? lbls #f init? iRet))
   ((n-type? stmt)  (sem-error '(() #t #t) src "Inner type not yet implemented."))
   ((n-case? stmt) ; we are outside a switch here.
    (sem-err '(() #t #t) (n-case-src stmt) "A case or default must be in a switch."))
   (else ;; it should be an expression-statement
    (let* ((e-r (ver-expr table stmt Mtype Ctype #f)))
      (if (not (stmt-expression? stmt))
	  (sem-err '(() #t #f) (e-r-src e-r) "Not an expression-statement." stmt)
	  '(() #t #t)))
    )))

    
;; I: Ltable, (s Ltable)
;;    stmt, (s n-syn)
;; O: boolean, #t iff the synchronized completes normally
(define (ver-syn Ltable stmt Mtype Ctype brk? cnt? lbls init? iRet)
  (match stmt (($ n-syn expr statement access src)
    (let* ((e-r  (ver-expr Ltable expr Mtype Ctype #f))
	   (e-rT (e-r-t e-r)))
      (if (and (not (type-ref? e-rT)) (not (typeUndef? e-rT)))
	  (sem-err #f src "The synchronized expression's type must be a reference type. Its type is "(T->s e-rT)"."))
      ;; The synchronized value uses a JVM local variable.
      ;; There is also a local variable for the return address of the
      ;; short ``finally'' subroutine.
      (let* ((n      (Ltable-nbWords Ltable)))
	(set-n-syn-iLocals! stmt (+ n 1))
	(set-Ltable-nbwords! Ltable (+ n 2))
	;; Two locals are used: one for the synchronized value and one
	;; for the subroutine return address. TBF: the second one
	;; should be conditionally added.
	(set-tyM-nbWords! Mtype (max (tyM-nbWords Mtype) (+ n 2)))
	(let* ((cmpl? (ver-stmt Ltable statement Mtype Ctype brk? cnt? lbls #t init? iRet)))
	  (set-tyM-nbWords! Mtype (max (tyM-nbWords Mtype) (- n 2)))
	  cmpl?)
	)))))

;; O: (throw-type #f #f). This stmt does not complete normally.
(define (ver-throw table stmt Mtype Ctype)
  (match stmt (($ n-throw expr src)
   (let* ((e-r (ver-expr table expr Mtype Ctype #f)))
     ;; TBF, verifies that the expression type is a subtype of throwable.
     (set-n-throw-expr! stmt e-r)
     `((,(e-r-t e-r)) #f #f)))))

;; The try statement (14.18)
;; I: table, (s Ltable)
;; O: (throw-type )
(define (ver-try table stmt Mtype Ctype brk? cnt? lbls init? iRet)
  (match stmt (($ n-try block catches finally src)
   (let* ((niRet  (or iRet (and finally (+ (if iRet 1 2) (Ltable-nbWords table)))))
	  (cmpl1? (ver-block table block Mtype Ctype brk? cnt? lbls #f init? niRet))
	  (cmpl2? (exists? (map (lambda (catch)  
				  (ver-catch table catch Mtype Ctype brk? cnt? lbls init? iRet))
				catches))))
     (if (not finally) `(() ,(or cmpl1? compl2?) #f)
	 (begin
	   ;; Three local variables are needed: one for the exception
	   ;; object of the short block calling the finally block,
	   ;; another one for the local variable containing the return
	   ;; address and a third one if a return with an expression occurs
	   ;; in the block.
	   (set-n-try-iRetAdr! stmt (Ltable-nbWords table))
	   (set-Ltable-nbWords! table (+ (if iRet 2 3) (Ltable-nbWords table)))
	   (let* ((cmpl? (ver-block table finally Mtype Ctype brk? cnt? lbls #f init? 
				    niRet)))
	     `(() ,(and cmpl? (or cmpl1? cmpl2?)) #f))))))))

;; Catch handler (14.18).
;; 
;; I: catch, (s n-catching)
;; O: boolean, #t iff the catch block completes normally
(define (ver-catch table catch Mtype Ctype brk? cnt? lbls init? iRet)
  (match catch (($ n-catching catchHeader block src)
    (let* ((src2       (n-catch-src  catchHeader))
	   (typeSpec   (n-catch-type catchHeader))
	   (typeId     (create-tyA (n-typeSpecifier-dim typeSpec)
				   (n-typeSpecifier-typeName typeSpec)
				   (get-Gtable table) (n-typeSpecifier-src typeSpec)))
	   (id         (n-catch-id   catchHeader))
	   (tableCatch (createNestLtable table)))

      (set-n-catch-type! catchHeader typeId)
      (add-catch-parameter! tableCatch id typeId catchHeader Mtype src2)
      ;; Verify the catch block itself with the new table.
      (ver-block tableCatch block Mtype Ctype brk? cnt? lbls #f init? iRet)))))


(define (add-catch-parameter! table id typeId catchHeader Mtype src2)
  (if (lupL-up table id 'non-method)
      (sem-err #f src2 "Local variable "id" multiply defined."))
  
  ;; The type of a catch parameter must be the class Throwable or
  ;; a subclass of it.  
  (if (and
       (not (typeUndef? typeId))
       (not (or (subClass? typeId '("java" "lang" "Throwable") (get-Gtable table))
		(T==? typeId '("java" "lang" "Throwable")))))
      (sem-err #f src "The exception type "(T->s typeId)
	       " is not a class or a subclass of java.lang.Throwable."))
  ;; Add the exception parameter as a local variable.
  ;; The scope of the parameter is the catch block.
  (let* ((mt     (add-name-table! table id typeId 'local '() src2))
	 (n      (+ 1 (Ltable-nbWords table)))
	 (access (make-varAccess n (T->jvmType typeId) #f #f #f)))
    (set-n-catch-access! catchHeader access)
    (set-mt-access! mt access)
    (set-Ltable-nbwords! table n)
    (set-tyM-nbWords! Mtype (max (tyM-nbWords Mtype) (Ltable-nbWords table)))
    ))


;; I: lbls, [(str stmt1 stmt2 brkTo?)]. 
;;          The stmt1 is the statement following this label stmt2.
;;          brkTo? == #t => There is a reachable break to that label.
;; O: 
(define (ver-label table stmt Mtype Ctype brk? cnt? lbls init? iRet)
  (let* ((id     (n-qName-name (n-label-qName stmt)))
	 (stmt2  (n-label-stmt stmt))
	 (marker (list id stmt2 stmt #f))
	 (lbls2  (: marker lbls))
	 (src    (n-label-src stmt))
	 (redef? (:~ id lbls)))
    (if redef? (sem-err #t src "Nested label "id" multiply defined."))
    ;; Analyze the next instruction with the new label.
    (let* ((cmpl (ver-stmt table stmt2 Mtype Ctype brk? cnt? lbls2 #t init? iRet)))
      (if (fourth marker)
	  `(,(car cmpl) #t #f)
	  cmpl))))

;; I: stmt,  (s n-continue)
;;    cnt?, #t iff a continue without a label may appear here.
;;    lbls, [(id Statement (s n-label)]
;; O: 
(define (ver-continue table stmt cnt? lbls)

  (define (loopStmt? node) (or (n-while? node) (n-for? node) (n-do? node)))

  (let* ((label (n-continue-id   stmt))
	 (src   (n-continue-src  stmt)))
    (cond
     ((and (not cnt?) (not label))
      (sem-err #f src "continue is not allowed outside a loop."))
     ((not label) ;; Continue is allowed here, it is ok.
      #t)
     (label
      (let* ((lblstmt (:~ label lbls)))
	(cond
	 ((not lblstmt) (sem-err #f src "Label "label" is not in scope."))
	 ((not (loopStmt? (cadr lblstmt)))
	  (sem-err #f src "continue "label" is not on a loop statement."))
	 (else 	(set-n-continue-nodeLabel! stmt (caddr lblstmt)))
	 ))))
    `(() #f #f)))

;; There is a case where the next statement is reachable, when the
;; break has the label attached to it. TBF
;;
;; I: stmt,   (s n-break)
;;    brk?, #t iff breaks without label is allowed.
;;    lbls, [(id Statement (s n-label)]
;; O: 
(define (ver-break table stmt brk? lbls)
  (let* ((label (n-break-id   stmt))
	 (src   (n-break-src  stmt)))
    (cond
     ((and (not brk?) (not label))
      (sem-err #f src "Break without label incorrectly placed."))
     ((not label) ;; Breaks are allowed here, it is ok.
      '(() #f #t))
     (else
      (let* ((lblstmt (:~ label lbls)))
	(if (not lblstmt)
	    (sem-err '(() #t #f) src "Label "label" is not in scope.")
	    (begin
	      (set-n-break-nodeLabel! stmt (third lblstmt))
	      ;; Mark that there is a break to that label.
	      (set-car! (cdddr lblstmt) 'vide)
	      '(() #f #f))))))))

;; 14.9
;; O: 
(define (ver-switch table stmt Mtype Ctype cnt? lbls init? iRet)
  (match stmt (($ n-switch expr block src)
    (let* ((e-r   (ver-expr table expr Mtype Ctype #f))
	   (type  (e-r-t e-r))
	   (pairs
	    (if (not (member type '(char byte short int)))
		(begin ; 14.9
 		  (sem-err #f src 
		   "Type expression on switch not char, byte, short, nor int." 
		   " The type of that expression is " (T->s type) ".")
		  ;; assume type int
		  (ver-blockSwitch table block Mtype 'int Ctype cnt? lbls init? iRet))
		(ver-blockSwitch table block Mtype type Ctype cnt? lbls init? iRet))))
      (set-n-switch-expr!  stmt e-r)
      (set-n-switch-casesStmts! stmt pairs)
      '(() #t #f)
      ))))

;; Syntaxically, the block of the switch statement is very
;; general. Here it is checked for conformity: no statement should
;; appear before at least one case statement occurs, default can only
;; appears once, etc. For a description of the semantic restrictions
;; see (14.9).
;;
;; I: table, (s Ltable)
;;    block, (s n-block) (this is a block of a switch statement)
;;    typeSel, (u 'int 'short 'byte 'char)
;;
;; O: association list (c . stmts)
;;
(define (ver-blockSwitch table block Mtype typeSel Ctype cnt? lbls init? iRet)
  ;; A new table since all declarations in the switch are local to it.
  (let* ((block-table (createNestLtable table))
	 (insts       (n-block-declStmts block)))
    ;; cases, list of (c . stmts) constants c met so far in the switch
    ;; with their corresponding statements.  stmts, list of statements
    ;; under the current case.  reach?, #t => iff the next stmt in l
    ;; is reachable
    (let loop  ((l insts) (cases '()) (stmts '())  (reach? #t))
      (if (pair? l)
	  (let* ((stmt (car l)))
	    (if (and (not (n-case? stmt)) (not reach?))
		(sem-err #f (n-stmt-src stmt) "Statement is unreachable."))
	    (cond
	     ((and (n-case? stmt) (not (eqv? (n-case-constExpr stmt) 'default)))
	      ;; case expr:
	      (let* ((e-r (ver-expr block-table (n-case-constExpr stmt) Mtype Ctype #f))
		     (valuep (e-r-v e-r))
		     (value  (if (char? valuep) (char->integer valuep) valuep))
		     (src    (n-case-src stmt)))
		(set-n-case-constExpr! stmt e-r)

		(if value
		    (begin
		      (if (:~ value cases)
			  (sem-err #f src  "The case value "value" is repeated."))
		      (if (not (type-=? typeSel (e-r-t e-r) value))
			  (sem-err #f src
			   "Case expression type incompatible."
			   "Type of the case expression: "(T->s (e-r-t e-r))
			   "Type of the selection expression: "(T->s typeSel)))
		      ;; Add all the past statements to the last case.
		      (if (and (not (null? stmts)) (not (null? cases)))
			  (set-cdr! (car cases) (reverse stmts)))
		      (loop (cdr l) (: (: value '()) cases) '() #t))
		    (begin
		      (sem-err #f src "Not a constant expression on case statement.")
		      (loop (cdr l) (: (: 'unknown '()) cases) stmts #t)))))
	     ((and (n-case? stmt) (eqv? (n-case-constExpr stmt) 'default))
	      ;; default:
	      (if (:~ 'default cases)
		  (sem-err #f (n-case-src stmt) 
			   "Repeating default statement in switch."))
	      ;; Add all the past statements to the last case.
	      (if (and (not (null? stmts)) (not (null? cases)))
		  (set-cdr! (car cases) (reverse stmts)))
	      (loop (cdr l) (: (: 'default '()) cases) '() #t))
	     ((n-localVarDecl?  stmt)
	      (ver-localVarDecl! block-table stmt Mtype Ctype)
	      ;; A local declaration.
	      (if (not (pair? cases))
		  (begin 
		    (sem-err #f (n-localVarDecl-src stmt) 
			     "No case or default before declaration in switch.")
		    (loop (cdr l) (: (: 'unknown '()) cases) (: stmt stmts) #t))
		  (loop (cdr l) cases (: stmt stmts) #t)))
	     (else ;; It is a statement
	      (ver-stmt block-table stmt Mtype Ctype #t cnt? lbls #t init? iRet)
	      (if (not (pair? cases))
		  (begin
		    (sem-err #f (n-stmt-src stmt) 
			     "No case or default before statement in switch.")
		    (loop (cdr l) (: (: 'unknown '()) cases) (: stmt stmts)
			  (not (n-break? stmt))))
		  (loop (cdr l) cases (: stmt stmts) (not (n-break? stmt)))))))
	  (begin
	    ;; Add all the last statements to the last case.
	    (if (and (not (null? stmts)) (not (null? cases)))
		(set-cdr! (car cases) (reverse stmts)))
	    ;; If there is no default case, add one at the end of the switch.
	    (if (not (:~ 'default cases))
		(reverse (: (: 'default '()) cases))
		(reverse cases)))
	  ))))

;; O: 
(define (ver-if Ltable stmt Mtype Ctype brk? cnt? lbls init? iRet)
  (match stmt (($ n-if expr Sthen Selse src)
    (let* ((e-r   (ver-expr Ltable expr Mtype Ctype #f))
	   (type  (e-r-t e-r)))
      (set-n-if-expr! stmt e-r)
      (verify-cond type src)
      (let* ((cmpl1 (ver-stmt Ltable Sthen Mtype Ctype brk? cnt? lbls #t init? iRet))
	     (cmpl2 (or (and Selse (ver-stmt Ltable Selse Mtype Ctype brk? cnt? lbls #t init? iRet))
			'(() #t #f))))
	`(,(append (car cmpl1) (car cmpl2)) #t ,(or (caddr cmpl1) (caddr cmpl2)))
	)))))

;; I: see ver-stmt
;; O: 
(define (ver-for table for Mtype Ctype lbls init? iRet)
  (let* ((newTable (createNestLtable table)))
    (match for (($ n-for init expr incr stmt src)
      (if (or (null? init) (pair? init))
	  ;; Verify init expressions. Since it is a list, these are
	  ;; expressions, not declarations.
	  (set-n-for-init!
	   for
	   (map (lambda (e) ; 14.12 and 14.7 
		  (if (not (stmt-expression? e))
		      (sem-err #f src 
		       "For init not a statement expression or local declaration."))
		  (ver-expr newTable e Mtype Ctype #f))
		init))
	  ;; It must be a (s n-localVarDecl)
	  (ver-localVarDecl! newTable init Mtype Ctype))
      
      ;; Verify condition.
      (let* ((e-r     (and expr (ver-expr newTable expr Mtype Ctype #f)))
	     (true?   (or (not e-r) (eqv? 'true (e-r-v e-r))))
	     (false?  (and e-r (eqv? 'false (e-r-v e-r)))))
	(set-n-for-expr! for e-r)
	(and expr (verify-cond (e-r-t e-r) src))
      
	;; Verify all incr expressions.
	(set-n-for-incr! for
	 (map (lambda (e) 
		(if (not (stmt-expression? e))
		    (sem-err #f src "Not a statement expression in incr."))
		(ver-expr newTable e Mtype Ctype #f)) 
	      incr))
	(let* ((v (ver-stmt newTable stmt Mtype Ctype #t #t lbls (not false?) init? iRet)))
	  `(,(car v) ,(or (not true?) (third v)) #f)))))))

;; O: 
(define (ver-do Ltable stmt Mtype Ctype lbls init? iRet)
  (match stmt (($ n-do stmt2 expr src)
    (let* ((e-r    (ver-expr Ltable expr Mtype Ctype #f))
	   (type   (e-r-t e-r))
	   (false? (eqv? 'false (e-r-v e-r)))
	   (true?  (eqv? 'true (e-r-v e-r))))
      (set-n-do-expr! stmt e-r)
      (verify-cond type src)
      (let* ((v (ver-stmt Ltable stmt2 Mtype Ctype #t #t lbls #t init? iRet))) 
	`(,(car v) ,(or (not true?) (third v)) #f)
	)))))

;; I: see ver-stmt
;; O: 
(define (ver-while Ltable while Mtype Ctype lbls init? iRet)
  (match  while (($ n-while expr stmt src)
    (let* ((e-r    (ver-expr Ltable expr Mtype Ctype #f))
	   (type   (e-r-t e-r))
	   (false? (eqv? 'false (e-r-v e-r)))
	   (true?  (eqv? 'true (e-r-v e-r))))
      (set-n-while-expr! while e-r)
      (verify-cond type src)
      (let* ((v (ver-stmt Ltable stmt Mtype Ctype #t #t lbls (not false?) init? iRet)))
	`(,(car v) ,(or (not true?) (third v)) #f))
      ))))

;; O: void
(define (verify-cond type src)
  (if (and (not (T==? 'boolean type)) (not (typeUndef? type)))
      (sem-err #f src "Conditional is not boolean type. Its type is "(T->s type)".")))

;; Check that the return type is compatible with method definition (14.15).
;; I:  return, (s n-return)
;;     Mtype, (s tyM)
;; O: '(() #f #f) => return never completes normally.
(define (ver-return Ltable return Mtype Ctype iRet)
  (match return (($ n-return expr src)
    (if expr
	(let* ((e-r     (ver-expr Ltable expr Mtype Ctype #f))
	       (tExpr   (e-r-t e-r))
	       (rType   (tyM-rType Mtype))
	       (rAccess (make-varAccess iRet (T->jvmType rType) #f #f #f)))
	  (set-n-return-expr! return e-r)
	  (set-n-return-access! return rAccess)
	  (and (not (type-=? rType tExpr (e-r-v e-r)))
	       (sem-err #f src "Incompatible return type."
			" Type of the return expression is " (T->s tExpr)"; " 
			"Type of method's result is " (T->s rType) ".")))
	;; There is no expression on return, therefore the method must
	;; be of type void or be a constructor.
	(if (and (tyM-rType Mtype) (not (eqv? 'void (tyM-rType Mtype))))
	    (sem-err #f src
		      "Return must specify a value, method's return type is not void.")
	    ))
    '(() #f #f)
    )))

;; The verification of an expression should verify, among other
;; things, if it is a constant expression.
;;
;; A constant expression is an expression having no assignment and for
;; which all variables are final and whose iniatilizers are constant
;; expressions; the operators instanceof, ++, and -- must also be absent;
;; no method calls can appear; casts must be to primitive types or String.
;; See (15.27) for more details.  
;;
;; I: Ltable, (s Ltable)
;;    expr, Expression
;;    Mtype, (s tyM)
;;    Ctype, (s type)
;;           It is a type-interface in the case of an initializer.
;;           The class type containing the method of this expression.
;; O: (s e-r) This structure allows to return a value if it is a
;;    constant expression (#f if not a constant expression).  In that
;;    case, src is a n-src and specify the point where it was
;;    recognised that it was not a constant expression.
;;         
(define (ver-expr Ltable expr Mtype Ctype postC?)
  (cond
   ((e-r? expr) expr) ;; it has been analysed. Useful for Contract Java.
   ((n-assignment?  expr) (ver-assignment  Ltable expr Mtype Ctype postC?))
   ((n-literal?     expr) (ver-literal     Ltable expr Mtype Ctype))
   ((n-boollit?     expr) (ver-boollit            expr            ))
   ((n-qName?       expr) (ver-qName       Ltable expr Mtype Ctype))
   ((n-op?          expr) (ver-op          Ltable expr Mtype Ctype postC?))
   ((n-instanceof?  expr) (ver-instanceof  Ltable expr Mtype Ctype postC?))
   ((n-postExpr?    expr) (ver-postExpr    Ltable expr Mtype Ctype postC?))
   ((n-unary?       expr) (ver-unary       Ltable expr Mtype Ctype postC?))
   ((n-specialName? expr) (ver-specialName Ltable expr Mtype Ctype))
   ((n-alloc?       expr) (ver-alloc       Ltable expr Mtype Ctype postC?))
   ((n-arrayAccess? expr) (ver-arrayAccess Ltable expr Mtype Ctype postC?))
   ((n-fieldAccess? expr) (ver-fieldAccess Ltable expr Mtype Ctype postC?))
   ((n-call?        expr) (ver-call        Ltable expr Mtype Ctype #f postC?))
   ((n-cast?        expr) (ver-cast        Ltable expr Mtype Ctype postC?))
   ((n-question?    expr) (ver-question    Ltable expr Mtype Ctype postC?))
   ;; For contract Java.
   ((n-specCExpr?   expr) (ver-specCExpr   Ltable expr Mtype Ctype postC?))
   (else (error "CE, ver-expr, did not recognise expression " expr))))

;; This is for contract Java special terms that may appeared in an
;; expression.
;;
;; I: expr, (s n-specCExpr)
;;    postC?, #t iff a @old or @result is allowed.
;; O: (s e-r)
(define (ver-specCExpr Ltable expr Mtype Ctype postC?)
  (match expr (($ n-specCExpr op expr2 src)
    (case op
      ((result) 
       (cond 
	((not postC?)
	 (e-r-error src "Cannot use `@result' outside a post-condition."))
	((T==? (tyM-rType Mtype) 'void)
	 (e-r-error src "Cannot use `@result', method doesn't return a value."))
	(else (make-e-r (tyM-rType Mtype) #f #f src expr2))))
      ((old)
       (cond 
	((not postC?)
	 (e-r-error src "Cannot use `@old' outside a post-condition."))
	(else
	 (let* ((e-r  (ver-expr Ltable expr2 Mtype Ctype #t)))
	   (make-e-r  (e-r-t e-r) #f #f src expr2)))))
      ))))

(define (ver-boollit  expr)
  (make-e-r 'boolean (n-boollit-value expr) #f (n-boollit-src expr) expr))

;; O: (s e-r)
(define (ver-literal Ltable expr Mtype Ctype)
  (let* ((src     (n-literal-src expr))
	 ;; Resolve the type since it could be type "String".
	 (type-l  (res-type! (n-literal-type expr) Ltable src))
	 (value   (n-literal-value expr)))
    ;; Although the literal has got a type, it might be incorrect since
    ;; it comes from the lexer who does not verify if an integer is too
    ;; big to be represented using 32 bits.
    ;; TBF: The value -2147483648 not handled properly: it is represented
    ;; by the unary operator - followed by 2147483648 which is rejected.

    (if (and (symbol? type-l)  (not (in-range? value type-l)))
	(e-r-error src "The literal "value" is outside the allowed range.")
	(make-e-r type-l value #f src expr))))

;; O: (s e-r)
(define (ver-specialName Ltable expr Mtype Ctype)
  (let* ((src (n-specialName-src expr)))
    (case (n-specialName-name expr)
      ((null)  (make-e-r 'null #f #f src expr)) ;; null is not a constant.
      ((this)
       (if (static-method? Mtype)
	   (e-r-error src "Invalid reference to `this' in a static context.")
	   ;; `this' cannot be assigned to.
	   (make-e-r Ctype #f #f src expr)))
      ((super) (e-r-error src "`super' cannot be used as a variable."))
      (else (error "CE, ver-specialName, unknown case " expr))
      )))

;; O: (s e-r)
(define (ver-postExpr Ltable expr Mtype Ctype postC?)
  ;; clearly no longer a constant expression
  (let* ((e-r      (ver-postExpr Ltable expr Mtype Ctype))
	 (e-r-expr (ver-expr Ltable (n-postExpr-expr expr) Mtype Ctype postC?)))
    (set-n-postExpr-expr! expr e-r-expr)
    (make-e-r (e-r-t e-r) #f #f (n-postExpr-src expr) expr)))

;; 
;; O: (s e-r)
(define (ver-instanceof Ltable expr Mtype Ctype postC?)
  ;; clearly no longer a constant expression
  ;; Verifies that the left side (the type) is a qualifiedName (not
  ;; a primitive type).
  (let* ((e-r      (ver-expr Ltable (n-instanceof-expr expr) Mtype Ctype postC?))
	 (src      (n-instanceof-src expr))
	 (typeSpec (n-instanceof-typeSpecifier expr))
	 (typeName (n-typeSpecifier-typeName typeSpec))
	 (dim      (n-typeSpecifier-dim typeSpec))
	 (type-var (create-tyA dim typeName (get-Gtable Ltable) src)))
    (cond
     ((or (typeUndef? (e-r-t e-r)) (typeUndef? type-var)) #t)
     ((not (type-ref? type-var))
      (sem-err #f src (T->s type-var) " is not a reference type."))
     ((not (type-ref2? (e-r-t e-r)))
      (sem-err #f src "Left expression's type is not a reference type."))
     ;; At this point it is only reference types. Use conversion rules.
     ((not (type-conversion? (e-r-t e-r) type-var))
      (sem-err #f src "This instanceof is always false." #\newline
		 "Type of left expression is " (T->s (e-r-t e-r)) 
		 #\newline
		 "Type of right expression is " (T->s type-var))))

    ;; Interestingly, the sun compiler rejects an instanceof that it
    ;; can determine is always false. We do the same.
    
    (set-n-instanceof-typeVar! expr type-var)
    (set-n-instanceof-expr!    expr e-r)
    (make-e-r 'boolean #f #f src expr)))

;; O: (s e-r)
(define (ver-cast Ltable expr Mtype Ctype postC?)
  ;; This apply a cast to a constant value resulting from a constant
  ;; expression. The exact rules of the JVM must be applied.
  ;; TBF, imprecise calculation in the Scheme implementation might
  ;; give incorrect results with float values.
  (define (apply-cast-const v typeCast typeExpr)
    ;; v is an integer or a float.
    (if (or (eqv? typeCast typeExpr) 
	    (and (member typeExpr '(float double))
		 (member typeCast '(float double))))
	v
	(let* ((n  (type->nbBytes typeCast)))
	  (if (and (member typeExpr '(float double)) 
		   (member typeCast '(byte int char short long)))
	      (limit-range (inexact->exact (truncate v)) typeCast)
	      (limit-range v typeCast)))))

  ;; A cast can be applied to convert between primitive types if a
  ;; narrowing or widening operation can be done (this includes all
  ;; combinations!). No cast can be applied between a reference type
  ;; and a primitive type. For reference types see (5.5).  If it is
  ;; a class or interface, the cast name should be resolved here.
  ;; The syntax of cast expressions is described in 15.15.
  (match expr (($ n-cast cast castedExpr src)
    (let* ((name       (or (and (pair? cast) (car cast)) cast))
	   (dim        (or (and (pair? cast) (cadr cast))  0))
	   (type-cast  (create-tyA dim name  (get-Gtable Ltable) src))
	   (e-r        (ver-expr Ltable castedExpr Mtype Ctype postC?))
	   (type-Cexpr (e-r-t e-r)))
      
      (if (not (type-conversion? type-Cexpr type-cast))
	  (sem-err #f src  "Expression cannot be cast to type."  #\newline
	   "Type of cast:       "(T->s type-cast) #\newline
	   "Type of expression: "(T->s type-Cexpr)))
      
      (set-n-cast-typeCast! expr type-cast)
      (set-n-cast-expr! expr e-r)
      ;; Always return the type of the cast, even in cases of errors.
      ;; A cast expression is never a variable, see 15.15.
      ;; If the cast conversion is necessary, keep the n-cast
      ;; as the expression to compile, otherwise simply use the castedExpr
      ;; without the n-cast. In this way, no checkcast instruction will
      ;; be generated when it can be verified at compile time that the
      ;; conversion cannot go wrong.
      ;; TBF, we should remove the n-cast for cases where none is 
      ;; necessary.
      
      (if (e-r-v e-r)
	  ;; Do the casting on the value itself and remove the cast node.
	  (make-e-r type-cast (apply-cast-const (e-r-v e-r) type-cast type-Cexpr)
		    #f src castedExpr)
	  (make-e-r type-cast (e-r-v e-r) #f src expr))
      ))))

;; I: expr, (s n-qName)
;;    Mtype, (s tyM)
;;    Ctype, (s type). The type from which the expression occurs.
;; O: (s e-r)
;;
(define (ver-qName Ltable expr Mtype Ctype)
  (let* ((qName       (n-qName-name expr))
	 (src         (n-qName-src expr))
	 (var-access  (lup Ltable qName Ctype 'non-method src))
	 (var         (car var-access))
	 (access      (cdr var-access))
	 (vTy         (and (mt? var) (mt-type var))))
    (cond 
     ((and (mt? var) (typeUndef? vTy)) (make-e-r 'undefined #f #t #f #f))
     ((or (not (mt? var)) (eqv? 'package vTy) (null? access) (tyM? vTy))
      ;; 15.10. A name cannot refer to a package, a type, or a method.
      (e-r-error src (qName->s qName)" is not a variable."))
     (else
      (let* ((final?  (var-final? var)))
	;; A non-static field cannot be accessed by a static method.
	(if (and (static-method? Mtype) (pair? access) (eqv? 'this (car access)))
	    (sem-err #f src "Static context, but field "qName" is not static."))
	;; Transfer access description to the n-qName.
	;; Eliminate all prefix access if the last component is a static field.
	;; TBF: this is not really correct if the prefix contains an array
	;;      access it should be evaluated and then discarded at run-time.
	(set-n-qName-access! expr (if (var-static? var) (list-last access) access))

	(make-e-r vTy
		  ;; if it is final and have a constant value.
		  (if (and final? (not (n-varInit? (mt-value var))))
		      (mt-value var)
		      #f)
		  ;; If it is not final it is a variable.
		  (not final?) src expr)))
     )))
  
;; I: expr, (s n-question)
;; O: (s e-r)
(define (ver-question Ltable expr Mtype Ctype postC?)
  ;; (15.24)
  (match expr (($ n-question cond-e Sthen Selse src)
    (let* ((e-r-cond (ver-expr Ltable cond-e Mtype Ctype postC?))
	   (e-r-then (ver-expr Ltable Sthen Mtype Ctype postC?))
	   (e-r-else (ver-expr Ltable Selse Mtype Ctype postC?))
	   (tExpr    (det-type-question (e-r-t e-r-then) (e-r-t e-r-else) 
					(e-r-v e-r-then) (e-r-v e-r-else))))
      (set-n-question-expr! expr e-r-cond)
      (set-n-question-Sthen! expr e-r-then)
      (set-n-question-Selse! expr e-r-else)
      
      (cond 
       ((not tExpr)
	(e-r-error src "Incompatible branches in operator `?'." 
		   " Cannot convert " (T->s (e-r-t e-r-then))
		   " to "  (T->s (e-r-t e-r-else))
		   " (or vice versa)."))

       ;; All branches must be a constant expression to be considered
       ;; a constant expression even though the conditional itself and
       ;; the corresponding branch would be enough. So true ? 10 : e
       ;; is not a constant expression if e is not a final initialized
       ;; variable.
       ((and (e-r-v e-r-cond) (e-r-v e-r-then) (e-r-v e-r-else))
	(make-e-r tExpr 
		  (if (eqv? 'true (e-r-v e-r-cond))
		      (e-r-v e-r-then)
		      (e-r-v e-r-else))
		  #f src expr))
       (else (make-e-r tExpr #f #f src expr)))
      ))))

;; Based on (5.5). Used to verify cast conversion.
;; I: 
;; O: #t iff tExpr can be converted to type-cast.
(define (type-conversion? tExpr type-cast)
  (or 
   (typeUndef? type-cast)   (typeUndef? tExpr)
   (T==? tExpr type-cast)
   ;; On primitive types the test can be made simpler since all
   ;; combinations of conversions are allowed.
   (prim-widening?  tExpr type-cast)
   (prim-narrowing? tExpr type-cast)
   ;; Reference types (consult 5.5) The value 0 is not important.
   ;; TBF: Here we should probably verify type-assignable only after
   ;; the other rules did not determine that there is an error.  This
   ;; implementation is more liberal.
   (type-=? tExpr type-cast 0)
   (and (type-ref? tExpr) (type-ref? type-cast)
	(cond
	 ((type-class? tExpr)
	  (or
	   (and (type-class? type-cast)
		(or (subClass? tExpr type-cast (get-Gtable type-cast))
		    (subClass? type-cast tExpr (get-Gtable type-cast))))
	   (and (type-interface? type-cast)
		(or (not (final-class? tExpr))
		    (and (final-class? tExpr) (implements? tExpr type-cast))))
	   (and (tyA? type-cast)
		(T==? tExpr '("java" "lang" "Object")))
	   ))
	 ((type-interface? tExpr)
	  (or (and (type-class? type-cast) 
		   (or (not (final-class? type-cast))
		       (and (final-class? type-cast)
			    (implements? type-cast tExpr))))
	      ;; This case is curious. Should it also apply to 
	      ;; assignable? It is not implemented correctly
	      ;; in pizza v0.39g.
	   (and (type-interface? type-cast)
		(interface-methods-compatible? tExpr type-cast))))
	 ;; Only one case remain, tExpr is an array.
	 ;; That means tExpr is a (s tyA).
	 (else
	  (or (and (type-class? type-cast) (T==? type-cast '("java" "lang" "Object")))
	      (and (type-interface? type-cast)
		   (T==? type-cast '("java" "lang" "Cloneable")))
	      (and (tyA? type-cast)
		   (type-conversion? (tyA-type tExpr) (tyA-type type-cast))))
	  )))))
  
;; I: expr, (s n-op)
;; O: (s e-r)
(define  (ver-op Ltable expr Mtype Ctype postC?)
  (match expr (($ n-op op ls rs src-op)
    (let* ((e-r-ls (ver-expr Ltable ls Mtype Ctype postC?))
	   (e-r-rs (ver-expr Ltable rs Mtype Ctype postC?)))
      (set-n-op-left! expr e-r-ls)
      (set-n-op-right! expr e-r-rs)
      (cond 
       ((typeUndef? (e-r-t e-r-ls)) e-r-ls)
       ((typeUndef? (e-r-t e-r-rs)) e-r-rs)
       (else
	(match e-r-ls (($ e-r type-ls vls ls-var? src-ls)
           (match e-r-rs (($ e-r type-rs vrs rs-var? src-rs)
	      (let* ((tExpr (determine-expr-op op type-ls type-rs Ltable src-op)))
		(set-n-op-type! expr tExpr)
		(cond
		 ((not tExpr)
		  ;; The expression cannot be typed. Don't try to see
		  ;; if it is a constant expression.
		  (make-e-r 'undefined #f #f src-op expr))
		 ((and vls vrs)
		  ;; Both operands are constants, the expression is a
		  ;; constant, evaluates it.
		  (apply-op-const op vls vrs type-ls type-rs tExpr src-op expr))
		 (else
		  ;; One of the operands is not a constant, the expression is
 		  ;; not a constant.
		  (make-e-r tExpr #f #f src-op expr)) 		  
		 )))))
	   )))))))

;; I: op, one of + - * / % << <<< >> && || & | ^ == != <= >= < >.
;; O: (u #f Type). Return the type of the resulting binary operation.
(define (determine-expr-op op t1 t2 Ltable src)
  (if (or (typeUndef? t1) (typeUndef? t2)) 
      'undefined
  (case op
    ((+) 
     (cond
      ((or (T==? t1 '("java" "lang" "String")) 
	   (T==? t2 '("java" "lang" "String")))
       (res-type! '("java" "lang" "String") Ltable src))
      ((or (not (type-numeric? t1)) (not (type-numeric? t2)))
       (sem-err #f src
		"Binary operator + works on numeric or String types only." #\newline
		"The left side has type: " (T->s t1) #\newline
		"The right side has type: " (T->s t2)))
      (else (binary-numeric-promotion t1 t2))))
    ((- * / %) 
     (if (or (not (type-numeric? t1)) (not (type-numeric? t2)))
	 (sem-err #f src "Binary "op" works on numeric types only." #\newline
		  "Type of left side of "op" is: "(T->s t1)#\newline
		  "Type of right side of "op" is: "(T->s t2))
	 (binary-numeric-promotion t1 t2)))
    ((== !=)
     (if (not (or (and (type-primitive? t1) (type-primitive? t2))
		  (and (not (type-primitive? t1)) (not (type-primitive? t2)))))
	 (sem-err #f src "Relational operator "op" cannot be applied between a primitive type and an object.")
	 'boolean))
    ((< > <= >=)
     (if (not (and (type-numeric? t1) (type-numeric? t2)))
	 (sem-err #f src "Operator "op" can be applied to numerical values only.")
	 'boolean))
    ((&& oror)    
     (if (not (and (type-boolean? t1) (type-boolean? t2)))
	 (sem-err #f src "Operator "(op->opJava op)" can be applied to boolean values only.")
	 'boolean))
    ((or & ^) ; works on integral and boolean
     (if (and (type-integral? t1) (type-integral? t2))
	 (binary-numeric-promotion t1 t2)
	 (if (and (type-boolean? t1) (type-boolean? t2))
	     'boolean
	     (sem-err #f src "Operator "(op->opJava op) 
		      " works between integral or boolean values."
		      "The expressions have types: "(T->s t1)" and "(T->s t2)"."))))
    ((<< >> >>>)
     ;; see (15.18)
     (if (or (not (type-integral? t1)) (not (type-integral? t2)))
	 (sem-err #f src "Binary "op" works on integral types only." #\newline
		  "The expressions have types: "(T->s t1)" and "(T->s t2))
	 (unary-prom t1)))
    (else (error "CE, determine-expr-op, unknown operator  " op)))))

;; Apply one operator between Java constants. The type of the final expression
;; is given by tExpr. This type is not determined by the value of the
;; expression. For example 2147483647 + 1 has a value outside the range of 
;; an int, but it is encoded as an int not a long. Types have been verified
;; to allow the operations to be performed without errors.
;; TBF : none of the NaN, infinities or minus zero are implemented.
;;
;; I: op, sym.
;;    vls, vrs, (int float char 'true 'false)
;;    tls, type of vls
;;    trs, type of vrs
;;    tExpr, (u 'int 'char 'long 'short 'byte 'float 'double)
;; O: (s e-r). The value of operation is inserted in the e-r result.
;;
(define (apply-op-const op vls vrs tls trs tExpr src expr)
  (define (op-or a b)  (or (eqv? a 'true) (eqv? b 'true)))
  (define (op-and a b) (and (eqv? a 'true) (eqv? b 'true)))
  (define (op-xor a b) (not (eqv? a b)))
  (define (bool-op op) (:~. op `((or . ,op-or) (oror . ,op-or) (and . ,op-and) 
				 (&& . ,op-and) (^ . ,op-xor))))
  
  (define (t->nbbits t) (:~. t '((int . 32) (long . 64))))
  ;; O: 0 <= int <= 63;
  (define (limit-shift v t) (binary-op '& (if (eqv? t 'int) 31 63) v 32))

  (case op
    ((+) ;; 15.17.1 and 15.17.2
     (if (T==? tExpr '("java" "lang" "String"))
	 (make-e-r tExpr (++ (toString vls tls) (toString vrs trs)) #f src expr)
	 (make-e-r tExpr  (limit-range (+ vls vrs) tExpr) #f src expr)))
    ((-) ;; 15.17
     (make-e-r tExpr (limit-range (- vls vrs) tExpr) #f src expr))
    ((*) ;; 15.16.1
     (make-e-r tExpr (limit-range (* vls vrs) tExpr) #f src expr))
    ((/) ;; 15.16.2
     (if (= vrs 0) (error "TBF, Case 0 for / as a constant"))
     (make-e-r tExpr (/ vls vrs) #f src expr))
    ((%) ;; 15.16.3
     (if (= vrs 0) (error "TBF, case % 0 not yet implemented"))
     (if (member tExpr '(float double))
	 (error "TBF, % not yet implemented for constant floating points"))
     ;; TBF remainder for % or modulo?
     (make-e-r tExpr (remainder vls vrs) #f src expr))
    ((<<)  ;; 15.18
     (make-e-r tExpr (limit-range (* vls (expt 2 (limit-shift vrs tls))) tExpr)
	       #f src expr))
    ((>>)  ;; 15.18
     (let* ((r  (floor (/ vls (expt 2 (limit-shift vrs tls))))))
       (make-e-r tExpr (limit-range r tExpr) #f src expr)))
    ((>>>) ;; 15.18 TBF for negative
     (let* ((r (floor (/ vls (expt 2 (limit-shift vrs tls))))))
       (make-e-r tExpr (limit-range r tExpr) #f src expr)))
    ((<= >= == < > !=) ;; 15.19 TBF does not handle NaN, infinities and -0
     (make-e-r 
      'boolean
      (boolScheme->java
       ((:~. op `((<= . ,<=) (<  . ,<) (>  . ,>) (>= . ,>=) (== . ,=)
		  (!= . ,(lambda (x y) (not (= x y))))))
	vls vrs))
      #f src expr))
    ((or ^ & oror &&) ;; tExpr can only be int or long.
     (make-e-r tExpr
	       (if (eqv? tExpr 'boolean)
		   (boolScheme->java  ((bool-op op) vrs vls))
		   ;; Both are integers
		   (limit-range (binary-op op vls vrs (t->nbbits tExpr)) tExpr))
	       #f src #f))
    (else (error "CE, apply-op-const, unknown operator " op))
    ))

(define (boolScheme->java b) (if b 'true 'false))

;; I: I1, I2 (s type). Interfaces. 
;; O: #t iff the intersection of
;;       methods of I1 and I2 having the same signature, have the same
;;       return type.  (This rule is for casting from interface to
;;       interface, see (5.5)).
;;
(define (interface-methods-compatible? I1 I2)
  (let* ((I1-methods  (type->method-types I1))
	 (I2-methods  (type->method-types I2)))
    (not (p-exists? 
	  (lambda (m2) 
	    (let* ((m (:~-p (lambda (m) (method-sign=? m m2))  I1-methods)))
	      (and m (different-result? m m2))))
	  I2-methods))))

;; I: m1, m2 (s tyM) 
;; O: #t iff m1 and m2 have the same signature with a different type of result.
(define (different-result? m1 m2)
  (and (method-sign=? m1 m2) (not (T==? (tyM-rType m1) (tyM-rType m2)))))

;; I: m1, m2 (s tyM)
;; O: #t iff if m1 and m2 have the same signature.
(define (method-sign=? m1 m2)
  (and (string=? (tyM-mName m1) (tyM-mName m2))
       (= (length (tyM-parm-types m1)) (length (tyM-parm-types m2)))
       (all T==? (tyM-parm-types m1) (tyM-parm-types m2))))

;; Return all method types from the table of the type.
;;
;; I : T, (s type)
;; O : [(s mt)]
;;
(define (type->local-method-types T)
  (map cdr (filter (lambda (x) (eqv? (mt-kind (cdr x)) 'method))
		   (table-names (type-table T)))))

;; Return all method types of the type T considering its hierarchy.
;; I : T, (s type)
(define (type->method-types T)
  ;; TBF. It should consider the hierarchy.
  (map mt-type (type->local-method-types T)))

;; The only place a n-fieldAccess is built by syntax analysis
;; is as a FieldAccess. This includes the following cases: this.id, super.id
;;
;; I: expr, (s n-fieldAccess)
;; O: (s e-r)
(define (ver-fieldAccess Ltable expr Mtype Ctype postC?)

  ;; field is almost an expression, but it could be `super', an
  ;; invalid case for ver-expr. Accessibility `protected' also
  ;; needs a particular call to lupT-f for super.
  ;; O: (u #f (s mt))
  (define (get-field-mt field id src)
    (cond 
     ((and (n-specialName? field) (eqv? 'super (n-specialName-name field)))
      (if (static-method? Mtype)
	  (sem-err (cons #f MT-UNDEF) src "`super' cannot be used in a static context.")
	  (cons (type-extend Ctype)
		(and (type-extend Ctype)
		     (lupT-f (type-extend Ctype) id #t Ctype #f src)))))
     (else 
      (let* ((e-r-field  (ver-expr Ltable field Mtype Ctype postC?))
	     (typeField  (e-r-t e-r-field))
	     (mt         (or (and (type-ref? typeField) 
				  (lupT-f typeField id #t Ctype typeField src))
			     MT-UNDEF)))
	(if (and (not (typeUndef? typeField)) (not (type-ref? typeField)))
	    (sem-err #f src "Cannot dereference the type "(T->s typeField)"."))
	(cons typeField mt)
	))))
  
  (match expr (($ n-fieldAccess field id src)
    (let* ((type.mt   (get-field-mt field id src))
	   (mt        (cdr type.mt))
	   (typeField (car type.mt)))
      (if (not mt) 
	  (e-r-error src "Unknown field "id" in type "(T->s typeField)".")
	  (begin
	    (set-n-fieldAccess-access! expr (mt-access mt))
	    (make-e-r (mt-type mt)
		      (if (n-varInit? (mt-value mt)) #f (mt-value mt))
		      (if (mt-access mt) #t #f)
		      src expr)))))))

;; I: expr, (s n-arrayAccess)
;; O: (s e-r)
(define (ver-arrayAccess Ltable expr Mtype Ctype postC?)
  (match expr (($ n-arrayAccess array index src)
    (let* ((e-r-array  (ver-expr Ltable array Mtype Ctype postC?))
	   (type       (e-r-t e-r-array))
	   (e-r-index  (ver-expr Ltable index Mtype Ctype postC?))
	   (type-index (unary-prom (e-r-t e-r-index))))
      (cond
       ((typeUndef? type)	(make-e-r 'undefined #f #t src expr))
       ((not (tyA? type))
	(sem-err #f src "Expression is not an array type. Type of expression is " 
		 (T->s type))
	(make-e-r type #f #t src expr))
       ((not (or (typeUndef? type-index) (T==? type-index 'int)))
	(sem-err #f src
		 "Array index expression must be an integral type. "
		 "The type of the index is: " (T->s type-index) ".")
	(make-e-r (subarray-type type) #f #t src expr))
       (else
	(let* ((typeAccess (subarray-type type)))
	  (set-n-arrayAccess-basicType! expr (T->jvmType typeAccess))
	  (make-e-r typeAccess #f #t src expr))
	))))))

;; I: expr, (s n-alloc)
;;    Ltable, (s Ltable)
;; O: (s e-r)
;;
(define (ver-alloc Ltable expr Mtype Ctype postC?)

  (define (ver-dimExpr expr)
    (let* ((e-r   (ver-expr Ltable expr Mtype Ctype postC?))
	   (type  (e-r-t e-r))
	   (src   (e-r-src e-r)))
      (if (and (not (typeUndef? type))
	       (not (and (type-numeric? type) (T==? (unary-prom type) 'int))))
	  (sem-err #f src "A dimension expression must be of type int."
		   "The dimension expression is of type " (T->s type)"."))
      e-r))

  (match expr (($ n-alloc qName allocExpr src1)
    (if qName (error "TBF, ver-alloc " expr))
    (if (n-arrayAlloc? allocExpr)
	;; Allocation of an array.
	(match allocExpr
	       (($ n-arrayAlloc typeName dimExprs dims initializers src2)
	  ;; Verify if typeName exists and replace name for the structure.
	  ;; If it is not a nodeqName it is a primitive type.
	  (if (n-qName? typeName)
	      (let* ((typeName (n-qName-name typeName))
		     (mt       (lup Ltable typeName #f 'type src2))
		     (typeStru (mt-type mt)))
		(set-n-arrayAlloc-typeName! allocExpr typeStru)
		(if (and (not (typeUndef? typeStru)) 
			 (not (type? typeStru)))
		    (e-r-error src2 (T->s typeName) " is not a type."))))
	  
	  (if (pair? dimExprs)
	      (set-n-ArrayAlloc-dimExprs! allocExpr (map ver-dimExpr dimExprs)))
	  (make-e-r 
	   (create-tyA (+ (if dimExprs (length dimExprs) 0) dims) 
		       typeName (get-Gtable Ltable) src2)
	   #f  #f  src1 expr)))
	;; The only other case is classAlloc.  Example : new C(2,3).
	;; The class C is searched locally (not its superclass) for
	;; the method <init> with a signature derives from the argument
	;; list.
	(match allocExpr
	       (($ n-classAlloc typeName argList fieldDeclarations src2)
	  (if (not (or (not fieldDeclarations) 
		       (null? fieldDeclarations)))
	      (error "TBF, fieldDeclarations in classAlloc " allocExpr))
	  (if (not (n-qName? typeName))
	      (e-r-error src2 "Instance allocation of a primitive type.")
	      (let* ((cName     (n-qName-name typeName))
		     (mt        (lup Ltable cName #f 'type src2))
		     (classType (mt-type mt))
		     ;; classType is the type where the constructor
		     ;; should be, while Ctype is the type where
		     ;; the class allocation occurs. 
		     (args-e-r  (det-args-type argList Ltable Mtype Ctype postC?))
		     (type-args (map e-r-t args-e-r)))
		(set-n-classAlloc-argList! allocExpr args-e-r)
		(set-n-classAlloc-typeName! allocExpr classType)
		(if (not (typeUndef? classType))
		    (cond 
		     ((not (type-class? classType))
		      (e-r-error src2 (qName->s cName)" is not a class."))
		     ((member 'abstract (type-mdfs classType))
		      (e-r-error src2 (qName->s cName)
				 " is abstract, cannot instanciate."))
		     (else
		      ;; TBF, the class could still be abstract because
		      ;; some inherited method is not yet implemented
		      ;;
		      (let* ((typeMethod (call->type! 
					  Ltable allocExpr type-args Mtype Ctype
					  src2 #f postC?)))
			(set-n-classAlloc-typeMethod! allocExpr typeMethod)
			))))
		(make-e-r classType #f #f src2 expr)))))
	))))

;; See (15.24) for the rules of the type of conditional operator.
;;
;; I: t1 type of operand 1. It is always an e-r type.
;;    v1 (u #f number)
;;    t2 type of operand 2. It is always an e-r type.
;;    v2 (u #f number)
;; O: type of the conditional ? expression.
;;    If it is not typable, return #f.
;;
(define (det-type-question t1 t2 v1 v2)
  (cond 
   ((typeUndef? t1)   t1)
   ((typeUndef? t2)   t2)
   ((T==? t1 t2)    t1)
   ((and (type-numeric? t1) (type-numeric? t2))
    (cond 
     ((and (member t1 '(byte short)) (member t2 '(byte short)))
      'short)
     ((and (member t1 '(byte short char)) (T==? t2 'int)
	   v2 (in-range? v2 t1))
      t1)
     ((and (member t2 '(byte short char)) (T==? t1 'int)
	   v1 (in-range? v1 t1))
      t2)
     (else (binary-numeric-promotion t1 t2))))
   ((and (eqv? t1 'null) (type-ref? t2))    t2)
   ((and (eqv? t2 'null) (type-ref? t1))    t1)
   ((and (type-ref? t1) (type-ref? t2))
    (cond
     ((type-=? t1 t2 #f)      t2)
     ((type-=? t2 t1 #f)      t1)
     (else  #f)))
   (else  #f)))

;; See 15.25.
;; O: (s e-r)
(define (ver-assignment Ltable assignment Mtype Ctype postC?)
  (match assignment (($ n-assignment lhs op rhs src)
    (let* ((e-lhs (ver-expr Ltable lhs Mtype Ctype postC?))
	   (e-rhs (ver-expr Ltable rhs Mtype Ctype postC?))
	   (Trhs  (e-r-t e-rhs))
	   (Tlhs  (e-r-t e-lhs))
	   (Trhsp (if (not (eqv? op '=))
		      (determine-expr-op (assign-op->op op) Trhs Tlhs Ltable src)
		      Trhs)))
      (cond 
       ((not (e-r-variable? e-lhs))
	(e-r-error src "The left hand side is not a variable."))
       ((or (not Trhsp) (typeUndef? Trhs) (typeUndef? Tlhs))
	ER-UNDEFINED)
       (else
	;; The type of the left side is the type of the variable.
	;; Because an implicit cast using the var type is assumed.
	(set-n-assignment-lhs! assignment e-lhs)
	(set-n-assignment-rhs! assignment e-rhs)
	(if (or ;; The general case is =.
	     (and (eqv? op '=)  (not (type-=? Tlhs Trhsp (e-r-v e-rhs))))
	     ;; For += it was verified by determine-expr-op that the operation is
	     ;; valid, simply check that a generated String goes into a string var. 
	     (and (eqv? op '+=) (T==? Trhsp '("java" "lang" "String"))
		  (not (T==? Tlhs '("java" "lang" "String"))))
	     ;; For operations, where numerical types are involved, a
	     ;; narrowing and widening are acceptable. So there is nothing else
	     ;; to check since determine-expr-op did check that. See 15.25.
	     )
	    (sem-err #f src "Not assignable types." #\newline 
		     "Left hand side type: "(T->s Tlhs) #\newline 
		     "Right hand side type: "(T->s Trhs)))
	(make-e-r Tlhs #f #f src assignment)))
      ))))

(define (assign-op->op op) (:~. op '((*= . *) (/= . /) (%= . %) (+= . +) (-= . -) 
				     (<<= . <<) (>>= . >>) (>>>= . >>>) (&= . &) 
				     (^= . ^) (or= . or))))

;; Return the method type of a method or constructor call.
;;
;; I: Ltable, the current nested table.
;;    Mcall, (u (s n-literal) (s n-boollit)  (s n-qName)
;;              (s n-call) (s n-arrayAccess)
;;              (s n-fieldAccess) (s n-specialName))
;;    type-args, [Type]. The types of the argument list.
;;    Mtype (s tyM)
;;    Ctype, (s type-class)
;;    super?, #t iff a super-call is allowed.
;; O: (u 'undefined (s tyM))
;;    The method type of the method called, if found.
;;
(define (call->type! Ltable Mcall type-args Mtype Ctype src super? postC?)
  ;; Determine the method called. It needs searching for name of
  ;; method in a class or interface. In case more than one method
  ;; exists, the number of arguments and if necessary their types
  ;; are used.  
  ;; O: (u #f (s mt))
  ;;
  (define (det-method-called)
    
    ;; Calculate the most specific method according to the rules of
    ;; section 15.11.2.2
    ;;
    ;; I: ms, [(s tyM)]. These are the applicable and accessible methods.
    ;; O: [(s tyM)]. The methods chosen. If there is more than one,
    ;; there is no unique most specific method.
    (define (mostSpecific ms) (foldr (lambda (x ms) (scanSpecific ms x)) '() ms)) 

    ;; I: ms, [(s tyM)].
    ;;    x, (s tyM).
    ;; O: [(s tyM)]. 
    (define (scanSpecific ms x)
      (let loop ((l ms) (r '()) (in? #f))
	(if (pair? l)
	    (cond ((moreSpecific? x (car l))   (loop (cdr l) r x))
		  ((moreSpecific? (car l) x)   (append r l))
		  (else (loop (cdr l) (: (car l) r) x)))
	    (if x (: x r) r))))

    ;; I: m1, m2 (s tyM). 
    ;; O: #t iff m1 is more specific than m2
    (define (moreSpecific? m1 m2)
      (and (call-conv? (res-type! (tyM-classType m1) Ltable LIB-SRC)
		       (res-type! (tyM-classType m2) Ltable LIB-SRC))
	   (method-app? (tyM-parm-types m1) (tyM-parm-types m2))))

    ;; See (5.3) 
    ;; O: #t iff type-arg can be converted to type-parm in a
    ;; method invocation.
    (define (call-conv? type-arg type-parm)
      ;; The conversions allowed are almost the same as assignment
      ;; conversion, but the narrowing on constant values are not permitted.
      (or (T==? type-arg type-parm)
	  (prim-widening? type-arg type-parm)
	  (reference-widening? type-arg type-parm)))

    ;; See (15.11.2.1) for definition of applicable.
    (define (method-app? t-args t-parms)
      (and (= (length t-args) (length t-parms))
	   (all (lambda (t-arg t-parm) 
		  ;; TBF #f should be modified to be more precise
		  (call-conv? (res-type! t-arg Ltable #f) 
			      (res-type! t-parm Ltable LIB-SRC)))
		t-args t-parms)))

    (define (acceptable mName)
      (lambda (method)
	(and (string=? mName (tyM-mName method))
	     (method-app? type-args (tyM-parm-types method)))))

    ;; I: type, (s type). The type from which the method should be found.
    ;;    preT, (u #f #t (s type)). 
    ;;    mName, str. The name of the method without any qualifier.
    ;; O: [(s mt)]
    (define (method-acc-app mName type init? preT src)
      (let* ((mts (lupT-m type (acceptable mName) #t Ctype preT init?)))
	(if (null? mts)
	    (sem-err #f src "No "(or (and (string=? "<init>" mName)"constructor") 
				      (++ "method "mName))
		     " applicable with parameter types "
		     (map T->s type-args)" in type "(T->s type)"." ))
	mts))
    
    ;; Return all methods that are applicable and accessible taking
    ;; into account the overridden methods. Several methods could be
    ;; returned. In that case a most specific computation should be
    ;; done to choose one. (This may fail and in that case it is an
    ;; ambiguous call.)
    ;;
    ;; O : [(s mt)], the list is empty if none are applicable.
    ;;
    (define (det-applicable-methods)
      (cond
       ((or (n-literal? Mcall) (n-boollit? Mcall))
	;; The case of a string literal "...".id() is acceptable,
	;; but it is a n-fieldAccess.
	(sem-err '() (or (and (n-literal? Mcall) (n-literal-src call))
			 (n-boollit-src Mcall))
		 "A literal cannot be used as a method name."))
       ((n-specialname? Mcall)
	(let* ((src (n-specialName-src Mcall)))
	  (cond
	   ((not (eqv? (n-specialName-name Mcall) 'super))
	    (sem-err '() src "`this' or `null' cannot refer to a method."))
	   ;; This is super(...).
	   ;; It can be done only as the first instruction of a constructor.
	   ((not super?) (sem-err '() src "Incorrectly placed super call."))
	   ((type-extend Ctype)
	    ;; Search method name using the name <init> in the superclass.
	    (method-acc-app "<init>" (type-extend Ctype) #t #f src))
	   (else '()))))
       ((n-qName? Mcall)
	;; See 15.11.1 for the rules on names identification.
	;; Two cases:
	;;   1) A simple identifier.
	;;   2) A qualified name of the form prefix . identifier
	;;
	;; The method name is always the last identifier of a
	;; qualified name.
	(let* ((qName       (n-qName-name Mcall))
	       (src         (n-qName-src Mcall))
	       (mName       (list-last qName))
	       (prefix      (if (pair? qName) (list-but-last qName) '()))
	       (class-table (Ltable-typeTable Ltable)))
	  (if (null? prefix) 
	      ;; Case 1, a simple identifier. The method called is in
	      ;; the class or one of the super-classes where this
	      ;; method is defined.
	      (let* ((mtas (method-acc-app mName Ctype #f #f src)))
		;; By default it uses the object itself.  If the
		;; method called is static, the code generator should
		;; not used this reference.
		(set-n-qName-access! Mcall 'this)
		mtas)
	      ;; Case 2, there is a prefix before the method name.
	      ;; This prefix can be a type, like in a static invocation,
	      ;; or a variable.
	      (let* ((mtA   (lup Ltable prefix Ctype 'non-method src))
		     ;; resolving necessary? TBF
		     (mt2   (and mtA (res-type! (car mtA) Ltable src))) 
		     (type  (and mt2 (mt-type mt2))))
		(cond
		 ((not type)
		  (sem-err '() src "Undefined type " (qName->s prefix)))
		 ((not (type-ref? type))
		  (if (typeUndef? type) '()
		      (sem-err '() src "The prefix "(qName->s prefix)
			       " does not specify a class or interface." #\newline
			       "It's type is: " (T->s type))))
		(else
		 (set-n-qName-access! Mcall (cdr mtA))
		 ;; TBF if it is empty, error message?
		 (method-acc-app mName type #f type src))
		)))))
       ((n-classAlloc? Mcall) ;; A special case from ver-alloc.
	(let* ((resType (res-type! (n-classAlloc-typeName Mcall) Ltable src)))
	  (set-n-classAlloc-typeName! Mcall resType)
	  (method-acc-app "<init>" resType #t #t src)))
       ((or (n-arrayAccess? Mcall) (n-call? Mcall))
	;; Cases like V[1](i) or f()().  This is not caught by the
	;; parser but it is clearly in error by its syntax.
	(sem-err '() src "Invalid form to call a method."))
       ((n-fieldAccess? Mcall)
	;; Cases like V[1].f(), a.f1(a,b).f2(d,e), "...".id(), or
	;; super.f()
	(match Mcall (($ n-fieldAccess field id src)
	  ;; id is the function name.  There is a special case for
	  ;; super.id(). ver-expr refuses super as an expression so it
	  ;; is processed here.
	  (let* ((type  
		  (if (and (n-specialName? field)
			   (eqv? (n-specialName-name field) 'super))
		      (res-type! (type-extend Ctype) Ltable src)
		      (e-r-t (ver-expr Ltable field Mtype Ctype postC?)))))
	    (if (type-ref? type)
		(method-acc-app id type #t type src)
		(if (typeUndef? type) '()
		    (sem-err '() src "Type of expression is not a class or interface." 
			     "It's type is: "(T->s type)))
		)))))
       (else (error "CE, unknown type of method-call " Mcall))))

    (let* ((app-acc  (det-applicable-methods))
	   (tMethods (mostSpecific (map mt-type app-acc))))
      ;; If app-acc is '() an error message has been reported already:
      ;; Don't give other error messages.
      (cond 
       ((null? tMethods)  #f)
       ((> (length tMethods) 1)
	(sem-err #f src "Ambiguous method call. Most specific methods are: "
		 (++del (map (lambda (x) (++ (T->s x)" in class " 
					     (T->s (tyM-classType x))))
			      mtM) ", ") "."))
       ((and (static-method? Mtype) (n-qName? Mcall) (eqv? 'this (n-qName-access Mcall))
	     (instance-method? (car tMethods)))
	(sem-err #f src "Cannot call instance method "
		 (T->s (car tMethods))" in a static context."))
       ;; The only correct case.
       (else (car tMethods)))))

  ;; Avoid searching a method when one of the argument is undefined.
  (if (p-exists? typeUndef? type-args)
      'undefined
      (let* ((mType-called  (det-method-called)))

	;; In the case of a method found in a class file, the classType of its
	;; method type is not yet resolved: It is done here.
	;; TBF there is probably a mistake with classType since it
	;; comes from the constant pool as is (performed by build-method in
	;; classToAst.scm) It is not a qName.

	(if mType-called
	    (set-tyM-classType! Mtype-called (res-type! (tyM-classType mType-called) 
							Ltable LIB-SRC)))
	(or mType-called 'undefined))))

;; O: [(s e-r)]  
(define (det-args-type argList Ltable Mtype Ctype postC?)
  (map (lambda (arg) (ver-expr Ltable arg Mtype Ctype postC?)) argList))

;; I: Ltable, the table of the method where this call occurs.
;;    call,  (s n-call).
;;    Mtype, (s tyM) The method type where the call occurs.
;;    Ctype, (s type)        The type where the call occurs.
;;    super?, #t iff a super() call is allowed.
;; O: (s e-r). Type of the call 
;;    Modify the field type of call and all access
;;    fields of the prefix, and the arguments.
;;
(define (ver-call Ltable call Mtype Ctype super? postC?)
  (match call (($ n-call method-call argList src)
    (let* ((args-e-r    (det-args-type argList Ltable Mtype Ctype postC?))
	   (type-args   (map e-r-t args-e-r))
	   (Mtype       (call->type! Ltable method-call
				     type-args Mtype Ctype src super? postC?))
	   (rType       (or (and (tyM? Mtype) (res-type! (tyM-rType Mtype) Ltable #f))
			    'undefined)))

      ;; Replace all arguments by their e-r.
      (set-n-call-argList! call args-e-r)
      ;; Keep the method type to generate bytecode.

      (set-n-call-method-type! call Mtype)
      (make-e-r rType #f #f src call))
      )))

;; I: postExpr, (s n-postExpr)
;; O: (s e-r)
(define (ver-postExpr Ltable postExpr Mtype Ctype postC?)
  (match postExpr (($ n-postExpr expr op src)
    (let* ((e-r   (ver-expr Ltable expr Mtype Ctype postC?))
	   (tExpr (e-r-t e-r)))
      ;; -- see (15.13.3)
      ;; ++ see (15.13.2)
      (set-n-postExpr-expr! postExpr e-r)

      (cond 
       ((not (e-r-variable? e-r))
	(e-r-error src "The post operator "op" must be applied to a variable."))
       ((and (not (typeUndef? tExpr)) (not (type-numeric? tExpr)))
	(e-r-error src "The post operator "op" must be applied to a numeric variable."
		   " The variable has type: "(T->s tExpr)))
       (else (make-e-r tExpr #f #f src postExpr)))
      ))))

;; I: unary, (s n-unary)
;; O: (s e-r)
(define (ver-unary Ltable unary Mtype Ctype postC?)
  (match unary (($ n-unary op expr src)
    (let* ((e-r        (ver-expr Ltable expr Mtype Ctype postC?))
	   (tExpr  (e-r-t e-r)))
      (set-n-unary-expr! unary e-r)
      (if (typeUndef? tExpr)
	  e-r
	  (case op
	    ((!)
	     (if (not (T==? tExpr 'boolean))
		 (sem-err (make-e-r 'boolean #f #f src unary) 
			  src "The operator ! must be applied to a boolean expression."
		    #\newline "The expression has type: "(T->s tExpr))
		 (if (e-r-v e-r)
		     (make-e-r 'boolean (if (eqv? (e-r-v e-r) 'true) 'false 'true) 
			       #f src unary)
		     (make-e-r 'boolean #f #f src unary)
		     )))
	    ((+ -) ;; + see (15.14.3), - see (15.14.4)
	     (if (not (type-numeric? tExpr))
		 (e-r-error src "The unary operator " op 
			    " must be applied to a primitive numeric type." 
			    #\newline "The expression has type: "(T->s tExpr))
		 (make-e-r (unary-prom tExpr)
			   (and (e-r-v e-r) 
				(if (eqv? op '-) (- (e-r-v e-r)) (e-r-v e-r)))
			   #f src unary)))
	    ((~) ;; see (15.14.5)
	     (if (not (type-numeric? tExpr))
		 (e-r-error 
		  src "The operator ~ must be applied to an integral type." 
		  #\newline "The expression has type: "  (T->s tExpr))
		 (make-e-r (unary-prom tExpr)
			   (if (e-r-v e-r) (- (- (e-r-v e-r)) 1) #f)
			   #f src unary)))
	    ((++ --) ;; ++ see (15.14.1) -- see (15.14.2) 
	     (cond 
	      ((not (e-r-variable? e-r))
	       (e-r-error "The unary operator "op" applied to a non-variable."))
	      ((not (type-numeric? tExpr))
	       (e-r-error src "The unary operator "op
		" must be applied to a variable of numeric type." #\newline 
		"The expression has type: " (T->s tExpr)))
	      (else ;; promotion of type should be done. TBF
	       (make-e-r tExpr #f #f src unary))))
	    (else "CE, ver-unary, unknown operator " op)
	    )))
    )))

;; This code is based on 14.7.
;;
;; O: #t iff expr is an expression that can be used as a statement.
(define (stmt-expression? expr)
  (or (n-assignment? expr)(n-postExpr? expr)(n-call? expr)
      (and (n-unary? expr) (member (n-unary-op expr) '(-- ++)))
      (and (n-alloc? expr) (n-classAlloc? (n-alloc-expr expr)))))

;; Verification of a variable local declaration.
;; I: table,    (s Ltable). This is current local table.
;;    varsDecl, (s n-localVarDecl)
;; O: int. The number of words used by the variables.
;;
(define (ver-localVarDecl! table varsDecl Mtype Ctype)

  ;; I: var, (s n-declaratorName)
  ;;    i, the position on the stack of this var.
  ;; O: void.
  (define (add-local type var i)
    (let* ((declarator (if (n-varInit? var)
			   (n-varInit-declaratorName var)
			   var))
	   (id     (n-declaratorName-id declarator))
	   (mdfs   (or (and (n-typespecifier-final type) '(final)) '()))
	   (src    (n-declaratorName-src declarator))
	   (idMt   (lupL-up table id 'non-method)))
      
      (if (and idMt (not (typeUndef? (mt-type idMt))))
	  (sem-err #f src id" is already a local variable or parameter.")
	  ;; The type of a var is a combination of typeSpecifier and
	  ;; the declarator.
	  (let* ((typeVar (create-tyA 
			   (+ (n-typeSpecifier-dim type)
			      (n-declaratorName-dim declarator))
			   (n-typeSpecifier-typeName type)
			   table src))
		 (mt     (add-name-table! table id typeVar 'local mdfs src))
		 (access (make-varAccess i (T->jvmType typeVar) #f #f #f)))
	    (set-n-localVarDecl-type! varsDecl typeVar)
	    (set-mt-access! mt access)
	    
	    (if (n-varInit? var)
		(begin
		  (set-n-varInit-access! var access)
		  (ver-initializer! table var mt Ctype #f)))
	    ))))

  (match varsDecl (($ n-localVarDecl type vars src)
    (let* ((nbWords   (make-list (type->nbWords (n-typeSpecifier-typeName type)) 
				 (length vars)))
	   (totalNbWords (foldl + 0 nbWords))
	   (positions    (list-scan2 + (Ltable-nbWords table) nbWords)))

      (for-each (c.1 add-local type) vars positions)
      ;; The number of words used has increased.
      (set-Ltable-nbWords! table (+ (Ltable-nbWords table) totalNbWords))
      ;; Maintain the maximum number of words used in the method-type.
      (set-tyM-nbWords! Mtype (max (tyM-nbWords Mtype) (Ltable-nbWords table)))
      '(() #t #t)
      ))))


;; I: mdfs, [sym]
;;    dim, int.
;;    name, (u Type (s n-qName) sym). This could be a type.
;;    final, bool
;; O: (s tyA)
;;
(define (create-tyA dim name table src)
  (let* ((simplified-name (if (n-qName? name) (n-qName-name name) name))
	 (type   (res-type! simplified-name table src)))
    (if (= dim 0) type	(make-tyA dim type))))

;; Search for a type, it should be the name of a type accessible
;; through the global table Gtable. This means that if it is not
;; directly in the global table, it is searched using the lazy import
;; list and the file system, and added to it. This is done by the
;; function `lup'.
;;
;; I : type-qName, (u Primitive qName Type (s mt)(s type)(s e-r)).
;;     T, (u (s Gtable) (s Ltable) (s Ttable) (s type))
;;     src, (u #f (s n-src))
;; O : (u Type (s mt))
;;
(define (res-type! type-qName T src)
  (if (not type-qname) (error "CE, res-type! lost type!"))
  (let* ((Gtable (get-Gtable T)))
    (cond
     ((e-r? type-qName)
      (set-e-r-t! (res-type! (e-r-t type-qName) Gtable src)))
     ((or (symbol? type-qName) (type-null? type-qName) (type? type-qName))
      ;; This is a Primitive, (s type), or null. Nothing to change.
      type-qName)
     ((tyA? type-qname)
      (set-tyA-type! type-qname (res-type! (tyA-type type-qname) Gtable src))
      type-qName)
     ((mt? type-qName)
      (set-mt-type! 
       type-qName (res-type! (mt-type type-qName) Gtable (or src (mt-src type-qName))))
      type-qName)
     (else
      ;; It should be the name of a type (class or interface), a qName.
      (let* ((mt (lup Gtable type-qName #f 'type src)))
	(mt-type mt))))))

(define (type-cloneable? type Gtable)
  (or (T==? type '("java" "lang" "Cloneable"))
      ;; Is this correct? TBF
      (subInterface? type 
		     ;; Should probably put a more specific src information
		     (res-type! '("java" "lang" "Cloneable") Gtable LIB-SRC))))

(define (type-null? type)  (eqv? type 'null))

(define (typeUndef? type)
  (or (eqv? type 'undefined) (and (tyA? type) (typeUndef? (tyA-type type)))))

;; A reference type is a class, an interface, or an array. (4.3)
(define (type-ref? type)
  (if (or (string? type) (pair? type))
      (error "CE, type-ref?, incorrect use with type " type))
  (or (type? type) (tyA? type)))

;; We also include the null type for this one.
(define (type-ref2? type) (or (type-ref? type) (type-null? type)))

;; Initializers can occur on field or local variable declarations.
;; The variable has already been inserted in the table, the initializer
;; must be verified for semantic errors and if its a constant expression, it
;; must be computed and attached to the variable in the table.
;; For fields in interfaces consult 9.3.1.
;;
;; I: table, (u (s Ltable) (s Ttable)).
;;    var, (s n-varInit).
;;    mt (s mt). The variable mt structure in the table.
;;    Ctype, (s type) type where var occurs.
;; O: void
;;
(define (ver-initializer! table var mt Ctype postC?)
  (match var (($ n-varInit varDecl initializer src1)
    (match varDecl (($ n-declaratorName id dim src2)
      (table-value-set! table id var)
      (if (n-arrayInit? initializer)
	  ;; ARRAY. Var must be an array.
	  (if (not (tyA? (mt-type mt)))
	      (sem-err #f src2 "Variable "id" is not an array.")
	      (ver-arrayInit! initializer (mt-type mt) table (var-static? mt) Ctype
			      postC?))
	  ;; EXPRESSION
	  (let* (;; If it is a static var, the initializer goes into
		 ;; <clinit> a static method; otherwise it goes into a
		 ;; constructor, a non static method.
		 (Ltable (make-Ltable '() table 0 table))
		 (Mtype  (if (var-static? mt)
			     (make-tyM "<clinit>" '(static) #f #f #f Ltable Ctype #f #f #f)
			     (make-tyM "<init>" '() #f #f #f Ltable Ctype #f #f #f)))
		 (e-r    (ver-expr Ltable initializer Mtype Ctype postC?))
		 (value  (e-r-v e-r)))

	    ;; If the value is really a constant to be put in the constant pool,
	    ;; change the n-varInit for the value.
	    (if (and value (var-final? mt)) (table-value-set! table id value))

	    (set-n-varInit-initializer! var e-r)
	    (if (not (type-=? (mt-type mt) (e-r-t e-r) value))
		(sem-err #f src1 
		 "The type of the initializer is incompatible with var "id"."
		 #\newline
		 "Type of "id" is: "(T->s (mt-type mt))"." #\newline
		 "Type of initializer is: "(T->s (e-r-t e-r))"."
		 ))))
      )))))

;; The semantic verification of an array initializer calls for a verification
;; of every expression in the initializer list.
;; Consider the cases:
;;             int V[]   = {1,2,3,4};
;;             int V[][] = {{1,2}, {3,4}};
;;             int V[]   = {1, 2, 4.7}; // error
;;             int V[][] = {{2,3}, new int[100]};
;;
;; I: init, (s n-arrayInit)
;;    Ta, (s tyA). The type of the corresponding initializer n-arrayInit
;;    static?, #t iff the variable to init is static.
;; O: void
;;
(define (ver-arrayInit! init Ta table static? Ctype postC?)
  (let* ((listExprs (ver-type-arrayInit init Ta table static? Ctype postC?)))
    ;; Replace the list by the list of analyzed expressions.
    (set-n-arrayInit-exprs! init listExprs)
    (set-n-arrayInit-type!  init Ta)
    init))

;; Verify that every expression is type assignable to the variable.
;; This takes into account the nested structure of the initializer.
;;
;; I: init, (s n-arrayInit)
;;    Ta, (s tyA). The type of the array to initialize.
;;
;; O: T = [(u T (s n-e-r))]
;;    The list has the same structure as the nested array declarations.
;;
(define (ver-type-arrayInit init Ta Ttable static? Ctype postC?)
  (let* ((Tap  (subarray-type Ta)))
    (map 
     (lambda (expr) ;; expr can be an expression or a n-arrayInit
       (if (n-arrayInit? expr)
	   (if (not (tyA? Tap))
	       (sem-err '() (n-arrayInit-src expr) "Too many nested arrays.")
	       ;; This is a legal nested array initializer.
	       (ver-arrayInit! expr Tap Ttable static? Ctype postC?))
	   ;; It is a simple expression
	   (let* (;; For a static var, the initializer goes into
		  ;; <clinit>; otherwise it goes into every
		  ;; constructor <init>, which are non-static.
		  (Ltable (make-Ltable '() Ttable 0 Ttable))
		  (Mtype  (if static?
			      (make-tyM "<clinit>" '(static) #f #f #f Ltable Ctype #f #f #f)
			      (make-tyM "<init>" '() #f #f #f #f Ltable Ctype #f #f)))
		  (e-r (ver-expr Ltable expr Mtype Ctype postC?)))
	     (if (not (type-=? Tap (e-r-t e-r) (e-r-v e-r)))
		 (e-r-error (e-r-src e-r)
			    "Initializer expression not type assignable." #\newline
			    "Type of inner expression: "(T->s (e-r-t e-r)) #\newline
			    "Subtype of variable  : "(T->s Tap))
		 e-r))))
     (n-arrayInit-exprs init))))

;; I: table,
;;    throws, [QualifiedName]
;; O:
;;
(define (det-names-throws table throws)
  ;; className, n-qName
  (define (det-name-className className)
    ;; A class thrown must be a throwable class (8.4.4), 14.16
    ;; TBF
    #f)
  (for-each det-name-className throws))

;; This is the simplest table search. It does not access the hierarchy
;; of types and not the up-table, neither the file system.
;;
;; I: table, (u (s Gtable) (s Ttable) (s Ltable))
;;    qName,  (u str [str])
;;    kind,  (u 'type 'field 'method 'non-method). 
;; O: (u #f (s mt) [(s mt)])
;;    It returns a list of (s mt) only if the kind is
;;    method; otherwise it returns the first mt. 
;;
(define (lupL table qName kind)
  (cond
   ((and (pair? qName) (= 1 (length qName)))  (lupL table (car qName) kind))
   ((eqv? kind 'method)
    (filter (lambda (mt) (eqv? 'method (mt-kind mt)))
	    (map cdr (:~-multiple qName (Ttable-names table)))))
   ((eqv? kind 'type)   (:~. qName (table-names (get-Gtable table))))
   ((eqv? kind 'field)  
    (:~. qName (filter (lambda (qmt) (eqv? 'field (mt-kind (cdr qmt))))
		       (table-names table))))
   (else
    (:~. qName (filter (lambda(qmt) (not (eqv? 'method (mt-kind (cdr qmt)))))
		       (table-names table))))))

;; Search only the chain of local tables, and not the type table at
;; the end of the chain of up-tables.
;;
;; I: table, (u #f (s Ltable) (s Ttable) (s Gtable))
;; O: (u #f (s mt))
;;
(define (lupL-up table qName kind)
  (if table
      (let* ((var (or (lupL table qName kind)
		      (lupL-up (table-up-table table) qName kind))))
	var)
      #f))

;; Return the methods that are accessible.
(define (accessible mts)
  (filter (lambda (mt) (not (member 'private (tyM-mdfs (mt-type mt))))) mts))

;; Resolve any type names from extensions to their type structure.
;; If it is a type coming from the source file, nothing has to be done
;; since all extensions have been resolved.
;;
;; I: type, (s type).
;; O: void.
(define (resolve-extend-implements! type)
  (if (not (type-inside? type))
      (let* ((ints  (and (pair? (type-interfaces type))
			 ;; TBF LIB-SRC should be modified.
			 (map (lambda (type) (res-type! type type LIB-SRC))
			      (type-interfaces type))))
	     (ext   (and (type-extend type)
			 (res-type! (type-extend type) type LIB-SRC))))
	
	(set-type-interfaces! type (or ints '()))
	(set-type-extend!     type ext))))

;; Search for a field id in a type and its supertypes taking into
;; account accessibility. Do not access the file system.
;;
;; Always return at most one mt structure, reports an error if
;; multiple fields of coming from different types can be reached.
;; This is ambiguity: consult 9.3.2.1 and 9.3.2.2.
;;
;; I: type, (u (s type) (s tyA)).  
;;    id, str. Field name to search.
;;    local?, #t iff a local search should also be done.
;;    fromT, (s type). The type where the reference occurs.
;;    preT, #f iff no prefix before id. Otherwise it is the type of the prefix.
;;             This information is necessary for protected accessibility.
;;    src, the source node for the id.
;;
;; O: (u #f (s mt)),  #f iff no id is found.
;;
(define (lupT-f type id local? fromT preT src)
  
  ;; It is ambiguous if the field comes from different classes.
  (define (ambiguous? mts)
    (and (> (length mts) 1) 
	 (> (length (set (map (c* varAccess-className mt-access) mts))) 1)))
  
  (cond 
   ((type? type)
    (let* ((table  (type-table type))
	   (r      (and local? (lupL table id 'field)))
	   (rr     (and r (ver-access (res-type! r table LIB-SRC) type fromT preT))))
      (if r 
	  rr
	  ;; The field id is not local, so search upward.
	  (begin
	    (resolve-extend-implements! type)
	    (if (or (type-extend type) (pair? (type-interfaces type)))
		(let* ((ext (if (type-extend type)
				(: (type-extend type) (type-interfaces type))
				(type-interfaces type)))
		       (mts  (map (lambda (e) (lupT-f e id #t fromT preT src)) 
				  ext))
		       (mts2 (elem-all-eqv #f mts)))
		  (cond
		   ((null? mts2) #f)
		   ((ambiguous? mts2)
		    (sem-err MT-UNDEF src "Field reference "id" is ambiguous from classes and/or interfaces: "
			     (++del (map (lambda (mt) 
					   (qName->s (varAccess-className (mt-access mt)))) mts2)", ")"."))
		   (else (res-type! (car mts2) table LIB-SRC))))
		#f)))))
   ((tyA? type)
    (if (string=? id "length")
	(make-mt "length" 'int 'field #f src '() 'arraylength)
	#f))
   (else  (error "CE, lupT-f, unknown case: " type))))

;; Return mt, for field, method or constructor, if it is accessible
;; from frmT, being defined in type T.  If the field or method
;; is accessed through a prefix, preT is the type of that prefix.
;;
;; See 6.6.
;; I: mt, (s mt)
;;    inT, fromT, (s type).
;;    preT, (u #f #t (s type)).
;;           #f, means no prefix, #t means it is an allocation using `new'.
;; O: (u #f (s mt))
(define (ver-access mt T frmT preT)

  (define (ver-package) (and (var-default? mt) (same-pack? frmT T)))
  (define (ver-private) (and (var-private? mt) (T==? frmT T)))

  ;; See 6.6.1 and 6.6.2 for details.
  (define (ver-protected)
    (and (var-protected? mt)
	 (or (T==? T frmT) (same-pack? T frmT)
	     (or (and (not preT) (subClass? frmT T (get-Gtable T)))
		 (and (type? preT)
		      (or (T==? preT frmT) (subClass? preT frmT (get-Gtable T))))))))

  (if (and mt (or (var-public? mt) (ver-package) (ver-private) (ver-protected)))
      mt #f))

;; This is a full search. It uses the table, the hierarchy of a class
;; or interface, and the file system if necessary.  Which means if an
;; interface or class is searched, and the type has not be found in
;; the table, it must be searched in the lazy import list and, if this
;; fail, in the file system according (both using CLASSPATH).
;;
;; This function is not called to search for a method. It is done
;; using lupT-m or lupL. If name should refer to a type, this function
;; should not be called with a local table (that is a Ltable), since a
;; local variable or a field could accidentally shadow a type name. In
;; that case, the correct procedure is to use a global table,
;; bypassing all local variables, parameters, and fields.
;;
;; I: table, (u (s Ltable) (s Gtable) (s Ttable))
;;    qName, (u str [str])
;;    type, (u #f (s type)) 
;;          #f if kind is 'type (no hierarchy is to be searched).
;;    kind, (u 'type 'non-method) 
;;          type means that only a type should be returned, nothing else.
;;          non-method means anything but a method, so it includes type.
;;          
;; O: (u (s mt) (: (s mt) access))
;;    It is (s mt) if kind is 'type
;;    The access is [(s varAccess)]
;;
(define (lup table qName type kind src)

  ;; I: id, str.
  ;; O: (u #f (cons (s mt) (s var-access))) 
  (define (localVariable id)
    (let* ((varLocal (lupL-up table id 'non-method)))
      (and varLocal (: varLocal (mt-access varLocal)))))

  ;; Is id a field of type? This id comes from either a simple
  ;; variable, or it is the first component of a qualified name.
  ;; So a call to lupT-f specify that id reference does not (#f)
  ;; come from a reference. type is where the reference is made, so it can
  ;; be used by lupT-f.
  ;;
  ;; I: id, str.
  ;; O: (u #f (cons (s mt) (s var-access)))
  (define (fieldVariable id)
    (let* ((varField (and type (lupT-f type id #t type #f src)))
	   (access   (and varField (mt-access varField))))
      (and access (: varField (if (not (varAccess-static? access))
				  (list 'this access)
				  access)))))

  ;; I: qName, (u str [str])
  ;;    type?, #t iff it can be assumed that qName could be a type.
  ;; O: (u #f (s mt))
  (define (aType qName type?)
    (or (lupL table qName 'type)
	(and (package? table) (string? qName)
	     (lup-file-system! (append (get-pack-name table) (list qName))
			       (get-Gtable table)))
	(lup-lazy-imports!  qName (get-Gtable table))
	(lup-file-system!   qName (get-Gtable table))
	(if type?
	    (begin 
	      (add-name-table! (get-Gtable table) qName 'undefined 'undefined '() LIB-SRC)
	      (sem-err MT-UNDEF src "Unknown "qName"."))
	    #f)))

  ;; If the qName prefix is the package name, search in it.
  ;; O: (u #f (s mt))
  (define (inThePackage qName)
    (let* ((pName (get-pack-name table)))
	(and  
	 (prefix=? pName qName)
	 (> (length qName) (length pName))
	 (let* ((idT (list-ref qName (length pName)))
		;; Search in two different ways: with and without the
		;; package name.
		(mtT (or (lupL table idT 'type)
			 (aType (list-head qName (+ 1 (length pName))) #f))))
	   (if mtT
	       `(,(list-head qName (+ 1 (length pName)))
		 ,(list-tail qName (+ 1 (length pName)))
		 ,mtT () ,(mt-type mtT))
	       #f)
	   ))))

  ;; O: (u #f (prefix suffix (s mt) () Type)
  (define (inAPackage qName)
    (let* ((packages (get-acc-packages (get-Gtable table)))
	   (pName    (:~-p (lambda (x) (prefix=? x qName)) packages))
	   (n        (and pName (length pName))))
      (if (and pName (> (length qName) n))
	  (let* ((idT (list-ref qName n))
		 (mtT (or (lup table idT #f 'type src)
			  (aType (list-head qName (+ 1 n)) #f))))
	    (if mtT
		`(,(list-head qName (+ 1 n)) ,(list-tail qName (+ 1 n))
		  ,mtT () ,(mt-type mtT))
		#f))
	  #f)))
  
  ;; I: mt, (s mt)
  (define (resT mt) (res-type! (mt-type mt) table  LIB-SRC))

  ;; Process the prefix of qName such that a type can be assigned to a
  ;; prefix of qName. So if the prefix is a package name, it is
  ;; processed.
  ;;
  ;; I: qName, [str]. It is a qualified name, not a string.
  ;; O: (prefix suffix mt access type)
  ;;    The two lists prefix and suffix form the qName. 
  (define (proc-suffix qName)
    (let* ((case1 (or (localVariable (car qName)) (fieldVariable (car qName))))
	   (case2 (or case1  (aType (car qName) #f)))
	   (case3 (or case1 case2 (aType qName #f)))
	   ;; The next two cases include a package name as a prefix.
	   (case4 (or case1 case2 case3 (inThePackage qName)))
	   (case5 (or case1 case2 case3 case4 (inAPackage qName))))

      (cond
       (case1
	`(,(list (car qName)) ,(cdr qName) ,(car case1) 
	  ,(if (pair? (cdr case1))
	       ;; It is ('this ...)
	       (reverse (cdr case1))
	       (list (cdr case1)))
	  ,(resT (car case1))))
       (case2 `(,(list (car qName)) ,(cdr qName) ,case2 () ,(resT case2)))
       (case3 `(,qName () ,case3 () ,(resT case3)))
       (case4 case4)
       (case5 case5)
       (else 
	(add-name-table! (get-Gtable table) (car qName) 'undefined 'undefined '() LIB-SRC)
	(sem-err #f src "Unknown "(car qName)".")
	`(() () ,MT-UNDEF () undefined))
       )))

  (cond
   ((and (pair? qName) (= 1 (length qName))) (lup table (car qName) type kind src))
   ((eqv? kind 'type)     (aType qName #t))
   ((string? qName)  ;; A simple name and it is a non-method.
    (or (localVariable qName) (fieldVariable qName) (: (aType qName #t) '())))
   (else
    ;; It's a qualified name. This does not include this.id or super.id.
    ;; These are handled through fieldAccess node by ver-expr.
    (let* ((pst   (proc-suffix qName)))
      (let loop ((prefix    (first  pst))
		 (suffix    (second pst)) 
		 (var       (third  pst))
		 (access    (fourth pst))
		 (varType   (fifth pst)))
	(cond
	 ((not (pair? suffix))	    (: var (reverse access)))
	 ((type-ref? varType)
	  (let* ((id       (car suffix))
		 (var      (lupT-f varType id #t type varType src)))
	    (cond
	     ((not var)
	      (sem-err #f src "Unknown field "id" in type "(T->s varType)".")
	      (: MT-UNDEF #f))
	     ((and (null? access)  (not (field-static? var)))
	      ;; If there is no access, the preceding component was a
	      ;; type, therefore it should be a static field for id.
	      (sem-err #f src "Static reference to non static field "id".")
	      (: MT-UNDEF #f))
	     (else
	      (loop (: id prefix) (cdr suffix) var (: (mt-access var) access)
		    (resT var))))))
	 (else
	  (if (not (typeUndef? varType))
 	      (sem-err #f src "Cannot dereference non-type " 
		       (qName->s (reverse prefix))". Its type is " (T->s varType)))
	  (: MT-UNDEF #f))
	 )))
    )))

;; Chapter 16. TBF
(define (ver-definite-assigment)
  #f
  )

;; I: T1, T2, (s type)
;; O: #t iff T1 and T2 are from the same package.
;; TBF to verify if it works with the names only.
(define (same-pack? T1 T2)
  (let* ((name1 (type-name T1))
	 (name2 (type-name T2))
	 (l1    (if (pair? name1) (length name1) 1))
	 (l2    (if (pair? name2) (length name2) 1)))
    (or (and (= l1 1) (= l2 1))
	(and (= l1 l2)(equal? (list-but-last name1) (list-but-last name2))))))

(define (var-public?    mt) (member 'public (mt-mdfs mt)))
(define (var-private?   mt) (member 'private (mt-mdfs mt)))
(define (var-protected? mt) (member 'protected (mt-mdfs mt)))
(define (var-default?   mt) 
  (not (intersect? '(private protected public) (mt-mdfs mt))))
(define (var-static? mt) (member 'static (mt-mdfs mt)))
(define (var-final? mt)  (member 'final (mt-mdfs mt)))
;; mt, (s mt)
(define (field-static? mt)  (and (mt-access mt) (varAccess-static? (mt-access mt))))

(define MT-UNDEF
  (make-mt #f 'undefined #f #f (make-n-src 0 0) '() (make-varAccess #f #f #f #f #f)))

;; A source identification of a class file.
(define LIB-SRC  (make-n-src 0 0))

;; I: Gtable, (s Gtable)
;; O: [[str]]. This is a list of package names.
;; TBF to include all possible packages from the CLASSPATH
(define (get-acc-packages Gtable)  (Gtable-imports Gtable))

;; Search for type name qName in the lazy import lists.
;;
;; The table has an explicit list of imported packages. The variable
;; CLASSPATH should be consulted for a list of directories where those
;; packages could be located. This look up is done only if the qName does
;; not exist in the table. If a correct class file is found, the type
;; will be added in the table under the qName.
;;
;; I: qName, (u str [str])
;;    Gtable (s Gtable)
;; O: (u #f (s mt))
;;    #f iff the qName cannot be found.
;; 
(define (lup-lazy-imports! qName Gtable)
  (trace "lup-lazy " qName)
  (let loop ((l (Gtable-imports Gtable)))
    (if (pair? l)
	(let* ((full-qName (append (if (string? (car l)) (list (car l)) (car l))
				   (if (string? qName) (list qName) qName)))
	       (dir         (qName->classfName (car l)))
	       ;; Does this thing make sense when qName is composite? TBF
	       (file-class-name  (++ (qName->s qName) ".class"))
	       (full-path-name (search-classpath-file CLASSPATH dir file-class-name))
	       (classStructure (and full-path-name (read-class-dir "" full-path-name)))
	       (type           (and classStructure (class->ast classStructure Gtable))))
	  (trace "Trying dir " dir ". " file-class-name)
	  (trace "full-path-name " full-path-name)
	  (if type
	      (begin
		;; We should probably create a n-src with a more appropriate
		;; information. We add name in both forms.
		(add-name-table! Gtable full-qName type 'type (type-mdfs type) LIB-SRC)
		(let* ((mt (add-name-table! Gtable qName type 'type (type-mdfs type) LIB-SRC)))
		  (extName->type! Gtable type)
		  (iNames->type! Gtable type)
		  mt))
	      (loop (cdr l))))
	#f)))

;; Search for qName as a class in the file system.
;;
;; The variable CLASSPATH is used. This function is called, since the
;; name could not be found in the table or as indicated by the
;; imports. 
;;
;; I : qName, (u str [str])
;; O : (u #f (s mt))
;;
(define (lup-file-system! qName Gtable)
  (trace "lup-file-system! " qName)
  (let* ((file-class-name  (++ (qName->classfName qName) ".class"))
	 (full-path-name   (search-classpath-file CLASSPATH "" file-class-name))
	 (classStructure   (and full-path-name (read-class-dir "" full-path-name)))
	 (type             (and classStructure (class->ast classStructure Gtable))))

    (if type
	(begin
	  ;; Is it done correctly since it is not using the name in
	  ;; the class file?  We should probably create a n-src
	  ;; with a more appropriate information.

	  ;; Add type name only without package name.
	  (if (pair? qName)
	      (add-name-table! Gtable (list-last qName) type 'type (type-mdfs type) LIB-SRC))
	  ;; Add full name.
	  (let* ((mt (add-name-table! Gtable qName type 'type (type-mdfs type) LIB-SRC)))
	    (extName->type! Gtable type)
	    (iNames->type!  Gtable type)
	    mt))
	#f)))

;; Function to add names to the table. The labels are kept in a
;; different list. It is not the responsability of this function
;; to handle multiple definition. The local kind includes the parameters.
;;
;; I: table, (u (s Gtable) (s Ttable) (s Ltable))
;;    qName, (u str [str])
;;    type,  Type.
;;    kind,  (u 'field 'method 'local 'undefined 'type 'package).
;; O: (s mt)
;;    Add name qName to table
;;
(define (add-name-table! table qName type kind mdfs src)
  ;; It is possible to have a qName with different types and kind.
  ;; It is even possible to have a qName with several times the
  ;; same kind, since several methods may have the same qName.
  (let* ((qmt (make-mt qName type kind #f src mdfs #f)))
    ((cond
      ((Ltable? table) set-Ltable-names!)
      ((Gtable? table) set-Gtable-names!)
      ((Ttable? table) set-Ttable-names!))
     table (: (: qName qmt) (table-names table)))
    qmt))

(define (table-value-set! table qName value)
  (let* ((var (:~. qName (table-names table))))
    (if (not var)(error "CE, table-value-set! var must be in table " qName))
    (set-mt-value! var value)))

;; Assignable is true iff an expression can be assigned to a variable.
;; If the two types are equal, it is assignable; it is also
;; assignable if a widening operation can be done on primitive types
;; or reference type.  Moreover, if the expression is a constant
;; expression, a narrowing conversion is allowed if its type is int
;; and its value is in the range of the variable.  See (5.2) and
;; (2.6.6) for more details.
;;
;; I: T, the type of the variable
;;    S, the type of the expression
;;    value, (u #f int) the value of the expression if type T is an int.
;;
;; O: bool, #t iff type S is assignable to type T.
;;
(define (type-=? T S value)
  (or (typeUndef? T) (typeUndef? S)
      (T==? T S) (prim-widening? S T) (reference-widening? S T)
      (and value (T==? S 'int) (prim-narrowing? 'int T) (in-range? value T))))

;; Is S a subclass of T? (TBF. It is probably the case that S and T does not
;; need res-type!. This should be verified. It could eliminate
;; the need of Gtable).
;;
;; I: S, (u qName (s type))
;;    T, (u qName (s type))
;;    Gtable, (s Gtable)
;; O: #t iff S is a subclass of T
;;
(define (subClass? S T Gtable)
  (let* ((extend     (type-extend (res-type! S Gtable LIB-SRC)))
	 (Tr         (res-type! T Gtable LIB-SRC))
	 (typeExtend (and extend (res-type! extend Gtable LIB-SRC))))
    (or (eq? Tr typeExtend) (and typeExtend (subClass? typeExtend Tr Gtable)))))

;; Does C implement I?
;;
;; I: C, (s type)
;;    I, (s type)
;; O: #t iff C implements I
;;    
(define (implements? C I)
  (let* ((interfaces (type-interfaces C))
	 (extend     (type-extend C))
	 (typeExtend (and extend (res-type! extend C LIB-SRC)))
	 (typeInterfaces (map (lambda (x) (res-type! x C LIB-SRC)) interfaces)))
    (or (p-exists? (lambda (x) (eq? I x)) typeInterfaces)
	(subInterface? typeInterfaces I)
	(and typeExtend (implements? typeExtend I)))))

;; Is there one of the interfaces Is a sub-interface of I?
;; I: Is, [(s type)]
;;    I, (s type)
;; O: #t iff one of interfaces is a subinterface of I.
;;
(define (subInterface? Is I) (p-exists? (c.2 one-subInterface? I) Is))

;; Is interface I1 a sub-interface of I2?  Unlike classes, interface
;; may extends several interfaces.
;;
;; I : I1, I2, (s type)
;; O : #t iff I1 is a sub-interface of I2.
;;
(define (one-subInterface? I1 I2)
  (let* ((interfaces (map (lambda (x) (res-type! x I1 LIB-SRC)) (type-interfaces I1))))
    (or (p-exists? (c.2 eq? I2) interfaces)
	(p-exists? (c.2 one-subInterface? I2) interfaces))))

;; I: v, (u int float)
;;    pt, (u 'byte 'char 'short 'int 'long 'float 'double 'undefined)
;; O: #t iff the value v is in the range of the type pt.
;;
(define (in-range? v pt)
  (or (typeUndef? pt) 
      (case pt
	((byte) (and (>= v -128) (<= v 127)))
	((char) (and (>= v 0) (<= v 65535)))
	((short)(and (>= v -32768) (<= v 32767)))
	((int)  (and (>= v -2147483648) (<= v 2147483647)))
	((long) (and (>= v -9223372036854775808) (<= v 9223372036854775807)))
	;; The problem with the following tests is their precision in the
	;; implementation.  Moreover, it refuses the denormalized cases.  TBF
	((float)  (or (= v 0) (and (<=  3.4028235E-38 (abs v)) 
				   (<= (abs v) 3.4028235E38))))
	((double) (or (= v 0) (and (<= 1.7976931348623157E-308 (abs v)) 
				   (<= (abs v) 1.7976931348623157E308))))
	(else (error "CE, in-range?, unknown primitive type " pt v)) 
	)))

;; This must be applied after an operation is performed between
;; integer constant expressions. 
;;
;; I: v, (u int float)
;;    t, (u 'int 'long 'float 'double)
;; O: truncated v to its type.
(define (limit-range v t)
  (let* ((n  (type->nbBytes t)))
    (case t
      ((byte char short) (bytes->int (int->bytes v n n)))
      ((int) (if (and (<= -2147483648 v) (<= v 2147483647))
		 v
		 ;; This truncates the value to n bytes.
		 (bytes->int (int->bytes v n n))))
      ((long) (if (and (>= v -9223372036854775808) (<= v 9223372036854775807))
		  v
		  (bytes->int (int->bytes v n n))))
      ;; TBF for float and double.
      ((float double) v)
      (else (error "CE, limit-range, should not be type " t))
      )))

;; O : #t iff t is an array type.
;; Is it correct according to the casting verification? TBF
(define (type-numeric? t)  (member t '(byte char short int long float double)))
(define (type-integral? t) (member t '(byte char short int long)))
(define (type-int? t)      (eqv? t 'int))
(define (type-boolean? t)  (eqv? t 'boolean))
(define (type-primitive? t)
  (member t '(boolean byte char short int long float double)))

;; Applying (5.6.1). Essentially, a long, float and double should stay
;; that way,  everything else becomes int.  
;; I: t, sym
(define (unary-prom t) (if (member t '(char short byte)) 'int  t))

;; Apply binary numeric promotion as described in (5.6.2)
;; I: t1 and t2 are numeric types
(define (binary-numeric-promotion t1 t2)
  (if (or (not (symbol? t1)) (not (symbol? t2)))
      (error "CE, binary-numeric-promotion called with non symbol " t1 t2))
  (cond 
   ((or (eqv? t1 'double) (eqv? t2 'double)) 'double)
   ((or (eqv? t1 'float) (eqv? t2 'float))   'float)
   ((or (eqv? t1 'long) (eqv? t2 'long))     'long)
   (else  'int)))

;; See 5.1.2.
;; I: p1, a primitive type. (char byte short int long float double)
;;    p2, a primitive type
;;
;; O: #t iff p1 can be widened to p2.
;;
(define (prim-widening? p1 p2)
  (and (type-primitive? p1) (type-primitive? p2)
       (case p1
	 ((byte)  (member p2 '(short int long float double numeric)))
	 ((short) (member p2 '(int long float double numeric)))
	 ((char)  (member p2 '(int long float double numeric)))
	 ((int)   (member p2 '(long float double numeric)))
	 ((long)  (member p2 '(float double numeric)))
	 ((float) (member p2 '(double numeric)))
	 (else	  #f))))

;; See 5.1.3
;; I: p1, a primitive type : char byte short int long float double String
;;    p2, a primitive type
;;
;; O: #t iff p1 can be narrowed to p2.
;;
(define (prim-narrowing? p1 p2)
  (and (type-primitive? p1) (type-primitive? p2)
       (case p1
	 ((byte)   (member p2 '(char numeric)))
	 ((short)  (member p2 '(byte char numeric)))
	 ((char)   (member p2 '(byte short numeric)))
	 ((int)    (member p2 '(byte short char numeric)))
	 ((long)   (member p2 '(byte short char int numeric)))
	 ((float)  (member p2 '(byte short char int long numeric)))
	 ((double) (member p2 '(byte short char int long float numeric)))
	 (else  #f))))


;; Can S be widened to T? See (5.1.4).
;;
;; I: S, anything
;;    T, anything
;; O: #t iff S and T are reference types and S can be widened to T.
(define (reference-widening? S T)
  (and (type-ref2? S) (type-ref? T)
       (or
	(type-null? S)
	(and (type-class? S)
	     (or (and (type-class? T)     (subClass? S T (get-Gtable S)))
		 (and (type-interface? T) (implements? S T))))
	(and (type-interface? S)
	     (or (and (type-class? T)     (T==? T '("java" "lang" "Object")))
		 (and (type-interface? T) (one-subInterface? S T))))
	(and (tyA? S)
	     (or (and (type-class? T)     (T==? T '("java" "lang" "Object")))
		 (and (type-interface? T) (type-cloneable? T))
		 (and (tyA? T) 
		      (reference-widening? (subarray-type S) (subarray-type T)))))
	)))

;; I: T, (s tyA)
;; O: a type for which the dimension of T has been reduced by 1.
(define (subarray-type T)
  (if (> (tyA-dim T) 1)
      (make-tyA (- (tyA-dim T) 1) (tyA-type T))
      (tyA-type T)))

;; I : type, (s type)
;; O: #t iff the class is a final class.
(define (final-class? type)  (member 'final (type-mdfs type)))

;; I: anything
;; O: #t iff type is an interface.
(define (type-interface? type)  (and (type? type) (not (type-cl? type))))

;; I: anything
;; O: #t iff type is a class.
(define (type-class? type) (and (type? type) (type-cl? type)))
  
;; T1, Type=(u sym str (s tyA) (s type) qName)
;; T2, Type
(define (T==? T1 T2)
  (cond
   ((and (symbol? T1) (symbol? T2))     (eqv? T1 T2))   
   ((and (tyA? T1) (not (tyA? T2)))
    (and (= (tyA-dim T1) 0) (eqv? (tyA-type T1) T2)))
   ((and (tyA? T2) (not (tyA? T1)))
    (and (= (tyA-dim T2) 0)  (eqv? (tyA-type T2)  T1)))
   ((and (tyA? T1) (tyA? T2))
    (and (= (tyA-dim T2) (tyA-dim T1)) (T==? (tyA-type T2) (tyA-type T1))))
   ((or (symbol? T1) (symbol? T2)) (eqv? T1 T2))
   ((or (qName? T1) (qName? T2))
    ;; One of them is a name but the other one might be a type.
    (cond ((and (qName? T1) (qName? T2))  (equal? T1 T2))
	  ((and (qName? T1) (type? T2))   (equal? T1 (type-name T2)))
	  ((and (qName? T2) (type? T1))   (equal? T2 (type-name T1)))
	  (else  #f)))
   ((and (type? T1) (type? T2))   (eq? T1 T2))
   (else (error "CE, T==? unknow case " T1 T2))
   ))

;; I: type, a primitive type.
;; O: 1, 2, 4, or 8. The number of bytes used by the primitive type.
(define (type->nbBytes type)
  (if (tyA? type)
      (if (= (tyA-dim type) 0)
	  (type->nbBytes (tyA-type type))
	  (error "CE, type->nbBytes called with a non primitive type " type))
      (if (symbol? type)
	  (:~. type '((int . 4) (short . 2) (char . 2) (byte . 1) (long . 8) 
		      (double . 8) (float . 4) (null . 4) (boolean . 1)))
	  1)))

;; Returns the number of words used by the type. For a reference or
;; array type it is only one word, for long or double 2 words.
;;
;; I: type, Type.
;; O: 1 or 2.
(define (type->nbWords type)
  (if (tyA? type)
      (if (= 0 (tyA-dim type)) (type->nbWords (tyA-type type)) 1)
      (if (and (symbol? type) (member type '(float double))) 2 1)))

;; Generates a string describing the type as it is written in
;; Java. This is mainly used to output in error messages, the internal
;; descriptions of types.
;;
;; I: type, Type
;; O: str
(define (T->s type)

  (define (dim->str dim) (if (= dim 0) "" (++ "[]" (dim->str (- dim 1)))))

  (cond
   ((string? type)     type)
   ((symbol? type)     (symbol->string type))
   ((qName? type)      (qName->s type))
   ((n-qName? type) (qName->s (n-qName-name type)))
   ((tyA? type)
    (match type (($ tyA dim typeName)
      (++ (cond 
	   ((symbol? typeName) (symbol->string typeName))
	   ((string? typeName) typeName)
	   ((qName? typeName)   (qName->s typeName))
	   ((type? typeName)   (T->s (type-name typeName)))
	   (else  (error "CE, T->s, Unknown case " type)))
	  (dim->str dim)))))
   ((type? type) 
    (++C (if (and (not (null? (get-pack-name type))) (type-inside? type))
	     (T->s (get-pack-name type)) "")
	 "/"(T->s (type-name type))))
   ((tyM? type) 
    (++ (if (string=? (tyM-mName type) "<init>")
	    (list-last (if (qName? (tyM-classType type))
			   (tyM-classType type)
			   (type-name (tyM-classType type))))
	    (tyM-mName type))
	"("(++del (map t->s (tyM-parm-types type))" ")")"))
   (else (error "CE, T->s, unknown type form " type))))

;; I : t, anything except an improper list.
;; O : #t iff it is a qName.
(define (qName? t)  (or (string? t) (and (pair? t) (all string? t))))

;; Given ("a" "b" "c") returns "a.b.c"
;; Should simply return the string qName if it is already a string
(define (qName->s qName)  (++del qName "."))

;; Given ("a" "b" "c") returns "a/b/c"
;; I: qName, (u str [str])
;; O: str.
;;    Should simply return the string qName if it is already a string
(define (qName->classfName qName)  (++del qName "/"))

;; I: r, anything
;;    src, (u #f (s n-src))
;;    msg, anything.
;; O: r
(define (sem-err r src msg . l)
  (set! *semantic-error* #t)
  (display-l "*" java:source-filename 
	     ":"(or (and (n-src? src) (n-src-line src)) '?)
	     ":"(or (and (n-src? src) (n-src-col src)) '?)
	     "; " msg)
  (if (pair? l) (for-each display l))
  (newline)
  r)

;; Same as sem-err but returns an undefined type as an e-r structure.
(define (e-r-error src msg . l)  (apply sem-err `(,ER-UNDEFINED ,src ,msg ,@l)))

(define ER-UNDEFINED (make-e-r 'undefined #f #t #f #f))

;; Transform a literal into a string according to Java rules.
;;
;; I: v, (u char str int float 'true 'false)
;;    t, (u 'boolean 'char 'int 'float '("java" "lang" "String"))
;; O: a string representation of v according to Java syntax.
;;
;; Is it the correct way to translate the numeric values? TBF.
(define (toString v t) (or (and (string? v)  v) 
			   (and (eqv? t 'char) (string (integer->char v)))
			   (and (or (eqv? v 'true) (eqv? v 'false)) (symbol->string v))
			   (number->string v)))

;; Returns the external Java representation of the operator op.
;;
;; I: (u str symbol)
;; O: (u str symbol)
(define (op->opJava op)
  (if (member op '(or oror))
      (:~. op '((or . "|") (oror .  "||")))
      op))

(define (n-stmt-src stmt)
  (cond
   ((n-label?        stmt) (n-label-src        stmt))
   ((n-localVarDecl? stmt) (n-localVarDecl-src stmt))
   ((n-if?           stmt) (n-if-src           stmt))
   ((n-switch?       stmt) (n-switch-src       stmt))
   ((n-while?        stmt) (n-while-src        stmt))
   ((n-do?           stmt) (n-do-src           stmt))
   ((n-for?          stmt) (n-for-src          stmt))
   ((n-break?        stmt) (n-break-src        stmt))
   ((n-continue?     stmt) (n-continue-src     stmt))
   ((n-return?       stmt) (n-return-src       stmt))
   ((n-throw?        stmt) (n-throw-src        stmt))
   ((n-syn?          stmt) (n-syn-src          stmt))
   ((n-try?          stmt) (n-try-src          stmt))
   ((n-block?        stmt) (n-block-src        stmt))
   ((n-case?         stmt) (n-case-src         stmt))
   ((n-assignment?   stmt) (n-assignment-src   stmt))
   ((n-literal?      stmt) (n-literal-src      stmt))
   ((n-boollit?      stmt) (n-boollit-src      stmt))
   ((n-op?           stmt) (n-op-src           stmt))
   ((n-instanceof?   stmt) (n-instanceof-src   stmt))
   ((n-postExpr?     stmt) (n-postExpr-src     stmt))
   ((n-unary?        stmt) (n-unary-src        stmt))
   ((n-specialName?  stmt) (n-specialName-src  stmt))
   ((n-qName?        stmt) (n-qName-src        stmt))
   ((n-alloc?        stmt) (n-alloc-src        stmt))
   ((n-arrayAccess?  stmt) (n-arrayAccess-src  stmt))
   ((n-fieldAccess?  stmt) (n-fieldAccess-src  stmt))
   ((n-call?         stmt) (n-call-src         stmt))
   ((n-cast?         stmt) (n-cast-src         stmt))
   ((n-question?     stmt) (n-question-src     stmt))
   (else (error "n-stmt-src, unknown node " stmt))))

