;; Mario Latendresse, July 2000
;;
;; Generation of JVM code given a semantically valid type.
;;

;; The main difference between a class and an interface file is the
;; modifiers (access-flags) of the file. The fields of the
;; interface are also implicitly public, static, and final; the methods
;; are implicitly public and abstract (and not static). These modifiers
;; are added automatically.
;;
;; I: type, (s type)
;;    members, [(u (s n-fieldDecl) (s n-initz) (s n-type) 
;;                 (s n-method) (s n-methodInterface) For contract Java]
;;    fileName, str. It should contain the package name and the file name.
;;    packageName, str.
;;
;; O: a structure usable by ecrire-class
;;
(define (type->jvm type members fileName packageName)
  (let* ((magic            #xCAFEBABE)
	 (minor-version    3)
	 (major-version    45)
	 (cp               (vector '() 0))
	 (access-flags     (+ 32 ;; always put the ACC_SUPER flag 
			    (mdfs->access-flags
			     (if (type-interface? type)
				 (: 'interface (type-mdfs type))
				 (type-mdfs type)))))
	 (this-class       (add-typeName! cp (type-name type) packageName))

	 ;; Classes always have extended (Object has been added if necessary)
	 ;; Interface may have no extension, but we add it here towards Object. 
	 (super-class      (add-typeName! 
			    cp (if (not (type-extend type))
				   '("java" "lang" "Object")
				   (type-name (type-extend type)))
			    '("")))
	 (interfaces       (map (lambda (qName) (add-typeName! cp qName '("")))
				(map type-name (type-interfaces type))))
 	 (fieldsInits      (filter (lambda (m) (or (n-fieldDecl? m) (n-initz? m)))
				   members))
	 ;; Fields must be processed before methods to handle
	 ;; initializers code.
	 (l1-l2-fields     (type->fields&Inits cp type fieldsInits))
	 (clinitCode       (car l1-l2-fields))
	 (initCode         (cadr l1-l2-fields))
	 (fields           (caddr l1-l2-fields)) 
	 (methods          (type->methods cp type clinitCode initCode))
	 (attribute-info   (list (attribute-sourceFile cp fileName))))

    (list
     magic  minor-version  major-version 
     ;; The constant pool begins with an empty slot. 
     (list->vector (: 'constante-vide (reverse (vector-ref cp 0))))
     access-flags  this-class      super-class     interfaces
     fields methods  attribute-info
     )))

;; I : mdfs, [sym]
;; O : a 16 bits word representing the modifiers of the type.
(define (mdfs->access-flags mdfs)
  (list-sum
   (map (lambda (mod) 
	  (:~. mod '((public . 1) (private . 2) (protected . 4)
		     (static . 8) (final . 16)  (synchronized . 32)
		     (super . 32) (volatile . 64) (transient . 128)
		     (native . 256) (interface . 512) (abstract . 1024))))
	mdfs)))

;; Add the name of the type to the constant pool. Return index of the constant.
;; This function is used when a type is translated into JVM.
;;
;; I: cp, vector
;;    qName, (u str [str])
;;    packageName, (u str [str]).
;; O: int.
(define (add-typeName! cp qName packageName)
  (let* ((name (++C (qName->classfName packageName) "/" (qName->classfName qName))))
    (add-cp! cp name 'class)))

;; Generate the class structure for fields with code for static and
;; non-static initializers. A Field may have an initializer.  If it is
;; a constant expression, it is an attribute to be placed in the
;; constant pool. A block of code, static or non-static, can be a
;; member of a class or interface. All static (non-static) block codes
;; are gathered in <clinit> (<init>s).
;;
;; I: cp, constant pool.
;;    T, (s type).
;;    fieldsInits, [(s n-fieldDecl) (s n-initz)]
;; O: (list [bcode for <clinit>] [bcode for constructors] [Field])
;;
(define (type->fields&Inits cp T fieldsInits)
  
  ;; I: mt, (s mt).
  ;; O: ([bcode] [bcode] Field)
  (define (mt->field mt)
    (let* ((typeField    (mt-type mt))
	   (mdfs         (mt-mdfs mt))
	   (id           (mt-id mt))
	   (value        (mt-value mt))
	   (descr        (T->jvmPool typeField 'noClass))
	   (staticField? (var-static? mt))

	   ;; An initializer is an expression that must be inserted in
	   ;; an <init> or <clinit> method, but the value field is a
	   ;; value to be placed in the constant pool. This value
	   ;; comes from a final declaration. It must be a primitive
	   ;; type or a String type.
	   (attributes    (if (and value (not (n-varInit? value)))
			      (list (attribute-constantValue value typeField))
			      '())))
      ;; For a static initializer, the code goes in <clinit>
      ;; For a non-static one it goes in every constructor.
      (list
       (if (and staticField? value (n-varInit? value))
	   (varInit->jvm value cp typeField)
	   '())
       
       (if (and (not staticField?) (n-varInit? value))
	   (varInit->jvm value cp typeField)
	   '())
       
       (list (mdfs->access-flags mdfs)  (add-cp! cp id 'utf8)
	     (add-cp! cp descr 'utf8)   (length attributes)   attributes))))

  (define (attribute-constantValue value type)
    (let* ((name-index (add-cp! cp "ConstantValue" 'utf8))
	   (cst-index  (add-cp! cp value type))
	   (att-length 2) ; always 2. (4.7.3)
	   (info       `(constantvalue ,cst-index)))
      (list name-index att-length info)))

  (define (m->code m)  (if (mt? m) (mt->field m) (blockInit->jvm m cp (type-table T))))
  (define (m->mtInit m) 
    (if (n-initz? m) (list m)
	(let* ((vars (n-fieldDecl-vars m))
	       (mts  (map (lambda (id) (lupL (type-table T) id 'field))
			  (map (lambda (x) 
				 (n-declaratorName-id 
				  (if (n-varInit? x) (n-varInit-declaratorName x) x)))
			       vars))))
	  mts)))

  (let* ((mtsInits  (list-append (map m->mtInit fieldsInits)))
	 (codes     (map m->code mtsInits)))
    
    ;; Gather initializer codes and fields attributes.
    (list (list-append (map car codes))
	  (list-append (map cadr codes))
	  (elem-all-eqv #f (map caddr codes)))))

;; Generate bytecoe for block initializer.
;; I: initz (s n-initz)
;; O: ([bcode <clinit>] [bcode <init>] #f)
(define (blockInit->jvm initz cp Ttable)
  (let* ((Ltable (make-Ltable '() #f 0 Ttable))
	 (Mtype  (if (n-initz-static initz)
		     (make-tyM "<clinit>" '(static) #f #f #f Ltable Ttable 0 #f)
		     (make-tyM "<init>"   '() #f #f #f Ltable Ttable 0 #f)))
	 (code   (n-block->jvm (n-initz-block initz) Mtype cp #f #f '())))
    (if (n-initz-static initz) `(,code () #f)	`(() ,code #f))))

;; Generate code for all methods.
;;
;; I: type, 
;;    clinitCode, [bcode]. Static initializer code.
;;    initCode, [bcode]. 
;;    Code from the initializer of fields for every constructor.
;; O: [Method]
(define (type->methods cp type clinitCode initCode)

  ;; I: mt, (s mt). necessarily a method or constructor.
  (define (mt->method mt)
    (let* ((method-type      (mt-type mt))
	   (access-flags     (mdfs->access-flags 
			      (union (if (type-interface? type) '(public abstract) '())
				     (tyM-mdfs method-type))))
	   (name-index	     (add-cp! cp (mt-id mt) 'utf8))
	   (descriptor-index (add-cp! cp (T->jvmPool method-type 'noClass) 'utf8))
	   (insts            (and (n-block? (mt-value mt))
				  (n-block-declStmts (mt-value mt))))
	   (attributes       (if insts
				 (list (code-attribute mt cp initCode))
				 '())))
      (list access-flags name-index descriptor-index (length attributes) attributes)
      ))

  (define (method-clinit)
    (let* ((descriptor       "()V")
	   (access-flags     (mdfs->access-flags '(static)))
	   (name-index	     (add-cp! cp "<clinit>" 'utf8))
	   (descriptor-index (add-cp! cp descriptor 'utf8))
	   (clinitCode2      (append clinitCode (Ijvm 'return #f)))
	   (attributes       (list (clinit-code-attribute clinitCode2 cp)))
	   (lattributes      (length attributes)))
      
      (list access-flags name-index descriptor-index lattributes attributes)))
  
  (let* ((methods (filter (lambda (mt) (eqv? (mt-kind mt) 'method))
			  (map cdr (table-names (type-table type))))))
    (if (pair? clinitCode)
	(: (method-clinit) (map mt->method methods))
	(map mt->method methods))
    ))

;; I: mt, (s mt). The value hold a (s n-block)
;;    initCode, [bcode]. This is the initialization byte code to insert 
;;                       in constructors. This code comes from non static
;;                       field initializers.
;; O: list
(define (code-attribute mt cp initCode)
  (let* ((attribute-name-index   (add-cp! cp "Code" 'utf8))
	 (block                  (mt-value mt))
	 (method-type            (mt-type  mt))
	 (tables-code            (method->jvm block method-type cp initCode))
	 (lineNumberTable        (car tables-code))
	 (exception-table        (cadr tables-code))
	 (exception-table-length (length exception-table))
	 (code                   (caddr tables-code))
	 ;; code is really a list of bytes. Its length is exactly the
	 ;; length of the method.
	 (code-length            (length code))
	 ;; TBF: These two values should come from method->jvm
	 (max-stack              1000)
	 (max-locals             1000)
	 ;; Currently there are two attributes: LineNumberTable
	 ;; and LocalVariableTable (JVMS 4.7.4).
	 (att-lineNumberTable  (attribute-lineNumberTable cp lineNumberTable))
	 ;; TBF: add this table for debuggers.
	 (att-localVariableTable '())
	 (attributes             `(,att-lineNumberTable))
	 (attributes-count       (length attributes))
	 (lineNumberTable-length (length lineNumberTable))
	 (attribute-length
	  (+ code-length 
	     ;; The number 12 counts the space taken by max-stack, max-locals,
	     ;; code-length, attributes-count and exception-table-length.
	     12 
	     ;; 8 counts attribute_name_index, attribute_length and 
	     ;; line_number_table_length of lineNumberTable_attribute
	     (+ 8 (* 4 lineNumberTable-length)) 
	     (* 8 exception-table-length)
	     )))
    
    (if (= code-length 0) (error "CE, code-attribute, code length is zero: " mt))
    (if (> code-length 65534) (error "CE, code-attribute, code length is too long:" code-length))
    (list attribute-name-index attribute-length
	  (list 'code max-stack max-locals code-length code exception-table-length 
		exception-table	attributes-count attributes))
    ))

;; I: table, list of pairs (lineNumber offset)
(define (attribute-lineNumberTable cp table)
  (let* ((name-index (add-cp! cp "LineNumberTable" 'utf8)))
    `(,name-index ,(+ 2 (* 4 (length table))) ;; attribute_length
      (lineNumberTable ,(length table) ;; line_number_table_length
		       ,table))))

(define (clinit-code-attribute clinitCode cp)
  (let* ((attribute-name-index   (add-cp! cp "Code" 'utf8))
	 (codeWithOffsets        (code-label->code-offsets clinitCode))
	 (table-code             (code->code-lineNumberTable codeWithOffsets))
	 (lineNumberTable        (car table-code))
	 (code                   (cadr table-code))
	 (att-lineNumberTable    (attribute-lineNumberTable cp lineNumberTable))
	 (lineNumberTable-length (length lineNumberTable))
	 ;; code is really a list of bytes. Its length is exactly the
	 ;; length of the method.
	 (code-length            (length code))
	 ;; These two values should come from method->jvm
	 ;; TBF
	 (max-stack              1000)
	 (max-locals             1000)
	 ;; TBF
	 (exception-table        '())
	 (exception-table-length (length exception-table))
	 ;; TBF
	 (attributes             `(,att-lineNumberTable))
	 (attributes-count       (length attributes))
	 (attribute-length       
	  (+ code-length 
	     ;; The number 12 counts the space taken by max-stack, max-locals,
	     ;; code-length, attributes-count and exception-table-length.
	     12 
	     (+ 8 (* 4 lineNumberTable-length)) 
	     (* 8 exception-table-length)
	     )))

    (if (> code-length 65534) (error "CE, clinit-code-attribute, code length is too long:" code-length))
    (list attribute-name-index attribute-length
	  (list 'code max-stack max-locals code-length code exception-table-length 
		exception-table	attributes-count attributes))
    ))

;; Source File attribute (JVMS 4.7.2)
(define (attribute-sourceFile cp fileName)
  (let* ((name-index      (add-cp! cp "SourceFile" 'utf8))
	 (fileName-index  (add-cp! cp fileName 'utf8))
	 (att-length      2) ; always 2. (4.7.2)
	 (info            `(sourceFile ,fileName-index)))
    (list name-index att-length info)))

;; I: block, (s n-block)
;;    Mtype, (s tyM). The method type of the method to translate.
;;    initCode, [bcode]. Code to insert in a constructor.
;; O: (tableLine tableExceptions [byte]). 
;;    [byte] is really a list of byte values and no longer of bytecode.
;;    tableLine, [(byteCodeOffset srcLineNumber)]
;;    tableExceptions, [(startOffset endOffset branchOffset cp-classIndex)]
;;
(define (method->jvm block Mtype cp initCode)
  (trace "method->jvm " (tyM-mName Mtype))
  (let* ((code         (Mblock->jvm block Mtype cp initCode))
	 (code2        (code-label->code-offsets code))
	 (table-code3  (code->code-lineNumberTable code2))
	 (tableLine    (car table-code3))
	 (code3        (cadr table-code3))
	 (table-code4  (code->code-exceptionTable code3 cp))
	 (tableExcep   (car table-code4))
	 (code4        (cadr table-code4)))
    `(,tableLine ,tableExcep ,code4)))

;; Build the line number table for the class file.
;;
;; I: [(u byte ... (LineNumber int))]
;;    Bytecode with line number information in the code.
;; O: (table [byte])
;;    table is [(offset src-line-number)]
;;    All LineNumber removed.
(define (code->code-lineNumberTable code)
  (let loop ((l code) (r '()) (i 0) (table '()))
    (if (pair? l)
	(if (lineNumber? (car l))
	    ;; TBF: reduce the size of the table by eliminating repetition.
	    (loop (cdr l) r i (: (: i (cadar l)) table))
	    (loop (cdr l) (: (car l) r) (+ i (if (integer? (car l)) 1 0)) table))
	(list (reverse table) (reverse r))
	)))

;; Build the exception table from try labels.
;;
;; I: code, [(u byte ...  (s tryLabel))]
;; O: (table [byte])
;;    All (s try-Label) have been removed from the code.
;;    table is [(startOffset endOffset catchOffset index-cp)]
;;
(define (code->code-exceptionTable code cp)
  ;; tryLabels, [(label startOffset endOffset)]
  ;; table, [(startOffset endOffset catchOffset index-cp)]
  (let loop ((l code) (r '()) (tryLabels '()) (i 0) (table '()))
    (if (pair? l)
	(if (tryLabel? (car l))
	    (case (tryLabel-kind (car l))
	      ((start) 
	       (loop (cdr l) r 
		     ;; insert new tuple for beginning of a try block
		     (: `(,(tryLabel-label (car l)) ,i #f) tryLabels) 
		     i table))
	      ((end)
	       (let* ((t (:~ (tryLabel-label (car l)) tryLabels))) 
		 ;; set the end offset of this try block.
		 (set-car! (cddr t) i)
		 (loop (cdr l) r tryLabels i table)))
	      ((catch) 
	       (let* ((t       (:~. (tryLabel-label (car l)) tryLabels))
		      (strType (tryLabel-type (car l)))
		      (index   (or (and (not strType) 0)
				   (add-cp! cp strType 'class)))
		      (entry   `(,(car t) ,(cadr t) ,i ,index)))
		 (loop (cdr l) r tryLabels i (: entry table)))))
	    (loop (cdr l) (: (car l) r) tryLabels  
		  (+ i (if (integer? (car l)) 1 0)) table))
	(list (reverse table) (reverse r)))))

;; Replace labels by offset. The lookupswitch instruction encoding details
;; is treated here, in particular the calculation of offset values.
;;
;; O: [(u byte ...)]
(define (code-label->code-offsets code)
  (let* ((offsets (code->offsets code)))
    ;; i is the number of bytes from the start.
    (let loop ((l code) (r '()) (i 0))
      (if (pair? l)
	  (cond
	   ((lineNumber? (car l)) ;; A source line number indicator.
	    ;; Keep it for later construction of the LineNumber table.
	    (loop (cdr l) (: (car l) r) i))
 	   ((label? (car l));; A label as an operand in an instruction.
	    (let* ((label-i  (:~. (label-label (car l)) offsets))
		   (verify   (if (not label-i) 
				 (error "CE, code-label->code-offsets, label unkown: " l)))
		   (offset   (- label-i (- i 1)))
		   (nb-bytes (label-nb-bytes (car l))))
	      (if (not (member nb-bytes '(2 4)))
		  (error "CE, code-label->code-offsets, offset length must be 2 or 4:" nb-bytes))
	      (loop (cdr l)
		    (append (reverse ((if (= nb-bytes 2) int->2bytes int->4bytes) offset))
			    r)
		    (+ i nb-bytes))))
	   ((targetLabel? (car l)) ;; A label as an instruction.
	    (loop (cdr l) r i))
	   ((jvmSwitch? (car l))
	    ;; lookupswitch offset values are computed here.
	    ;; The following element of l is a list of pairs (c . sym). 
	    ;; Where c is a constant value of a case and sym
	    ;; the symbol of a label.
	    (if (null? (cdr l))	(error "CE, Encoding error of switch statement " l))
	    (if (not (:~ 'default (cadr l)))
		(error "CE, missing default in encoding of switch " l))

	    (let* ((operand          (cadr l))
		   (pairs (increasing-sort
			   (filter (lambda (x) (not (eqv? (car x) 'default)))
				   operand)
			   car))
		   (nb-bytes-padding (remainder (- 4 (remainder (+ i 1) 4)) 4))
		   (inst-length      (jvmSwitchLength operand i)))
	      (loop
	       (cddr l)
	       (append
		(reverse 
		 (append
		  (list 171) ;; code-op of lookupswitch
		  (make-list 0 nb-bytes-padding)
		  ;; default offset (TBF only works for lookupswitch)
		  (int->4bytes (- (:~. (:~. 'default operand) offsets) i)) 
		  (int->4bytes (- (length operand) 1)) ;; nbpairs
		  (list-append
		   (map (lambda (c-l)
			  (let* ((c      (car c-l))
				 (label  (cdr c-l))
				 (offset (- (:~. label offsets)  i)))
			    (append (int->4bytes c) (int->4bytes offset))))
			pairs))))
		r)
	       (+ i inst-length))))
	   ((integer? (car l)) ;; normal case of an integer byte.
	    (loop (cdr l) (: (car l) r) (+ i 1)))
	   (else ;; An unknown piece of information, not a byte.
	    (loop (cdr l) (: (car l) r) i)))
	  (reverse r))
      )))

;; O : [(sym . int)] A list of all labels with their byte offset in
;; the code.
(define (code->offsets code)
  (let loop ((l code) (labels '()) (i 0))
    (if (pair? l)
	(cond
	 ((jvmSwitch? (car l))
	  (if (null? (cdr l)) (error "CE, encoding error of switch statement " l))
	  (let* ((operand     (cadr l))
		 (inst-length (jvmSwitchLength operand i)))
	    (loop (cddr l) labels  (+ i inst-length))))
	 ((integer? (car l))  ;; a pure byte.
	  (loop (cdr l) labels (+ i 1)))
	 ((targetLabel? (car l))
	  (loop (cdr l)
		;; insert label in association list.
		(: (: (targetLabel-label (car l)) i) labels)
		i))
	 ((label? (car l))
	  (loop (cdr l)	labels (+ i (label-nb-bytes (car l)))))
	 ;; Everything else is not a byte nor a label.
	 (else  (loop (cdr l) labels i)))
	labels)))

(define (lineNumber? e) (and (pair? e) (eqv? (car e) 'lineNumber)))

;; This function does not work after calling code-label->code-offsets
(define (jvmSwitch? code-op) (eqv? code-op 'switch))

(define (jvmSwitchLength operand offset)
  ;; lookupSwitch only. TBF should use also lookuptable for performance.
  (jvmLookupswitchLength operand offset))

;; I: offset, int. The offset of the code-op of the instruction.
;;                 Necessary to compute the padding length.
(define (jvmLookupswitchLength operand offset)
  (+ 1 ;; op-code
     (remainder (- 4 (remainder (+ offset 1) 4)) 4) ;; padding
     4 ;; default offset
     4 ;; number of pairs 
     (* 8 (- (length operand) 1)) ;; each pair requires 8 bytes
     ;; It is minus one since default is in the operand but is not
     ;; counted by the JVM instruction.
     ))

;; I: block, (s n-block)
;;    Mtype, (s tyM). The method type of the method containing that block.
;; O: [bcode]
(define (Mblock->jvm block Mtype cp initCode)
  (let* ((insts       (n-block-declStmts block))
	 ;; rType should not be #f. Constructors should now have 'void.
	 (rType        (tyM-rType Mtype)))

    (if (not insts)
	'()
	`(,@(let* ((codeFirstInst
		    (if (pair? insts)
			(inst->jvm (car insts) Mtype cp #f #f '() #f)
			'()))
		   (codeRest (or (and (pair? insts)
				      (list-append 
				       (map (lambda (x)
					      (inst->jvm x Mtype cp #f #f '() #f)) 
					    (cdr insts))))
				 '())))

	      (append
	       (if (method-constructor? Mtype)
		   ;; The first instruction is the super call. It always
		   ;; exists since it has been added if it was not specified
		   ;; by the programmer. Here we add the initialization code
		   ;; for the fields.
		   (append codeFirstInst initCode)
		   codeFirstInst)
	       codeRest))
	  
	  ;; A return statement should always be executed to exit a method.
	  ;; Such a statement is actually added automatically here.
	  ;; TBF. It should be conditionally added.
	  ,@(if (not (T==? 'void rType))
		(if (type-ref? rType) 
		    (Ijvm 'aconst_null cp)
		    (Ijvm 'ldc cp rType 0))
		'())
	  ,@(Ireturn cp rType)
	  ))))

;; Generate instruction to store value on top of stack. It is assumed
;; that only the last component of a list of var-access has to be
;; considered.  For example, for a field access a.b.c, the reference
;; to a.b is assumed on the stack just below the value to be stored.
;;
;; I : lhs, (u (s n-qName) (s n-fieldAccess)  (s n-arrayAccess)) 
;;     type-lhs,
;; O : [bcode]
(define (gen-store lhs cp)
  (cond
   ((or (n-qName? lhs) (n-fieldAccess? lhs))
    (let* ((access (if (n-qName? lhs) (n-qName-access lhs) (n-fieldAccess-access lhs)))
	   (varAccess (if (pair? access) (list-last access)  access)))
      (if (varAccess-i varAccess)
	  (Ijvm 'store-local cp (varAccess-i varAccess) (varAccess-jvmType varAccess))
	  ;; It is not a local var, it is a field.  access is 
	  ;; (s varAccess) or [(s varAccess)] but only the last component
	  ;; is used since the object reference is assumed to be just
	  ;; below the value.
	  (gen-store-field varAccess cp))))
   ((n-arrayAccess? lhs)  (Ijvm 'store-array cp (n-arrayAccess-basicType lhs)))
   (else (error "CE, gen-store, unknown case: " lhs))))

;; I: varAccess, (s varAccess)
;; O: [bcode]
(define (gen-store-field varAccess cp)
  (Ijvm (if (varAccess-static? varAccess) 'putstatic 'putfield)
	cp
	(qName->classfName (varAccess-className varAccess))
	(varAccess-fieldName varAccess)
	(varAccess-jvmType varAccess)))

;; I: Mtype, (s tyM). The method type of the method containing that instruction.
;;    bLab, sym. Label to branch to for a break without label.
;;    cLab, sym. Label to branch to for a continue without label.
;;    insts, [inst]
;;    labels, [(kind label labelBreak labelContinue)]
;;    lab?, #t iff there is a label on the instruction inst.
;; O: [bcode]
;;
(define (inst->jvm inst Mtype cp bLab cLab labels lab?)
  (cond
   ((n-emptyStatement? inst)  '())
   ((n-label?    inst) (n-label->jvm     inst Mtype cp bLab cLab labels))
   ((n-break?    inst) (abruptJump->jvm  inst cp bLab labels))
   ((n-continue? inst) (abruptJump->jvm  inst cp cLab labels))
   ((n-return?   inst) (n-return->jvm    inst Mtype cp labels))
   ((n-switch?   inst) (n-switch->jvm    inst Mtype cp labels))
   ((n-while?    inst) (n-while->jvm     inst Mtype cp labels lab?))
   ((n-do?       inst) (n-do->jvm        inst Mtype cp labels lab?))
   ((n-for?      inst) (n-for->jvm       inst Mtype cp labels lab?))
   ((n-if?       inst) (n-if->jvm        inst Mtype cp bLab cLab labels))
   ((n-block?    inst) (n-block->jvm     inst Mtype cp bLab cLab labels))
   ((n-try?      inst) (n-try->jvm       inst Mtype cp bLab cLab labels))
   ((n-localVarDecl? inst) (n-localVarDecl->jvm inst cp))
   ((n-syn?      inst)     (n-syn->jvm inst Mtype cp bLab cLab labels)) 
   ((n-throw? inst)        (n-throw->jvm inst Mtype cp))
   ((or (n-unary? inst) (n-postExpr? inst) (n-call? inst) (n-assignment? inst) 
	(n-alloc? inst))
    (expr->jvm inst Mtype cp #f))
   (else (error "CE, inst->jvm, unknown instruction: " inst))))  

;; I: inst, (n-throw)
(define (n-throw->jvm inst Mtype cp)
  (append (expr->jvm (n-throw-expr inst) Mtype cp #t) 
	  (Ijvms (n-throw-src inst) 'athrow cp)))

(define (n-syn->jvm inst Mtype cp bLab cLab labels)

  ;; Generate short subroutine for nested break, continue, and return
  ;; statements to call to release the lock.
  (define (finish-sub iLocals)
    (append
     ;; Store the return address in a local variable.
     (Ijvm 'astore cp (+ 1 iLocals))
     ;; Get the object on which the lock is set.
     (Ijvm 'load-local cp iLocals 'reference)
     ;; Release the lock.
     (Ijvm  'monitorexit cp)
     ;; Return using the local variable address.
     (Ijvm 'ret cp (+ 1 iLocals))))

  (match inst (($ n-syn expr stmt iLocals src)
    (let* ((labelBlock    (Ijvm 'label cp))
	   (labelEnd      (Ijvm 'label cp))
	   (labelFinally  (Ijvm 'label cp))
	   (labels2    `((finally ,labelFinally) ,@labels))
	   (jvmInsts   (inst->jvm stmt Mtype cp bLab cLab labels2 #f)))

      (if (null? jvmInsts) 
	  (append (expr->jvm expr Mtype cp #t) (Ijvm 'pop cp))
	  (append
	   ;; The synchronized object.
	   (expr->jvm expr Mtype cp #t)
	   (Ijvm 'dup cp)
	   (Ijvms src 'store-local cp iLocals 'reference)
	   (Ijvms src 'monitorenter cp)
	   ;; Mark the beginning of the synchronized block.
	   (list (make-tryLabel labelBlock 'start #f))
	   jvmInsts
	   (Ijvms src 'load-local cp iLocals 'reference)
	   ;; Mark the end of the synchronized block.
	   (list (make-tryLabel labelBlock 'end #f))
	   (Ijvms src 'monitorexit cp)
	   ;; Jump outside the synchronized statement.
	   (Ijvm 'goto cp (make-label labelEnd 2))
	   ;; The any exception block
	   (list (make-tryLabel labelBlock 'catch #f))
	   (Ijvms src 'load-local cp iLocals 'reference)
	   (Ijvms src 'monitorexit cp)
	   ;; Rethrow the exception
	   (Ijvms src 'athrow cp)
	   ;; A subroutine in case of break, continue, or return.
	   ;; TBF this should be conditionally added.
	   ;; The entry label to the finally subroutine.
	   (list (make-targetLabel labelFinally))
	   (finish-sub iLocals)
	   ;; The label to get outside the synchronized statement.
	   (list (make-targetLabel labelEnd)))
	  )))))

;; Create a label component and passed it out to inst->jvm.
;;
;; I: labels, [(u ('finally label) ('jvm label) ('java label labbrk labcnt)]
;;            For finally and jvm, the label has been generated by Ijvm and
;;            exists in the code as a target label.
;;            For java, the label is directly from the source code whereas
;;            labbrk and labcnt are jvm labels.
(define (n-label->jvm inst Mtype cp bLab cLab labels)
  (let* ((label       (n-qName-name (n-label-qName inst)))
	 (inst2       (n-label-stmt inst))
	 (brkLabel    (Ijvm 'label cp))
	 (cntLabel    (Ijvm 'label cp))
	 (newLabels   `((java ,label ,brkLabel ,cntLabel) ,@labels)))
    ;; If there is a label on this instruction and it is not one of
    ;; the loop instructions, it is the default case to insert
    ;; the break jvm label after the instruction.
    (append
     (inst->jvm inst2 Mtype cp bLab cLab newLabels #t)
     (if (or (n-do? inst2) (n-for? inst2) (n-while? inst2))
	 '()
	 (list (make-targetLabel brklabel))))))

;; Generating code for a return would be trivial if it were not for
;; try and synchronized statements. In the case of a try with a
;; finally, it must first call the finally before returning. In the
;; case of a synchronized, it must call the block to release the lock.
;; Moreover, the return might be nested in several levels of try and
;; synchronized statements or both. The list labels contains all
;; finally block to call (jsr). If a return with an expression is
;; nested in a try with a finally, the evaluation must be done before
;; calling the finally, the value store temporarily on the stack and
;; restore after calling the finally to return it.
;;
;; I: inst, (s n-return)
;; O: [bcode]
(define (n-return->jvm inst Mtype cp labels)
  (let* ((rType     (tyM-rType Mtype))
	 (finallies (get-finallies labels))
	 (access    (n-return-access inst)))

    (append
     (if (n-return-expr inst) 
	 (append
	  (expr->jvm (n-return-expr inst) Mtype cp #t)
	  (primitive-conversion->jvm (e-r-t (n-return-expr inst)) rType)
	  (if (pair? finallies)
	      (Ijvm 'store-local cp (varAccess-i access) (varAccess-jvmType access))
	      '()))
	 '())
     
     ;; If the return is in at least one try with a finally, it must
     ;; first execute the finally blocks before returning. If it
     ;; returns a value it must be calculated before calling the
     ;; finally blocks.
     (list-append (map (lambda (l) (Ijvm 'jsr cp (make-label l 2))) finallies))

     ;; Get back the return value if finally called.
     (if (and (n-return-expr inst) (pair? finallies))
	 (Ijvm 'load-local cp (varAccess-i access) (varAccess-jvmType access))
	 '())

     (Ireturn cp rType)
     )))

;; All return instructions are generated here.
;;
;; I: rType, Type
;; O: [bcode]
(define (Ireturn cp rType)
  (cond
   ((or (not rType) (T==? 'void rType))    (Ijvm 'return cp))
   ((or (T==? 'int rType)  (T==? 'char rType) (T==? 'short rType)
	(T==? 'byte rType) (T==? 'boolean rType))
    (Ijvm 'ireturn cp))
   ((T==? 'double rType) (Ijvm 'dreturn cp))
   ((T==? 'float rType)  (Ijvm 'freturn cp))
   ((T==? 'long rType)   (Ijvm 'lreturn cp))
   ((type-ref2? rType)   (Ijvm 'areturn cp))
   (else (error "CE, Ireturn, unknown result type: " rType))
   ))

;; I: (s n-if)
;; O: [bcode]
(define (n-if->jvm inst Mtype cp bLab cLab labels)
  (let* ((label1 (Ijvm 'label cp))
	 (label2 (Ijvm 'label cp))
	 (label3 (Ijvm 'label cp)))
    (append 
     (cond->jvm (n-if-expr inst) Mtype cp label2 'false)
     (list (make-targetLabel label1))
     (inst->jvm (n-if-Sthen inst) Mtype cp bLab cLab labels #f)
     (Ijvm 'goto cp (make-label label3 2))
     (list (make-targetLabel label2))
     (if (n-if-Selse inst)
	 (inst->jvm (n-if-Selse inst) Mtype cp bLab cLab labels #f)
	 '())
     (list (make-targetLabel label3))
     )))

;; I: inst, (s n-while)
;; O: [bcode]
(define (n-while->jvm inst Mtype cp labels lab?)
  (match inst (($ n-while expr statement src)
    (let* ((labelBrk (or (and lab? (third  (car labels))) (Ijvm 'label cp)))
	   (labelCnt (or (and lab? (fourth (car labels))) (Ijvm 'label cp)))
	   (newLabels `((jvm ,labelBrk) (jvm ,labelCnt) ,@labels)))
      (append
       (list (make-targetLabel labelCnt))
       (cond->jvm expr Mtype cp labelBrk 'false)
       (inst->jvm statement Mtype cp labelBrk labelCnt newLabels #f)
       (Ijvm 'goto cp (make-label labelCnt 2))
       (list (make-targetLabel labelBrk))
       )))))

;; I: inst, (s n-for)
;;    lab?, #t iff this for is labeled.
;; O: [bcode]
(define (n-for->jvm inst Mtype cp labels lab?)
  (match inst (($ n-for init cond-expr incr statement src)
    (append
     (if (or (pair? init) (null? init))
	 ;; initialize, but don't leave the values on stack
	 (list-append (map (lambda (e) (expr->jvm e Mtype cp #f)) init))
	 ;; it must be a localVarDecl
	 (n-localVarDecl->jvm init cp))
     (let* ((label1   (Ijvm 'label cp))
	    (labelBrk (or (and lab? (third  (car labels))) (Ijvm 'label cp)))
	    (labelCnt (or (and lab? (fourth (car labels))) (Ijvm 'label cp)))
	    (newLabels `((jvm ,labelBrk) (jvm ,labelCnt) ,@labels)))
       (append
	;; Entry point.
	(list (make-targetLabel label1))
	;; Testing the end condition.
	(if cond-expr (cond->jvm cond-expr Mtype cp labelBrk 'false) '())
	(inst->jvm statement Mtype cp labelBrk labelCnt newLabels #f)
	;; The continue point.
	(list (make-targetLabel labelCnt))
	;; Update, but don't leave any values on stack
	(list-append (map (lambda (e) (expr->jvm e Mtype cp #f)) incr))
	;; Loop back to the beginning.
	(Ijvm 'goto cp (make-label label1 2))
	;; End point
	(list (make-targetLabel labelBrk))
	))))))

;; I: inst, (s n-do)
;;    lab?, #t iff there is a label on this do statement.
;; O: [bcode]
(define (n-do->jvm inst Mtype cp labels lab?)
  (match inst (($ n-do statement cond-expr src)
    (let* ((label1    (Ijvm 'label cp))
	   (labelBrk  (or (and lab? (third  (car labels))) (Ijvm 'label cp)))
	   (labelCnt  (or (and lab? (fourth (car labels))) (Ijvm 'label cp)))
	   (newLabels `((jvm ,labelBrk) (jvm ,labelCnt) ,@labels)))
      (append
       (list (make-targetLabel label1))
       (inst->jvm statement Mtype cp labelBrk labelCnt newLabels #f)
       (list (make-targetLabel labelCnt))
       (cond->jvm cond-expr Mtype cp label1 'true)
       (list (make-targetLabel labelBrk))
       )))))

;; Generate code for continue and break instructions. Take care of
;; try and synchronized statements.
;;
;; I: inst, (s n-continue)
;;    label, sym.
;;    labels, see function n-label.
;; O: [bcode]
(define (abruptJump->jvm inst cp label labels)
  ;; The label is either provided by the instruction or it is the
  ;; default label.
  (let* ((targetId  (or (and (n-continue? inst) (n-continue-id inst))
			(and (n-break? inst)    (n-break-id inst))))
	 (theLabel  (or (and targetId `(java ,targetId)) `(jvm ,label)))
	 (finallies (get-finallies-labels theLabel labels)) 
	 ;; The target does exist since the semantic analysis verified that.
	 (target    (or (and (not targetId) label)
			(and targetId (if (n-continue? inst)
					  (fourth (:~-s targetId labels cadr))
					  (third  (:~-s targetId labels cadr)))))))
    (append
     ;; Call all finally blocks and synchronized exits before jumping
     ;; out.
     (list-append (map (lambda (label) (Ijvm 'jsr cp (make-label label 2))) 
		       finallies))
     (Ijvm 'goto cp (make-label (or target label) 2)))))

;; Returns the list of all finally labels before label.
;; I: lbls, [(u ('finally label) ('jvm label) ('java label labbrk labcnt)]
;;    label, (u ('java sym) ('jvm sym)). 
;; O: [label]
(define (get-finallies-labels label lbls)
  (get-finallies (list-head-p lbls 
			      (lambda (x) (and (eqv? (car label)  (car x))
					       (eqv? (cadr label) (cadr x)))))))

(define (get-finallies lbls) 
  (map cadr (filter (lambda (x) (eqv? 'finally (car x)))lbls)))

;; I: inst,  (s n-block)
;;    bLab,  (u #f label)
;;    cLab,  (u #f label)
;;    labels, [(label)]
;; O: [bcode]
(define (n-block->jvm inst Mtype cp bLab cLab labels)
  (list-append (map (lambda (x) (inst->jvm x Mtype cp bLab cLab labels #f)) 
		    (n-block-declStmts inst))))

;; Generate bytecode for try statement. Labels are used to mark the
;; beginning and end of the try block. The exception table is built
;; later using these marks.
;;
;; I: inst,   (s n-try)
;;    lbls,  [(kind label labelBreak labelContinue)]
;;              kind, (u 'finally 'java 'jvm);  label, sym.
;;            This list contains all labels that a break or continue might
;;            jump to and all finally labels of a finally block.
;; O: [bcode]
(define (n-try->jvm inst Mtype cp bLab cLab lbls)
  (match inst (($ n-try block catches finally src iRetAdr)
    (let* ((lblBlockTry (Ijvm 'label cp))
	   (lblTry      (Ijvm 'label cp))
	   (lblEndTry   (Ijvm 'label cp))
	   (lblFinally  (and finally (Ijvm 'label cp)))
	   (lbls2       (if finally 
			      `((finally ,lblFinally) ,@lbls) 
			      lbls))
	   (block-jvm     (n-block->jvm block Mtype cp bLab cLab lbls2)))
      (if (null? block-jvm) '()
	  (append
	   ;; Mark the beginning of the try
	   (list (make-tryLabel lblTry 'start #f))
	   ;; Mark the beginning of the try block
	   (list (make-tryLabel lblBlockTry 'start #f))
	   block-jvm
	   ;; Mark the end of the try block
	   (list (make-tryLabel lblBlockTry 'end #f))
	   ;; Jsr to the finally block if it exists
	   (if finally (Ijvm 'jsr cp (make-label lblFinally 2)) '())
	   ;; Jump outside the try statement.
	   (Ijvm 'goto cp (make-label lblEndTry 2))
	   ;; All catch blocks. They jsr to the finally block if it exists. 
	   (catches->jvm catches Mtype cp bLab cLab lblBlockTry 
			 lblEndTry lblFinally lbls2)
	   ;; Mark the end of the try block
	   (list (make-tryLabel lblTry 'end #f))
	   ;; Add short catch block for any exceptions, if finally exists.
	   (if finally
	       (append
		(list (make-tryLabel lblTry 'catch #f))
		(Ijvm 'astore cp iRetAdr)
		(Ijvm 'jsr cp (make-label lblFinally 2))
		(Ijvm 'aload cp iRetAdr)
		(Ijvm 'athrow cp))
	       '())

	   ;; The entry label to the finally subroutine.
	   (list (make-targetLabel lblFinally))
	   ;; The finally block itself.
	   (finally->jvm finally Mtype cp bLab cLab iRetAdr lbls)
	   ;; The label to get outside the try statement.
	   (list (make-targetLabel lblEndTry)))
	  )))))

;; I: catches, [(s n-catch)]  
;;    labelFinally, (u #f label)
;; O: [bcode]
(define (catches->jvm catches Mtype cp bLab cLab lblBlockTry lblEndTry lblFinally lbls)
  (define (catch->jvm catch)
    (match catch (($ n-catching catchHeader block src)
      (match catchHeader (($ n-catch type id src2 access)
	(let* ((descr   (T->jvmPool type 'class))
	       (i       (varAccess-i access))
	       (jvmType (varAccess-jvmType access)))
	  (append
	   (list (make-tryLabel lblBlockTry 'catch descr))
	   ;; The exception object is on top of the stack, store it in
	   ;; the catch parameter.
	   (Ijvm 'store-local cp i jvmType)
	   (n-block->jvm block Mtype cp bLab cLab lbls)
	   ;; Call the finally block, if it exists.
	   (if lblFinally (Ijvm 'jsr cp (make-label lblFinally 2)) '())
	   ;; TBF, goto should be removed if it is the last catch and there
	   ;; is no finally.
	   (Ijvm 'goto cp (make-label lblEndTry 2))
	   )))))))

  (list-append  (map catch->jvm catches)))

;; I: finally, (s n-block)
;; O: [bcode]
(define (finally->jvm finally Mtype cp bLab cLab iRetAdr lbls)
  (if finally
      (append
       ;; Store the return address in a local variable.
       (Ijvm 'astore cp (+ 1 iRetAdr)) 
       (n-block->jvm finally Mtype cp bLab cLab lbls)
       (Ijvm 'ret cp (+ 1 iRetAdr)))
      '()))

;; Generate code to initialize local variables.
;;
;; Syntaxically a field declaration is like a local declaration but it
;; may have several modifiers whereas for a local variable only `final' is
;; possible. Field initialization is handled directly using varInit->jvm.
;;
;; This may contain several declarations.  The initialization of local
;; variables is done at the point of declaration in the code.
;;
;; I: inst, (s n-localVarDecl)
;; O: [bcode]
;;
(define (n-localVarDecl->jvm inst cp)
  (match inst (($ n-localVarDecl type vars src)  
    (list-append (map (lambda (var) (varInit->jvm var cp type))
		      (filter n-varInit? vars))))
   ))

;; Generates byte code for initializer. It could be for a local
;; variable or a field (static and non-static).
;;
;; I: var, (s n-varInit)
;;    type, type of the var.
;; O: [bcode]
;;
(define (varInit->jvm var cp type)
  (let* ((initializer     (n-varInit-initializer var))
	 (varAccess       (n-varInit-access var))
	 (field?          (not (varAccess-i varAccess)))
	 (nonStaticField? (and field? (not (varAccess-static? varAccess)))))
    
    (if (n-arrayInit? initializer)
	;; It is an array initializer.
	(append
	 (if nonStaticField?  (Ijvm 'aload #f 0) '())
	 (arrayInit->jvm (n-varInit-initializer var) cp)
	 (if field?
	     (gen-store-field varAccess cp)
	     ;; It is a local array variable.
	     (Ijvm 'astore cp (varAccess-i varAccess))))
	;; It is an expression initializer.
	(append
	 (if nonStaticField?  (Ijvm 'aload #f 0) '())
	 (expr->jvm initializer #f cp #t)
	 (primitive-conversion->jvm (e-r-t initializer) type)
	 (if field?
	     (gen-store-field varAccess cp)
	     (Ijvm 'store-local cp (varAccess-i varAccess) 
		   (varAccess-jvmType varAccess))))
	)))

;; Bytecode to initialize an array.  That code evaluate and stores all
;; elements in the array and leave a reference to it on the stack. A
;; multidimensional array is built recursively.
;;
;; I: init, (s n-arrayInit).
;; O: [byte]. 
;;
(define (arrayInit->jvm init Mtype cp)
  (let* ((exprs  (n-arrayInit-exprs init))
	 (type   (n-arrayInit-type init))
	 (bType  (subarray-type type))
	 (src    (n-arrayInit-src init)))
    (append
     ;; Allocate one dimensional array.
     (Ijvm 'ldc cp 'int (length exprs))
     (Ijvms src (if (type-primitive? bType) 'newarray 'anewarray) cp bType)
     ;; Reference is on top of stack.
     (list-append
      (mapi (lambda (expr i) 
	      (append 
	       (Ijvm 'dup #f) ; keep reference to array to store elements.
	       (Ijvm 'ldc cp 'int i) ;; position i.
	       (if (n-arrayInit? expr)
		   (append (arrayInit->jvm expr cp) (Ijvm 'aastore #f))
		   (append (expr->jvm expr #f cp #t) ;; the element
			   (primitive-conversion->jvm (e-r-t expr) bType)
			   (Ijvm 'store-array cp (T->jvmType bType))))))
	    exprs)) ;; reference to array is on top of stack.
     )))

;; There are two JVM instructions to implement switch statements:
;; lookupswitch and tableswitch. The lookupswitch is the easiest to
;; use although it may generate a much longer instruction than
;; tableswitch.  Tableswitch should be used when the case values are
;; not scattered.
;;
;; O: [byte]
(define (n-switch->jvm inst Mtype cp lbls)
  (match inst (($ n-switch selExpr block src casesStmts)
    (let* ((case-labels (map (lambda (c) (: (car c) (Ijvm 'label cp))) casesStmts))
	   (label-end   (Ijvm 'label cp)))
      (append
       (expr->jvm selExpr Mtype cp #t)
       (Ijvms src 'lookupswitch cp case-labels)
       ;; All cases after the lookupswitch instruction.
       (list-append
	(map (lambda (case-stmts)
	       (cons
		;; The label for this case value.
		(make-targetLabel (:~. (car case-stmts) case-labels))
		;; All the statements of this case.
		(list-append
		 (map (lambda (stmt) (inst->jvm stmt Mtype cp label-end #f lbls #f))
		      (cdr case-stmts)))))
	     casesStmts))
       (list (make-targetLabel label-end))
       )))))

;; Generates bytecode to stack up the necessary elements to access an object.
;; I: lhs,
;;    duplicate?, #t iff the reference should be duplicated.
;;
;; O: [byte]
(define (gen-objectref lhs Mtype cp duplicate?)
  (cond
   ((n-qName? lhs)
    (let* ((access (n-qName-access lhs)))
      (if (pair? access)
	  (append (access->jvm (reverse (cdr (reverse access))) cp (n-qName-src lhs))
		  (if duplicate? (Ijvm 'dup #f) '()))
	  '())))
   ((n-fieldAccess? lhs)
    (append (expr->jvm (n-fieldAccess-field lhs) Mtype cp #t)
	    (if duplicate? (Ijvm 'dup #f) '())))
   ((n-arrayAccess? lhs)
    (append (expr->jvm (n-arrayAccess-array lhs) Mtype cp #t)
	    (expr->jvm (n-arrayAccess-index lhs) Mtype cp #t)
	    (if duplicate? (Ijvm 'dup2 #f) '())))
   (else  (error "CE, gen-objectref, unknown case: " lhs))
   ))

;; Generates a duplicate instruction to replicate top stack value.
;; The code is based on the number of words stack up by gen-objectref.
;; I: node, the same as gen-objectref.
;; O: [bcode]
(define (dup-inst node type)
  (Ijvm
   (:~. (if (or (T==? 'long type) (T==? 'double type)) 2 1)
	(:~. (if (n-arrayAccess? node) 2 
		 (if (n-fieldAccess? node) 1
		     (if (and (n-qName? node) (pair? (n-qName-access node))) 1 0)))
	     '((0 . ((1 . dup)    (2 . dup2)))
	       (1 . ((1 . dup_x1) (2 . dup2_x1)))
	       (2 . ((1 . dup_x2) (2 . dup2_x2))))))
   #f))

;; Generates bytecode to push on stack the value of a variable.
;;
;; I: node, (u (s n-qName) (s n-arrayAccess) (s n-fieldAccess))
;;    cp, constant pool.
;;    duplicate?, #t iff the object reference should be duplicated on stack.
;; O: [bcode]
;;
(define (variable->jvm node Mtype cp duplicate?)
  (cond
   ((n-qName? node)
    (append
     (gen-objectref node Mtype cp duplicate?)
     (varAccess->jvm (list-last (n-qName-access node)) cp (n-qName-src node) )))
   ((n-fieldAccess? node)
    (append (gen-objectref node Mtype cp duplicate?)
	    (varAccess->jvm (n-fieldAccess-access node) cp (n-fieldAccess-src node))))
   ((n-arrayAccess? node)
    (append (gen-objectref node Mtype cp duplicate?)
	    (Ijvm 'load-array cp (n-arrayAccess-basicType node))))
   (else (error "CE, variable->jvm, unknown case: " node))))

;; Generates code to push the value of a local variable, a parameter
;; or a field given an access description.
;;
;; I: access, (u (s varAccess) [(s varAccess)])
;;    dupl?, #t iff the reference should be duplicated.
;; O: [byte]
;;
(define (access->jvm access cp src)
  (if (pair? access)
      (list-append (map (lambda (x) (varAccess->jvm x cp src)) access))
      (varAccess->jvm access cp src)))

;; Generates code for one varAccess structure. This generates the basic
;; instructions for accessing a variable.
;;
;; I: varAccess, (u #f sym (s varAccess))
;;               TBF, removes the #f at semantic analysis in the access list.
;; O: [byte]
;;    Possible instructions are getfield, getstatic, and all the loads
;;    local: fload, iload, dload, etc.
;;
(define (varAccess->jvm varAccess cp src)
  (if (varAccess? varAccess)
      (if (varAccess-i varAccess)
	  ;; It is a local access
	  (Ijvms src 'load-local cp (varAccess-i varAccess) 
		 (varAccess-jvmType varAccess))
	  ;; It is a field access
	  (Ijvms src
		 (if (varAccess-static? varAccess) 'getstatic 'getfield)
		 cp 
		 (qName->classfName (varAccess-className varAccess))
		 (varAccess-fieldName varAccess)
		 (varAccess-jvmType varAccess)))
      ;; Special case described by a symbol.
      (if (symbol? varAccess)
	  (case varAccess
	    ((arraylength) (Ijvm 'arraylength cp))
	    ((this)
	     ;; This handles reference to a field without the `this' keyword.
	     (Ijvms src 'aload #f 0))
	    (else (error "CE, varAccess->jvm, unknown case: " varAccess)))
	  '())))

;; Generate bytecode to call a method.  A method can be invoked with
;; invokestatic, invokevirtual, invokeinterface, or invokespecial.
;;
;; I: call, (s n-call)
;;    result?, #t ==> keep result on stack.
;; O: [byte]
(define (call->jvm call Mtype cp result?)

  ;; See (7.7 JVMS) for the case of invokespecial
  (define (anInvokeSpecial? method mdfs Mtype)
    (or (method-constructor? Mtype) (member 'private mdfs)
	(and (n-fieldAccess? method)
	     (n-specialName? (n-fieldAccess-field method))
	     (eqv? 'super (n-specialName-name (n-fieldAccess-field method))))))

  (match call (($ n-call method argList src callMtype)
    (match callMtype (($ tyM mName mdfs rType parm-types throws table 
       classType nbWords contract src2)

      `(;; Stack the reference to the object,
	;; if it is not a static invocation.
	,@(if (not (member 'static mdfs))
	      (expr->jvm method Mtype cp #t)
	      '())
	
	;; Stack up the arguments
	,@(list-append 
	   (map (lambda (expr parm-type)
		  (let* ((expr-type (e-r-t expr)))
		    (append 
		     (expr->jvm expr Mtype cp #t)
		     (if (prim-widening? expr-type parm-type)
			 (widening-conversion->jvm expr-type parm-type)
			 '()))))
		argList parm-types))
	;; Call the method
	,@(let* ((descr    (T->jvmPool callMtype 'noClass))
		 (nbWords  (foldl + 1 (map type->nbWords parm-types)))
		 (typeName (T->jvmPool classType 'class)))
	    (if (type-interface? classType)
		(Ijvms src 'invokeinterface cp typeName mName descr nbWords)
		(Ijvms src 
		       (cond
			((member 'static mdfs)  'invokestatic)
			((anInvokeSpecial? method mdfs callMtype) 'invokespecial)
			(else 'invokevirtual))
		       ;; For Contract Java, the method name is modified.
		       cp typeName (if (and (eqv? contract 'contract)
					    (not (eqv? 'wrapper (tyM-contract Mtype))))
				       (++ mName "<c>") mName) descr)))
	,@(if (and (not result?) rType (not (T==? rType 'void)))
	      (Ijvms src 'pop cp)
	      '()))
      )))))

;; Generate code to convert fromType to toType.
;; O: [bcode]
;; TBF verify all these conversions carefully
(define (widening-conversion->jvm fromType toType)
  (cond
   ((or (eqv? fromType toType) (type-ref2? fromType) (type-ref2? toType))
    '())
   (else
    (case fromType
      ((int char byte short)
       (if (member toType '(double float long))
	   (Ijvm (:~. toType '((double . i2d) (float . i2f) (long . i2l))) #f)
	   ;; There are no conversion from char, byte, and short to int.
	   ;; The char, byte, and short are on the stack as int already.
	   '()))
      ((float)  (if (eqv? toType 'double)  (Ijvm 'f2d #f)   '()))
      ((long)   (if (member toType '(float double))
		    (Ijvm (:~. toType '((float . l2f) (double . l2d))) #f)
		    '()))
      (else '())))))

;; TBF, verify all these conversions carefully
;; O: [bcode]
(define (narrowing-conversion->jvm fromType toType)
  (cond
   ((or (eqv? fromType toType) (type-ref2? fromType) (type-ref2? toType))
    '())
   (else
    (case fromType
      ((int char byte short)
       (if (member toType '(byte char short))
	   (Ijvm (:~. toType '((byte . i2b)(char . i2c) (short . i2s)))	 #f)
	   '()))
      ((long)
       (if (member toType '(int float double))
	   (Ijvm (:~. toType '((int . l2i) (float . l2f) (double . l2d)))  #f)
	   (if (member toType '(short byte char))
	       (append
		(Ijvm 'l2i #f)
		(Ijvm (:~. toType '((char . i2c) (short . i2s) (byte . i2b)))  #f))
	       '())))
      ((float)
       (if (member toType '(int long))
	   (Ijvm (:~. toType '((int . f2i) (long . f2l))) #f)
	   (if (member toType '(char byte short))
	       (append
		(Ijvm f2i #f)
		(Ijvm (:~. toType '((char . i2c) (short . i2s) (byte . i2b))) #f))
	       '())))
      ((double)
       (if (member toType '(float int long))
	   (Ijvm (:~. toType '((int . d2i) (long . d2l) (float . d2f))) #f)
	   (if (member toType '(char byte short))
	       (append
		(Ijvm 'd2i #f)
		(Ijvm (:~. toType '((char . i2c) (short . i2s) (byte . i2b)))  #f))
	       '())))
      (else  '())))))

;; Generates instructions to convert a primitive value to another
;; primitive value by either widening or narrowing.
;; If fromType is the same as toType, an empty list is generated.
;;
;; I :  fromType, toType. Jvm types.
;; O : [bcode]
(define (primitive-conversion->jvm fromType toType)
  (let* ((code  (widening-conversion->jvm fromType toType)))
    (if (pair? code)
	code
	(narrowing-conversion->jvm fromType toType))))

;; Same as primitive-conversion->jvm but with conversion to String
;; for the + operator.
(define (primitive-conversion2->jvm fromType toType cp)
  ;; Use String.concat to concatenate the objects.
  (define (toString->jvm type)
    (Ijvm 'invokestatic cp (T->jvmPool '("java" "lang" "String") 'class) "valueOf" 
	  (++ "("(T->jvmPool 
		 (cond 
		  ((or (type-ref2? type) (qName? type)) '("java" "lang" "Object"))
		  ((or (T==? type 'short) (T==? type 'byte) (T==? type 'int))
		   ;; Char is not included since it must use
		   ;; the method valueOf(char)
		   'int)
		  (else type))
		 'noClass)
	     ")Ljava/lang/String;")))
  
  (if (T==? toType '("java" "lang" "String"))
      (toString->jvm fromType)
      (primitive-conversion->jvm fromType toType)))

;; The expression is a conditional in a branching context.  This is a
;; version of expr->jvm only for conditional when the branching target
;; is known.
;;
;; I : expr, necessarily a boolean expression.
;;     cp, the constant pool.
;;     br-label, a label to branch to when false or true depending on br-kind.
;;     br-kind, (u 'true 'false)
;;              Specifies case it must branch to br-label.
;;
;; O :  [bcode]
;;
(define (cond->jvm expr Mtype cp br-label br-kind)
  ;;(display-ln "cond->jvm " expr)
  (cond 
   ((or (n-boollit? expr) (and (e-r? expr) (e-r-v expr)))
    ;; It's a constant expression.
    (if (eqv? (if (e-r? expr) (e-r-v expr)
		  (n-boollit-value expr)) 'true)
	(if (eqv? br-kind 'true)
	    (Ijvm 'goto cp (make-label br-label 2))
	    '()) ;; no instruction since it is true and it should continue.
	;; It is "false".
	(if (eqv? br-kind 'false)
	    (Ijvm 'goto cp (make-label br-label 2))
	    '())))
   ((e-r? expr)  (cond->jvm (e-r-node expr) Mtype cp br-label br-kind))
   ((or (n-qName? expr) (n-fieldAccess? expr) (n-arrayAccess? expr)
	(n-instanceof? expr) (n-call? expr) (n-question? expr)
	(and (n-op? expr) (member (n-op-op expr) '(^ or &))))
    (append
     (expr->jvm expr Mtype cp #t)
     (Ijvm (if (eqv? 'false br-kind) 'ifeq 'ifne) cp (make-label br-label 2))))
   ((n-unary? expr)
    (if (eqv? '! (n-unary-op expr))
	(cond->jvm (n-unary-expr expr) Mtype cp br-label 
		   (if (eqv? br-kind 'true) 'false 'true))
	(error "TBF, cond->jvm, unary: " expr)))

   ((and (n-op? expr) (eqv? (n-op-op expr) 'oror))
    (let* ((eleft      (n-op-left expr))
	   (eright     (n-op-right expr))
	   (label-true (Ijvm 'label cp)))
      (append
       (if (eqv? br-kind 'true)
	   (cond->jvm eleft Mtype cp br-label 'true)
	   (cond->jvm eleft Mtype cp label-true 'true))
       (cond->jvm eright Mtype cp br-label br-kind)
       (list (make-targetLabel label-true)))
      ))
   ((and (n-op? expr) (eqv? (n-op-op expr) '&&))
    (let* ((eleft      (n-op-left expr))
	   (eright     (n-op-right expr))
	   (label-false (Ijvm 'label cp)))
      (append
       (if (eqv? br-kind 'false)
	   (cond->jvm eleft Mtype cp br-label 'false)
	   (cond->jvm eleft Mtype cp label-false 'false))
       (cond->jvm eright Mtype cp br-label br-kind)
       (list (make-targetLabel label-false)))))
   ((and (n-op? expr) (member (n-op-op expr) '(< > <= >= == !=)))
    ;; We are dealing with numerical values for all operators but == and !=.
    (let* ((eleft     (n-op-left expr))
	   (tleft     (e-r-t eleft))
	   (eright    (n-op-right expr))
	   (tright    (e-r-t eright))
	   (t-conv    (if (prim-widening? tleft tright) tright tleft)))

      (if (and (type-ref2? tleft) (type-ref2? tright))
	  (append
	   (expr->jvm eleft Mtype cp #t)
	   (expr->jvm eright Mtype cp #t)
	   (Ijvm (:~. (n-op-op expr)
		      (if (eqv? br-kind 'true)
			  '((== . if_acmpeq) (!= . if_acmpne))
			  '((== . if_acmpne) (!= . if_acmpeq))))
		 #f (make-label br-label 2)))
	  (append
	   (expr->jvm eleft Mtype cp #t)
	   (widening-conversion->jvm tleft tright)
	   (expr->jvm eright Mtype cp #t)
	   (widening-conversion->jvm tright tleft)
	   (if (or (type-boolean? t-conv) (type-int? (unary-prom t-conv)))
	       (Ijvm (:~. (n-op-op expr)
			  (if (eqv? br-kind 'false)
			      '((== . if_icmpne) (!= . if_icmpeq) (<  . if_icmpge) 
				(>  . if_icmple) (<= . if_icmpgt) (>= . if_icmplt))
			      '((== . if_icmpeq) (!= . if_icmpne) (<  . if_icmplt) 
				(>  . if_icmpgt) (<= . if_icmple) (>= . if_icmpge))))
		     #f (make-label br-label 2))
	       ;; It is not int, compare the values to generate
	       ;; an int and use it to branch.
	       (append
		(Ijvm (:~. t-conv '((float . fcmpg) (double . dcmpg) (long . lcmp))) #f)
		(Ijvm (:~. (n-op-op expr)
			   (if (eqv? br-kind 'false)
			       '((== . ifne) (!= . ifeq) (< . ifge) (> . ifle) 
				 (<= . ifgt) (>= . iflt))
			       '((== . ifeq) (!= . ifne) (< . iflt) (> . ifgt) 
				 (<= . ifle) (>= . ifge))))
		      #f (make-label br-label 2))
		))))))
   (else (error "cond->jvm TBF " expr))
   ))

;; I: expr, (u (s n-literal) (s n-boollit) (s n-op) ...)
;;    Mtype (u tyM). The method type of the method where this expression occurs.
;;    cp, constant-pool
;;    result?, #t iff the result should be left on the stack.
;; O: [bcode]
;;
(define (expr->jvm expr Mtype cp result?)
  (cond
   ((and (e-r? expr) (e-r-v expr))  (Ijvm 'ldc cp (e-r-t expr) (e-r-v expr)))
   ((e-r? expr)                     (expr->jvm (e-r-node expr) Mtype cp result?))
   ((or (n-qName? expr) (n-arrayAccess? expr) (n-fieldAccess? expr))
    (variable->jvm expr Mtype cp #f))
   ((n-literal? expr)     (Ijvm 'ldc cp (n-literal-type expr) (n-literal-value expr)))
   ((n-specialName? expr) (specialName->jvm expr cp result?))
   ((n-boollit? expr)     (Ijvm 'ldc cp 'int (javaBool->int (n-boollit-value expr))))
   ((n-assignment? expr)  (assignment->jvm expr Mtype cp result?))
   ((n-postExpr? expr)    (postExpr->jvm   expr Mtype cp result?))
   ((n-call? expr)        (call->jvm       expr Mtype cp result?))
   ((n-alloc? expr)       (alloc->jvm      expr Mtype cp result?))
   ((n-unary? expr)       (unary->jvm      expr Mtype cp result?))
   ((n-op? expr)          (expr-op->jvm    expr Mtype cp))
   ((n-question? expr)    (question->jvm   expr Mtype cp))
   ((n-instanceof? expr)
    (let* ((cast-expr (n-instanceof-expr expr))
	   (typeVar   (n-instanceof-typeVar expr))
	   (src       (n-instanceof-src expr)))
      (append (expr->jvm cast-expr Mtype cp #t)
	      (Ijvms src 'instanceof cp (T->jvmPool typeVar 'class)))))
   ((n-cast? expr)
    ;; Very similar to instanceof.
    (let* ((cast-expr (n-cast-expr expr))
	   (typeCast  (n-cast-typeCast expr))
	   (src       (n-cast-src expr)))
      (append
       (expr->jvm cast-expr Mtype cp #t)
       (primitive-conversion->jvm (e-r-t cast-expr) typeCast)
       (if (type-ref? typeCast)
	   (Ijvms src 'checkcast cp (T->jvmPool typeCast 'class))
	   '()))))
   (else (error "CE, expr->jvm, unknown expression: " expr))
   ))

;; The post ++ and -- operators as well as the operator assignments
;; need some special compilation.
;; O: [bcode]
(define (postExpr->jvm expr Mtype cp result?)
  (let* ((e-r (n-postExpr-expr expr)))
    ;; See if iinc can be used, a common case. The variable
    ;; must be local and an int.
    (if (and (eqv? 'int (unary-prom (e-r-t e-r))) 
	     (n-qName? (e-r-node e-r))
	     (varAccess? (n-qName-access (e-r-node e-r)))
	     (varAccess-i (n-qName-access (e-r-node e-r))))
	(append
	 ;; Leave the value of the variable on the stack if requested.
	 (if result? (expr->jvm e-r Mtype cp #t) '())
	 (Ijvm 'iinc #f 
	       (varAccess-i (n-qName-access (e-r-node e-r)))
	       (if (eqv? '-- (n-postExpr-op expr)) -1 1)))
	;; More general case using add.
	(append
	 ;; Stack value with reference.
	 (variable->jvm (e-r-node e-r) Mtype cp #t)
	 ;; Keep result before adding 1.
	 (if result? (dup-inst (e-r-node e-r) (e-r-t e-r)) '())
	 (Ijvm 'ldc cp (e-r-t e-r) 1) ;; stack 1
	 (Ijvm (:~. (e-r-t e-r) 
		    '((int . iadd)  (byte . iadd)   (short . iadd)
		      (char . iadd) (double . dadd) (long . ladd) (float . fadd))) 
	       cp)
	 
	 (gen-store (e-r-node e-r) cp)))))

(define (assignment->jvm expr Mtype cp result?)
  ;; 15.25
  ;; An assignment may require some conversion on primitive types.
  ;; In the case of a complex assignment (e.g. for array or
  ;; qualified name) it may be necessary to first compute the
  ;; object referred to and then the right side with a final store.
  ;; This is done by the function gen-objectref.
  (let* ((rhs  (n-assignment-rhs expr))
	 (op   (n-assignment-op  expr))
	 (lhs  (n-assignment-lhs expr))
	 (src  (n-assignment-src expr))
	 (type-rhs  (e-r-t rhs))
	 (type-lhs  (e-r-t lhs)))
    
    (append
     ;; This stacks up the value and a copy of the reference object.
     ;; This may be two, three or four words (array).
     (if (eqv? op '=) 
	 (gen-objectref (e-r-node lhs) Mtype cp #f)
	 (variable->jvm (e-r-node lhs) Mtype cp #t))
     ;; This may stack up two words (double and long).
     (expr->jvm rhs Mtype cp #t)
     ;; Implicit conversion (implicit cast for primitive)
     (primitive-conversion2->jvm type-rhs type-lhs cp)
     ;; Operate on the two values.
     (if (eqv? '= op)
	 '()
	 (Ijvms src (assign-op->op op) cp (unary-prom type-lhs)))
     ;; If requested, duplicate result to keep it on stack
     ;; but move it below the object reference word(s).
     (if result? (dup-inst (e-r-node lhs) type-lhs) '())
     (gen-store (e-r-node lhs) cp)
     ;; If the result is requested it is now on top of stack.
     )))

(define (specialName->jvm expr cp result?)
  (match expr (($ n-specialName name src)
    (case name
      ((null)       (Ijvms src 'aconst_null #f))
      ((this super) ;; The `this' keyword has been specified explicitly.
       (Ijvms src 'aload #f 0))
      (else (error "CE, specialName->jvm, unkown case: " expr))))))

;; I: result?, #t iff the result should be kept on the stack
;; O: [bcode]
(define (unary->jvm expr Mtype cp result?)
  (match expr (($ n-unary op expr2 src)
    ;; op can be ++, --, -, +, ~, !
    (case op
      ((! ~) (append (expr->jvm expr2 Mtype cp #t)
		     (Ijvm 'ldc cp 'int (if (eqv? op '!) 1 -1))
		     (Ijvms src 'ixor cp)))
      ((- +)
       (append
	(expr->jvm expr2 Mtype cp #t) 
	(if (eqv? op '-)
	    (Ijvms src (:~. (e-r-t expr2) 
			    '((long  . lneg) (int    . ineg) (char . ineg)
			      (byte  . ineg) (short  . ineg) (float . fneg) 
			      (double . dneg)))
		   cp)
	    '())
	(if result? '() (Ijvm 'pop #f))))
      ((++ --)
       ;; Cab iinc be used? The variable must be local and an int.
       (if (and (eqv? 'int (unary-prom (e-r-t expr2)))
		(n-qName? (e-r-node expr2))
		(varAccess? (n-qName-access (e-r-node expr2)))
		(varAccess-i (n-qName-access (e-r-node expr2))))
	   (append
	    (Ijvm 'iinc #f (varAccess-i (n-qName-access (e-r-node expr2)))
		  (if (eqv? '-- op) -1  1))
	    ;; Push value on the stack if requested.
	    (if result?	(expr->jvm expr2 Mtype cp #t)	'()))
	   ;; General case.
	   (append
	    ;; Stack value with reference.
	    (variable->jvm (e-r-node expr2) Mtype cp #t)
	    (Ijvm 'ldc cp (e-r-t expr2) 1) ;; stack 1
	    (Ijvm (:~. (e-r-t expr2) 
		       '((int . iadd)  (byte . iadd)   (short . iadd)
			 (char . iadd) (double . dadd) (long . ladd)
			 (float . fadd)))
		  cp)
	    (if result? (dup-inst (e-r-node expr2) (e-r-t expr2)) '())
	    (gen-store (e-r-node expr2) cp))))
      (else (error "CE, unary->jvm, unknown case: " expr))
      ))))

;; I: expr, (s n-question)
;; O: [bcode]
(define (question->jvm expr Mtype cp)
  (match expr (($ n-question cond-expr Sthen Selse src)
    (let* ((br-false    (Ijvm 'label cp))
	   (br-continue (Ijvm 'label cp)))
    (append 
     (cond->jvm cond-expr Mtype cp br-false 'false)
     (expr->jvm Sthen Mtype cp #t)
     (widening-conversion->jvm (e-r-t Sthen) (e-r-t Selse))
     (Ijvm 'goto cp (make-label br-continue 2))
     (list (make-targetLabel br-false))
     (expr->jvm Selse Mtype cp #t)
     (widening-conversion->jvm (e-r-t Selse) (e-r-t Sthen))
     (list (make-targetLabel br-continue))))
    )))

;; I: expr, (s n-alloc)
;;    result?, #t => keep result on stack. (this makes sense only for classAlloc)
;; O: [bcode]
(define (alloc->jvm expr Mtype cp result?)
  (match expr (($ n-alloc qName alloc-expr src)
    (if (not qName)
	(cond
	 ((n-arrayAlloc? alloc-expr)
	  (match alloc-expr
		 (($ n-arrayAlloc typeName dimExprs dims initializers src)
	    (if (and (= 1 (length dimExprs)) (= 0 dims))
		;; One dimensional arrays.
		`(,@(expr->jvm (car dimExprs) Mtype cp #t)
		  ,@(if (type-primitive? typeName)
			(Ijvms src 'newarray cp typeName)
			(Ijvms src 'anewarray cp typeName)))
		;; Multi-dimensional arrays.
		`(,@(list-append (map (lambda (e) (expr->jvm e Mtype cp #t)) dimExprs))
		  ,@(Ijvms src 'multianewarray cp 
			   (T->jvmPool `(,typeName ,(+ (length dimExprs) dims))
				       'noClass)
			   (length dimExprs)))))))
	 ((n-classAlloc? alloc-expr)
	  ;; typeName is necessarily a (s type)
	  (match alloc-expr (($ n-classAlloc typeName argList fields src typeMethod)
	    (let* ((className  (T->jvmPool typeName 'class))
		   (typeDescr  (T->jvmPool typeMethod 'noClass))
		   (parm-types (tyM-parm-types typeMethod)))
	      (append
	       (Ijvms src 'new cp className)
	       (Ijvm 'dup cp)
	       ;; Stack up arguments and do required primitive conversions.
	       (list-append 
		(map (lambda (e parm-type) 
		       (append (expr->jvm e Mtype cp #t)
			(widening-conversion->jvm (e-r-t e) parm-type)))
		     argList parm-types))
	       (Ijvms src  'invokespecial cp className "<init>" typeDescr)
	       (if result? '() (Ijvms src 'pop cp))
	       ))
	    )))
	 (else  (error "alloc->jvm, TBF 2 " expr)))
	(error "alloc->jvm, TBF 3 " expr))
    )))

;; Some operators are overloaded, these are +, |, &, ^, ==, !=.
;;
;; I: expr, (s n-op)
;;    cp, constant pool
;; O: [bcode]
;;
(define (expr-op->jvm expr Mtype cp)

  (case (n-op-op expr)
    ((&& oror == <= >= != < >)
     (let* ((label-continue (Ijvm 'label cp))
	    (label-true     (Ijvm 'label cp)))
       (append
	(cond->jvm expr Mtype cp label-true 'true)
	(Ijvm 'ldc #f 'int 0) ;; false
	(Ijvm 'goto cp (make-label label-continue 2))
	(list (make-targetLabel label-true))
	(Ijvm 'ldc #f 'int 1) ;; true
	(list (make-targetLabel label-continue))
	)))

    ((+ - * / << >> >>> % or ^ &) 
     ;; Note: these are all operators allowed in operator-assignments.
     ;; In the case of operators | (aka or), ^, and &, the operands
     ;; may be boolean. Even in that case both operands must be
     ;; evaluated, and even for these, this code works as if they were
     ;; integer values.
     `(,@(expr->jvm (n-op-left expr)  Mtype cp #t)
       ,@(primitive-conversion2->jvm (e-r-t (n-op-left expr)) (n-op-type expr) cp)
       ,@(expr->jvm (n-op-right expr) Mtype cp #t)
       ,@(primitive-conversion2->jvm (e-r-t (n-op-right expr))
				     (if (member (n-op-op expr) '(<< >> >>>))
					 'int
					 (n-op-type expr)) cp)
       ,@(Ijvms (n-op-src expr) (n-op-op expr) cp (n-op-type expr))
       ))
    (else (error "CE, expr-op->jvm, operator unknown:  " (n-op-op expr)))
    ))

;; This is similar to Ijvm but with the insertion of a line number in
;; the bytecode.
;;
;; I: src, (s n-src)
;;    mne, sym
;; O: [bcode]
(define (Ijvms src mne cp . l)
  (: `(lineNumber ,(n-src-line src))  (apply Ijvm `(,mne ,cp ,@l))))

;; Basic generation of byte code.
;;
;; Generate bytecode according to the mne. 
;; This function maintains the pool of constants of the class file.
;;
;; I: mne, sym
;;    cp, a constant pool
;; O: [bcode]
;;
(define (Ijvm mne cp . l)

  (define (ldc value type)
    ;; Note: ldc2 does not exist.
    (let* ((index (add-cp! cp value type)))
      (if (member type '(long double))
	  `(20 ,@(int->2bytes index)) ;; ldc2_w
	  (if (<= index 255)
	      `(18 ,index) ;; ldc
	      (if (<= index 65535)
		  `(19 ,@(int->2bytes index)) ;; ldc_w
		  (error "CE, ldc, Ijvm, index is greater than 65535: " index))))))

  ;; See 15.17.2 for some comments about using StringBuffer.
  (define (stringConcat->jvm)
    (Ijvm 'invokevirtual cp "java/lang/String"
	  "concat" "(Ljava/lang/String;)Ljava/lang/String;"))

  (case mne
    ((label) ;; This is not a JVM instruction.
     (let* ((n     (vector-ref cp 1))
	    (label (string->symbol (++ "l" n))))
       (vector-set! cp 1 (+ n 1))
       label))
    ((+ - * / << >> >>> % or ^ &)
     (if (and (eqv? mne '+) (T==? '("java" "lang" "String") (car l)))
	 (stringConcat->jvm)
	 (Ijvm (:~. mne
		    (case (car l)
		      ((int boolean byte char short)
		       '((+ . iadd) (* . imul) (% . irem) 
			 (- . isub) (/ . idiv) (or . ior)
			 (^ . ixor) (& . iand) (<< . ishl) (>> . ishr) (>>> . iushr)))
		      ((long)
		       '((+ . ladd) (* . lmul) (% . lrem) 
			 (- . lsub) (/ . ldiv) (or . lor)
			 (^ . lxor) (& . land) (<< . lshl) (>> . lshr) (>>> . lushr)))
		      ((float)
		       '((+ . fadd) (* . fmul) (% . frem) (- . fsub) (/ . fdiv)))
		      ((double) 
		       '((+ . dadd) (* . dmul) (% . drem) (- . dsub) (/ . ddiv)))
		      (else (error "CE, Ijvm, unknown type: " (car l) " mne " mne))
		      ))
	       cp)))
    ((aconst_null arraylength athrow  monitorenter monitorexit nop 
		  pop pop2 swap  dup dup_x1 dup_x2  dup2 dup2_x1 dup2_x2 
		  f2d f2i f2l i2b i2c i2d  i2f i2l i2s   l2d l2f l2i d2i d2l d2f)
     (list (:~. mne '((aconst_null . 1) (arraylength . 190) 
		      (athrow . 191) (monitorenter . 194) (monitorexit . 195) (nop . 0)
		      (pop . 87) (pop2 . 88) (swap . 95) (dup . 89) (dup_x1 . 90) 
		      (dup_x2 . 91) (dup2 . 92) (dup2_x1 . 93) (dup2_x2 . 94)
		      (f2d . 141) (f2i . 139) (f2l . 140)
		      (i2b . 145) (i2c . 146) (i2d . 135)
		      (i2f . 134) (i2l . 133) (i2s . 147)
		      (l2d . 138) (l2f . 137) (l2i . 136)
		      (d2i . 142) (d2l . 143) (d2f . 144)
		      ))))
    ((if_icmpge if_icmple if_icmpgt if_icmplt if_icmpne if_icmpeq if_acmpeq
		if_acmpne ifeq ifne iflt ifgt ifge ifle)
     (list
      (:~. mne '((if_icmpeq . 159) (if_icmpne . 160) (if_icmplt . 161)
		 (if_icmpge . 162) (if_icmpgt . 163) (if_icmple . 164)
		 (if_acmpeq . 165) (if_acmpne . 166)
		 (ifeq . 153) (ifne . 154) (iflt . 155) (ifge . 156)
		 (ifgt . 157) (ifle . 158)))
      (car l)))
    ((dcmpg dcmpl fcmpg fcmpl lcmp)
     (list (:~. mne '((dcmpg . 152) (dcmpl . 151) (fcmpg . 150) 
		      (fcmpl . 149) (lcmp . 148)))))
    ((goto)    (list 167 (car l)))
    ((jsr)     (list 168 (car l)))
    ((jsr_w)   (list 201 (car l)))
    ((load-local)
     (let* ((i  (car l))  (type  (cadr l)))
       (case type
	 ((byte char short int)  (Ijvm 'iload cp i))
	 ((boolean)    (Ijvm 'iload cp i))
	 ((float)      (Ijvm 'fload cp i))
	 ((long)       (Ijvm 'lload cp i))
	 ((double)     (Ijvm 'dload cp i))
	 ((reference)  (Ijvm 'aload cp i))
	 (else (error "CE, Ijvm, unknown type: " type mne))
	 )))
    ((store-local)
     (let* ((i  (car l)) (type  (cadr l)))
       (case type
	 ((byte char short int)   (Ijvm 'istore cp i))
	 ((boolean)   (Ijvm 'istore cp i))
	 ((float)     (Ijvm 'fstore cp i))
	 ((long)      (Ijvm 'lstore cp i))
	 ((double)    (Ijvm 'dstore cp i))
	 ((reference) (Ijvm 'astore cp i))
	 (else (error "CE, Ijvm, unknown type: " type mne))
	 )))
    ((iaload daload saload baload aaload caload laload faload)
     (list (:~. mne '((iaload . 46) (daload . 49) (baload . 51) (aaload . 50)
		      (faload . 48) (laload . 47) (saload . 53) (caload . 52)))))
    ((load-array)
     (let* ((type-array (car l)))
       (Ijvm (:~. type-array '((int . iaload)  (short . saload)
			       (byte . baload) (boolean . baload)
			       (char . caload) (long . laload)
			       (float . faload) (double . daload)
			       (reference . aaload)))
	     #f)))
    ((iastore dastore sastore bastore aastore castore lastore fastore)
     (list (:~. mne
		'((iastore . 79) (dastore . 82) (bastore . 84) (aastore . 83)
		  (fastore . 81) (lastore . 80) (sastore . 86) (castore . 85)))))
    ((store-array)
     (let* ((type-array (car l)))
       (Ijvm (:~. type-array '((int . iastore)  (short . sastore) (byte . bastore) 
			       (boolean . bastore) (char . castore) (long . lastore)
			       (float . fastore) (double . dastore)
			       (reference . aastore)))
	     #f)))
    ((ldc)
     ;; ldc takes care of special cases like iconst_<i>, lconst_<l>, bipush,
     ;; sipush, etc.
     (let* ((valueA (if (char? (cadr l))
			;; TBF, not totally correct for utf-8 character.
			(char->integer (cadr l))
			(cadr l)))
	    (type  (car l))
	    (value (if (and (pair? valueA) (eqv? type 'double))
		       (highLow->double valueA)
		       (if (and (pair? valueA) (eqv? type 'long))
			   (highLow->long valueA)
			   valueA))))

       (case type
	 ((int char short byte)
	  (cond
	   ((and (<= -1 value) (<= value 5))   (list (+ value 3))) ;; iconst_<n> 
	   ((and (<= -128 value) (<= value 127))     (Ijvm 'bipush cp value))
	   ((and (<= -32768 value) (<= value 32767)) (Ijvm 'sipush cp value))
	   (else (ldc value 'int))))
	 ((boolean)  (Ijvm 'ldc cp 'byte (javaBool->int value)))
	 ((long)  (if (or (= value 0) (= value 1))
		      (list (+ value 9)) ;; lconst_<l>
		      (ldc value 'long)))
	 ((float)  (if (or (= value 0.0) (= value 1.0) (= value 2.0))
		       (list (+ (inexact->exact value) 11)) ;; fconst_<f>
		       (ldc value 'float)))
	 ((double) (if (or (= value 0.0) (= value 1.0))
		       (list (+ (inexact->exact value) 14)) ;; dconst_<d>
		       (ldc value 'double)))
	 ((null)  (Ijvm 'aconst_null #f))
	 (else (if (T==? type '("java" "lang" "String"))
		   (ldc value 'string)
		   (error "CE, Ijvm ldc, unknown type: " type)))
	 )))
    ;; All return instructions.
    ((return ireturn dreturn freturn areturn lreturn)
     (list (:~. mne '((return  . 177) (ireturn . 172) (dreturn . 175) (freturn . 174)
		      (lreturn . 173) (areturn . 176)))))
    ((iadd iand idiv imul ineg ior irem ishl ishr isub iushr ixor
	   ladd land ldiv lmul lneg lor lrem lshl lshr lsub lushr lxor
	   dadd ddiv dmul dneg drem dsub fadd fdiv fmul fneg frem fsub)
     (list (:~. mne '((iadd . 96)  (iand  . 126)  (idiv . 108)
		      (imul . 104) (ineg  . 116)  (ior  . 128)
		      (irem . 112) (ishl  . 120)  (ishr . 122)
		      (isub . 100) (iushr . 124)  (ixor . 130)
		      
		      (ladd . 97)  (land . 127)  (ldiv . 109)
		      (lmul . 105) (lneg . 117)  (lor . 129)
		      (lrem . 113) (lshl . 121)  (lshr . 123)
		      (lsub . 101) (lushr . 125) (lxor . 131)
		      
		      (dadd . 99)  (ddiv . 111)  (dmul . 107) 
		      (dneg  . 119) (drem . 115) (dsub . 103)
		      
		      (fadd . 98)   (fdiv . 110) (fmul . 106) 
		      (fneg  . 118) (frem . 114) (fsub . 102)
		      ))))
    ((newarray) (let* ((type (car l)))  `(188 ,(type->atype type))))
    ((anewarray)
     (let* ((type   (car l))
	    (index  (add-cp! cp (T->jvmPool type 'class) 'class)))
       `(189 ,@(int->2bytes index))))
    ((multianewarray)
     (let* ((type  (car l)) (dims  (cadr l))
	    (index (add-cp! cp type 'class)))
       `(197 ,@(int->2bytes index) ,dims)))
    ((new)
     ;; New is used for instance class allocation only. (No arrays).
     (let* ((className  (car l))
	    (index      (add-cp! cp className 'class)))
       `(187 ,@(int->2bytes index))))
    ((invokestatic invokevirtual invokespecial)
     (let* ((className  (car l))
	    (mName      (cadr l))
	    (typedescr  (caddr l))
	    (methodRef  (add-cp! cp `(,className ,`(,mName ,typedescr)) 'methodRef)))
       `(,(:~. mne '((invokevirtual . 182) (invokestatic . 184)
		     (invokespecial . 183)))
	 ,@(int->2bytes methodRef))))
    ((invokeinterface)
     (let* ((className  (car l))
	    (mName      (cadr l))
	    (typedescr  (caddr l))
	    (nargs      (cadddr l))
	    (methodRef  (add-cp! cp  `(,className ,`(,mName ,typedescr))
				 'interfaceMethodRef)))
       `(185 ,@(int->2bytes methodRef)  ,nargs  0)
       ))
    ((getstatic getfield putstatic putfield)
     (let* ((className (car l))
	    (fieldName (cadr l))
	    (typedescr (caddr l))
	    (fieldRef  (add-cp! cp `(,className ,`(,fieldName ,typedescr)) 'fieldRef)))
       `(,(:~. mne '((getstatic . 178) (getfield . 180) 
		     (putstatic . 179) (putfield . 181)))
	 ,@(int->2bytes fieldRef))))
    ((instanceof checkcast)
     (let* ((index (add-cp! cp (car l) 'class)))
       `(,(if (eqv? mne 'instanceof) 193 192) ,@(int->2bytes index))))
    ;; Here are all cases where a wide instruction can occur, except
    ;; iinc.
    ((aload iload fload lload dload istore fstore astore lstore dstore ret)
     (let* ((index (car l))) 
       `(;; there could be a wide instruction.
	 ,@(wide-instr index)
	 ;; op-code
	 ,(if (and (not (eqv? mne 'ret)) (< index 4))
	      (+ (:~. mne '((iload . 26)  (aload . 42) (fload . 34)
			    (lload . 30)  (dload . 38) (istore . 59) (fstore . 67)
			    (astore . 75) (lstore . 63) (dstore . 71)))
		 index)
	      (:~. mne '((aload . 25)  (fload . 23)  (iload . 21)  (lload . 22)
			 (dload . 24)  (istore . 54) (fstore . 56) (astore . 58)
			 (lstore . 55) (dstore . 57) (ret . 169))))
	 ;; index
	 ,@(if (or (eqv? mne 'ret) (>= index 4))
	       (int->1-2bytes index)
	       '())
	 )))
    ((iinc)
     (let* ((index  (car l))
	    (const  (cadr l))
	    (indexB (int->1-2bytes index))
	    (constB (int->1-2bytes const))
	    (wide?  (or (> (length indexB) 1) (> (length constB) 1))))
       `(,@(if wide? '(196) '()) ;; wide
	 132 ;; iinc
	 ;; force two bytes for both operands if wide.
	 ,@(if wide? (int->2bytes index) indexB)
	 ,@(if wide? (int->2bytes const) constB))
       ))
    ((bipush)     `(16 ,@(int->bytes (car l) 1 1)))
    ((sipush)     `(17 ,@(int->bytes (car l) 2 2)))
    ((lookupswitch)
     ;; A large part of lookupswitch is encoded when the offsets of
     ;; branch instructions are determined. The padding are not
     ;; encoded here as the number of bytes to add is unknown at this
     ;; point.  pairs should be a list of pairs (c . label) where c is
     ;; an integer or 'default and label is (s label).  Pairs should
     ;; be sorted on the keys c.

     (let* ((pairs  (car l)))  `(switch ,pairs)))
    (else (error "CE, Ijvm, mne unkown: " mne))
    ))

(define (javaBool->int b)  (if (eqv? b 'true) 1 0))

;; Always return a list of four bytes encoding the value v, bigendian.
;; I: v, integer value in the range [-2147483648, 4294967295].
;; O: [bcode]
;;
(define (int->4bytes v)
  (if (> v 4294967295) (error "CE, int->4bytes, larger than 4294967295: " v))
  (if (< v -2147483648) (error "CE, int->4bytes, less than -2147483648: " v))
  (int->bytes v 4 4))

;; Always return a list of two bytes.
;; I: v, integer value in the range [-32768, 65535].
;; O: [bcode]
;;
(define (int->2bytes v)
  (if (> v 65535) (error "CE, int->2bytes, larger than 65535: " v))
  (if (< v -32768) (error "CE, int->2bytes, less than -32768: " v))
  (int->bytes v 2 2))

;; Returns bigendian two's complement byte codification of the integer
;; value of v.  This value can be negative. At least one and at most
;; two bytes are returned. The list of bytes never contains a negative
;; value. If the value v is in the range 128-255, it returns a list
;; containing this value and not two bytes (0 v).
;;
;; I: v, integer value in the range -32768 to 65535. 
;; O: [byte]
;;    
(define (int->1-2bytes v)
  (if (> v 65535) (error "CE, int->1-2bytes, larger than 65535: " v))
  (if (< v -32768) (error "CE, int->1-2bytes, less than -32768: " v))
  (let* ((r (int->bytes v 1 2)))
    (if (and (= 0 (car r)) (> (length r) 1)) (cdr r) r)))

(define (wide-instr index) (if (> index 255) (list 196) '()))

;; This is the function building the constant pool. All constants added
;; to the constant pool is done through this function.
;;
;; I:  cp, the constant pool
;;     kind, sym.
;; O: index in the constant pool of the added value
;;
(define (add-cp! cp value kind)
  (let* ((kind2 (if (symbol? kind) kind 'string))
	 (cst
	  (list
	   (kind->tag kind2)
	   (let* ((csts (vector-ref cp 0)))
	     (case kind2
	       ((utf8)         (str->utf8 value))
	       ((class string) (add-cp! cp value 'utf8))
	       ((boolean)      (add-cp! cp (javaBool->int value) 'int))
	       ((int)	
		(if (or (< value -4294967296) (> value 4294967295))
		    (error "CE, add-cp, long too large: " value))
		(if (< value 0) (complement2 (- value) 32) value))
	       ((double) (long->highLow (double->ieee64bits value)))
	       ((float)  (float->ieee32bits value))
	       ((long)
		(if (or (< value -9223372036854775808)
			(> value 9223372036854775807))
		    (error "CE, add-cp, long too large: " value))
		(let* ((valuep  (if (< value 0) (complement2 (- value) 64) value)))
		  (long->highLow valuep)))
	       ((nameAndType)
		(let* ((name  (add-cp! cp (car value) 'utf8))
		       (type  (add-cp! cp (cadr value) 'utf8)))
		  `(,name ,type)))
	       ((interfaceMethodRef methodRef fieldRef)
		(let* ((classIndex  (add-cp! cp (car value) 'class))
		       (nameAndType (add-cp! cp (cadr value) 'nameAndType)))
		  `(,classIndex ,nameAndType)))
	       (else (error "CE, add-cp!, unknown kind " kind2))
	       )))))
    ;; Constants are not repeated in the constant-pool.
    (if (member cst (vector-ref cp 0))
	(length (member cst (vector-ref cp 0)))
	(begin
	  ;; For long and double two entries are used.
	  (vector-set! cp 0 (if (member kind2 '(double long))
				(: 'unusable (: cst (vector-ref cp 0)))
				(: cst (vector-ref cp 0))))
	  ;; Index 0 is never returned.
	  (- (length (vector-ref cp 0)) (if (member kind2 '(double long)) 1 0)))
	)))

;; This function assumes that integer->character can be applied to the
;; range [0,255].
;; 
;; O: a couple (n str)
(define (str->utf8 s)
  (let loop ((l (string->list s)) (r '()))
    (if (pair? l)
	(let ((v (char->integer (car l))))
	  (if (and (not (= 0 v)) (>= 127 v))
	      (loop (cdr l) (: v r))
	      (if (or (= 0 v)  (and (> v 127) (< v 2048)))
		  (loop (cdr l) (: (+ 128 (remainder v 64))
				   (: (+ 192 (quotient v 64)) r)))
		  (loop (cdr l)
			(: (+ 128 (remainder v 64))
			   (: (+ 128 (quotient (remainder v 4096) 64))
			      (: (+ 224 (quotient v 4096)) r)))))))
	(list (length r) (list->string (map integer->char (reverse r)))))))
				    
(define (type->atype type)
  (:~. type '((boolean . 4) (char . 5) (float . 6) (double . 7)
	      (byte . 8) (short . 9) (int . 10) (long . 11))))

;; O : int
(define (kind->tag kind)
  (or (:~. kind '((utf8 . 1) (boolean . 3) (byte . 3) (short . 3) (char . 3)
		  (int . 3) (float . 4) (long . 5) (double . 6)
		  (class . 7) (string . 8) (fieldRef . 9) (methodRef . 10)
		  (interfaceMethodRef . 11) (nameAndType . 12)))
      (error "CE, kind->tag, unknown kind " kind)))

;; Generates a string describing the type according to the class pool
;; syntax of the JVM. The string generated is a fieldDescriptor as
;; specified in section 4.3 of JVS.
;;
;; I: type, (u 'int 'float 'byte 'short 'char 'long 'double 'boolean 'void
;;              (s tyA) (s type) (s tyM))
;;    kind, (u 'class 'noClass). TBF try to eliminate this argument.
;;           'class means that the descriptor is a filename not a type.
;;           For instanceof and checkcast, the descriptor of a class
;;           or interface without an array should not include de L and ;.
;; O: str
(define (T->jvmPool type kind)

  (define (primitiveType->letter ptype)
    (:~. ptype '((void . #\V) (int . #\I) (float . #\F) (double . #\D) (byte . #\B) 
		 (short . #\S) (long . #\J) (char . #\C) 
		 (boolean . #\Z) (undefined . #\U))))

  (cond
   ((symbol? type) (string (primitiveType->letter type)))
   ((qName? type)  (if (eqv? kind 'noClass)
		       (++ "L" (qName->classfName type) ";")
		       (qName->classfName type)))
   ((pair? type) ; This is an array describe by a couple (typeName dim)
    (++ (make-string (cadr type) #\[) (T->jvmPool (car type) 'noClass)))
   ((tyA? type)
    (++ (make-string (tyA-dim type) #\[) (T->jvmPool (tyA-type type) 'noClass)))
   ((type? type)
    (let* ((Tname  (++C (if (type-inside? type) 
			    (qName->classfName (get-pack-name type))   "")
			"/" (qName->classfName (type-name type)))))
      (if (eqv? kind 'noClass) (++ "L" Tname ";") Tname)))
   ((tyM? type)
    (let* ((parms      (++ (map (c.2 T->jvmPool 'noClass) (tyM-parm-types type))))
	   (descResult (T->jvmPool (tyM-rType type) 'noClass)))
      (++ "("parms")" descResult)))
   ((n-qName? type)  (T->jvmPool (n-qName-name type) kind))
   (else (error "CE, T->jvmPool, unknown type: " type))
   ))

(define (T->jvmType type)
  (cond
   ((type-primitive? type) type)
   ((or (tyA? type) (qName? type) (type? type)) 'reference)
   ;; TBF eventually semantic should not call T->jvmType and we would
   ;; remove the case undefined.
   ((typeUndef? type) 'undefined)
   (else (error "CE, T->jvmType, unknown type: " type))))





