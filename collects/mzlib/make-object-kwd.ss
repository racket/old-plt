#|

This is the abstract syntax for make-object/kwd:

  (make-object/kwd E (id E ...) ...)

make-object/kwd expands into make-object. It searches thru the list of 
methods and the names of the initialization arguments to find matches
for the provided keywords. There are three types of matches:

  - initialization arguments:
    In this case, make-object/kwd passes the value of the keyword
    to the initializer. Some of these arguments are required.
  - set/get methods:
    If the class has a set-<keyword>  method of arity n,
    where n is the number of arguments in the keyword (see alignment above)
    then set-<keyword> is called after the object is created to set the field
  - parameter-based settings:
    If the class has a <keyword> method of arity '(0 1), the
    method is called with the value of the keyword.

The only drawback (not counting the hackery involved) is that the
initialization arguments must be specified for each class, in the
macro definition, so it knows what arguments need to be passed as
keywords.

|#


(define-macro make-object/kwd
  (let ([real-args-table (make-hash-table)])
    (hash-table-put!
     real-args-table
     frame%
     `((label) (parent #f) (width #f) (height #f) (x #f) (y #f) (style ())))
    (hash-table-put! real-args-table horizontal-panel% `((parent) (style ())))
    (hash-table-put! real-args-table vertical-panel% `((parent) (style ())))
    (hash-table-put! real-args-table canvas% `((parent) (style ())))
    (hash-table-put! real-args-table button% `((label) (parent) (callback) (style ())))
    (hash-table-put! real-args-table editor-canvas% `((parent) (editor #f) (style ()) (scrolls-per-page 100)))
    (hash-table-put! real-args-table message% `((label) (parent) (style ())))
    
    
    (lambda (c . kwd-pairs)
      (unless (andmap (lambda (kwd)
			(list? kwd)
			(= 2 (length kwd))
			(symbol? (car kwd)))
		      kwd-pairs)
	(error 'make-object/kwd "malformed keywords"))
      (let* ([kwd-gensyms (map (lambda (x) (gensym "make-object/kwd/kwd")) kwd-pairs)]
	     [class-gensym (gensym "make-object/kwd/class")]
	     [kwds (map car kwd-pairs)]
	     [set-kwds (map (lambda (x) (string->symbol
					 (string-append
					  "set-"
					  (symbol->string x))))
			    kwds)]
	     [validate-keyword-before-object-creation
	      (lambda (kwd set-kwd)
		`(begin
		   (unless (or (memq ',kwd names)
                               (memq ',kwd arg/defaults-kwds)
                               (memq ',set-kwd names))
		     (error 'make-object/kwd "keyword ~a not available in class ~e"
			    ',kwd ,class-gensym))))]

	     [construct/validate-arguments
	      (lambda ()
                `(map (lambda (arg/default)
                        (cond
                          [(let loop ([kwd-pairs (list ,@(map (lambda (x)
                                                                `(list ',(car x)
                                                                       (lambda () ,(cadr x))))
                                                              kwd-pairs))])
                             (cond
                               [(null? kwd-pairs) #f]
                               [else (let ([kwd-pair (car kwd-pairs)])
                                       (if (eq? (car kwd-pair) (car arg/default))
                                           ((cadr kwd-pair))
                                           (loop (cdr kwd-pairs))))]))
                           =>
                           (lambda (x) x)]
                          [(not (null? (cdr arg/default)))
                           (cadr arg/default)]
                          [else
                           (error 'make-object/kwd "required keyword left unspecified: ~a"
                                  (car arg/default))]))
                      arg/defaults))]

	     [validate/invoke-keyword-after-object-creation
	      (lambda (gen kwd set-kwd)
		`(cond
                   [(memq ',kwd arg/defaults-kwds)
                    (void)]
                   [(memq ',set-kwd names)
                    (unless (equal? (length ,gen) (arity (ivar obj ,set-kwd)))
                      (error 'make-object/kwd "keyword ~a not available in class ~e"
                             ',kwd ,class-gensym))
                    (apply (ivar obj ,set-kwd) ,gen)]
                   [(memq ',kwd names)
                    (unless (equal? '(0 1) (arity (ivar obj ,kwd)))
                      (error 'make-object/kwd "keyword ~a not available in class ~e"
                             ',kwd ,class-gensym))
                    (send obj ,kwd ,gen)]))])
	`(let ([,class-gensym ,c]
	       ,@(map (lambda (gen kwd) `[,gen (list ,@(cdr kwd))]) kwd-gensyms kwd-pairs))
           
           (let ([arg/defaults (hash-table-get ,real-args-table ,class-gensym (lambda () #f))])
             (unless arg/defaults
               (error 'make-object/kwd "unknown class: ~e" ,class-gensym))

             (let ([arg/defaults-kwds (map car arg/defaults)]
                   [names (interface->ivar-names (class->interface ,class-gensym))])
               (begin ,@(map validate-keyword-before-object-creation
                             kwds
                             set-kwds))
               (let ([obj (apply make-object ,class-gensym ,(construct/validate-arguments))])
                 (begin ,@(map validate/invoke-keyword-after-object-creation
                               kwd-gensyms
                               kwds
                               set-kwds))
                 obj))))))))

