
(module string-constant mzscheme
  (require-for-syntax (lib "etc.ss")
		      (lib "list.ss"))
  (require (lib "file.ss"))

  (provide string-constant string-constants this-language all-languages set-language-pref)
  
  ;; set-language-pref : symbol -> void
  (define (set-language-pref language)
    (put-preferences (list 'plt:human-language) (list language)))
  
  ;; language : symbol
  (define language (get-preference 'plt:human-language (lambda () 'english)))
  
  (define-syntaxes (string-constant string-constants this-language all-languages)
    (let ()
      ;; type sc = (make-sc symbol (listof (hash-table symbol string)))
      (define-struct sc (language-name constants))
      
      (define string-constants-file-cache #&#f)

      (define (get-string-constants filename)
	(unless (unbox string-constants-file-cache)
	  (set-box! string-constants-file-cache (make-hash-table)))
	(let ([key (string->symbol filename)])
	  (hash-table-get
	   (unbox string-constants-file-cache)
	   key
	   (lambda ()
	     (let* ([filename (build-path (this-expression-source-directory) filename)]
		    [sexp (call-with-input-file filename read 'text)])
	       (unless (and (list? sexp)
			    (andmap (lambda (x) 
				      (and (list? x)
					   (= 2 (length x))
					   (symbol? (car x))
					   (string? (cadr x))))
				    sexp))
		 (raise-syntax-error 'string-constant
				     (format "expected `((,symbol string) ...), got: ~s" sexp)))
	       (let ([ht (make-hash-table)])
		 (for-each (lambda (x) 
			     (when (hash-table-get ht (car x) (lambda () #f))
			       (raise-syntax-error
				'string-constants 
				(format "found duplicate for ~a in ~a"
					(car x) filename)))
			     (hash-table-put! ht (car x) (cadr x)))
			   sexp)
		 (hash-table-put!
		  (unbox string-constants-file-cache)
		  key
		  ht)
		 ht))))))
      
      (define available-string-constant-sets
        (list 
         (make-sc 'english (get-string-constants "english-string-constants.ss"))
         (make-sc 'spanish (get-string-constants "spanish-string-constants.ss"))
         (make-sc 'french (get-string-constants "french-string-constants.ss"))
         (make-sc 'german (get-string-constants "german-string-constants.ss"))
         (make-sc 'danish (get-string-constants "danish-string-constants.ss"))))
      
      (define first-string-constant-set (car available-string-constant-sets))
      
      ;; env-var-set? : symbol -> boolean
      ;; returns #t if the user has requested this langage info.
      ;; If the environment variable is set to something that
      ;; isn't well-formed according to `read' you get all output
      ;; If the environment variable is set to a symbol (according to read)
      ;; you get that language. If it is set to a list of symbols
      ;; (again, according to read) you get those langauges.
      ;; if it is set to anything else, you get all langauges.
      (define (env-var-set? lang)
	(let ([var (or (getenv "PLTSTRINGCONSTANTS")
		       (getenv "STRINGCONSTANTS"))])
	  (and var
	       (with-handlers ([exn:read? (lambda (x) #t)])
		 (let ([specific (read (open-input-string var))])
		   (cond
		     [(symbol? specific) (eq? lang specific)]
		     [(list? specific) (memq lang specific)]
		     [else #t]))))))

      (define dummy
        (let* ([already-printed #&#f]
	       [warning-table null]
	       [check-one-way
		(lambda (sc1 sc2)
		  (let ([ht1 (sc-constants sc1)]
			[ht2 (sc-constants sc2)])
		    (hash-table-for-each
		     ht1
		     (lambda (constant value)
		       (unless (hash-table-get ht2 constant (lambda () #f))
			 (let ([no-warning-cache-key
                                (cons (sc-language-name sc1) (sc-language-name sc2))])
			   (hash-table-put! ht2 constant value)
			   (unless (unbox already-printed)
                             (when (or (env-var-set? (sc-language-name sc1))
				       (env-var-set? (sc-language-name sc2)))
                               (cond
                                 [(memf (lambda (x) (equal? (car x) no-warning-cache-key)) warning-table)
                                  =>
                                  (lambda (x)
                                    (let ([ent (car x)])
                                      (set-car! (cdr ent) (cons (list constant value) (cadr ent)))))]
                                 [else
                                  (set! warning-table (cons (list no-warning-cache-key
                                                                  (list (list constant value)))
                                                            warning-table))])))))))))])

          (for-each (lambda (x) 
                      (check-one-way x first-string-constant-set)
                      (check-one-way first-string-constant-set x))
                    (cdr available-string-constant-sets))
          
	  ;; in some cases, the printf may raise an exception because the error port
	  ;; is gone. If so, just don't display the warning.
	  (unless (unbox already-printed)
	    (with-handlers ([not-break-exn?
			     (lambda (x) (void))])
	      (set-box! already-printed #t)
	      (for-each
	       (lambda (bad)
		 (let* ([lang-pair (car bad)]
                        [constants (cadr bad)]
                        [lang1-name (car lang-pair)]
                        [lang2-name (cdr lang-pair)])
		   (fprintf
		    (current-error-port)
		    "WARNING: language ~a had but ~a does not:\n"
		    lang1-name
		    lang2-name)
                   (for-each
                    (lambda (x) (fprintf (current-error-port) "   ~s\n" x))
                    (quicksort
		     constants
		     (lambda (x y) (string<=? (symbol->string (car x)) (symbol->string (car y))))))
                   (newline (current-error-port))))
	       warning-table)))))
           
      (define (string-constant stx)
        (syntax-case stx ()
          [(_ name)
           (let ([ht (sc-constants first-string-constant-set)]
                 [datum (syntax-object->datum (syntax name))])
             (unless (symbol? datum)
               (raise-syntax-error #f
                                   (format "expected name, got: ~s" datum)
                                   stx))
             (unless (hash-table-get ht datum (lambda () #f))
               (raise-syntax-error 
                #f
                (format "~a is not a known string constant" datum) 
                stx))
             (with-syntax ([(constants ...) (map (lambda (x) (hash-table-get (sc-constants x) datum))
                                                 available-string-constant-sets)]
                           [(languages ...) (map sc-language-name available-string-constant-sets)]
                           [first-constant (hash-table-get (sc-constants first-string-constant-set) datum)])
               (syntax (cond
                         [(eq? language 'languages) constants] ...
                         [else first-constant]))))]))
      
      (define (string-constants stx)
        (syntax-case stx ()
          [(_ name)
           (let ([ht (sc-constants first-string-constant-set)]
                 [datum (syntax-object->datum (syntax name))])
             (unless (symbol? datum)
               (raise-syntax-error #f
                                   (format "expected name, got: ~s" datum)
                                   stx))
             (unless (hash-table-get ht datum (lambda () #f))
               (raise-syntax-error 
                #f
                (format "~a is not a known string constant" datum)
                stx))
             (with-syntax ([(constants ...) (map (lambda (x) (hash-table-get (sc-constants x) datum))
                                                 available-string-constant-sets)])
               (syntax (list constants ...))))]))
      
      (define (this-language stx)
        (syntax-case stx ()
          [(_)
           (syntax language)]))
      
      (define (all-languages stx)
        (syntax-case stx ()
          [(_) 
           (with-syntax ([(languages ...) (map sc-language-name available-string-constant-sets)])
             (syntax (list 'languages ...)))]))
      
      (values
       string-constant
       string-constants
       this-language
       all-languages))))

#|
(require string-constant)
(string-constant is-this-your-native-language)
(string-constants is-this-your-native-language)
(all-languages)
(this-language)
(expand #'(string-constant is-this-your-native-language))
(expand #'(string-constants is-this-your-native-language))
|#
