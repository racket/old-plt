
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
      ;; type sc = (make-sc symbol (listof (list symbol string)))
      (define-struct sc (language-name constants))
      
      ;; string-constants-file-cache : (box (listof (list sym (listof (list sym string)))))
      ;; first listof is the list of languages, second listof is the list of string-constants
      (define string-constants-file-cache #&())

      ;; get-string-constants : string -> (listof (list sym string))
      (define (get-string-constants filename)
        (let* ([key (string->symbol filename)]
               [res (assq key (unbox string-constants-file-cache))])
          (if res
              (cadr res)
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
                                      (format "expected `((<symbol> <string>) ...), got: ~s" sexp)))
                (let ([ht (make-hash-table)])
                  (for-each (lambda (x) 
                              (when (hash-table-get ht (car x) (lambda () #f))
                                (raise-syntax-error
                                 'string-constants 
                                 (format "found duplicate for ~a in ~a"
                                         (car x) filename)))
                              (hash-table-put! ht (car x) (cadr x)))
                            sexp))
                (set-box! string-constants-file-cache
                          (cons (list key sexp) 
                                (unbox string-constants-file-cache)))
                sexp))))
      
      (define available-string-constant-sets
        (list 
         (make-sc 'english (get-string-constants "english-string-constants.ss"))
         (make-sc 'spanish (get-string-constants "spanish-string-constants.ss"))
         (make-sc 'french (get-string-constants "french-string-constants.ss"))
         (make-sc 'german (get-string-constants "german-string-constants.ss"))
         (make-sc 'dutch (get-string-constants "dutch-string-constants.ss"))
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
               ;; type no-warning-cache-key = (cons symbol symbol)
               ;; warning-table : (listof (list no-warning-cache-key (listof (list sym string))))
	       [warning-table null]
	       [check-one-way
		(lambda (sc1 sc2)
		  (let ([assoc1 (sc-constants sc1)]
			[assoc2 (sc-constants sc2)])
		    (for-each
		     (lambda (pair1)
                       (let* ([constant1 (car pair1)]
                              [value1 (cadr pair1)]
                              [pair2 (assq constant1 assoc2)])
                         (unless pair2
                           (let ([no-warning-cache-key (cons (sc-language-name sc1) (sc-language-name sc2))])
                             (unless (unbox already-printed)
                               (when (or (env-var-set? (sc-language-name sc1))
                                         (env-var-set? (sc-language-name sc2)))
                                 (cond
                                   [(memf (lambda (x) (equal? (car x) no-warning-cache-key)) warning-table)
                                    =>
                                    (lambda (x)
                                      (let ([ent (car x)])
                                        (set-car! (cdr ent) (cons (list constant1 value1) (cadr ent)))))]
                                   [else
                                    (set! warning-table (cons (list no-warning-cache-key
                                                                    (list (list constant1 value1)))
                                                              warning-table))])))))))
		     assoc1)))])

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
           (let ([assoc-table (sc-constants first-string-constant-set)]
                 [datum (syntax-object->datum (syntax name))])
             (unless (symbol? datum)
               (raise-syntax-error #f
                                   (format "expected name, got: ~s" datum)
                                   stx))
             (let ([default-val (assq datum assoc-table)])
               (unless default-val
                 (raise-syntax-error 
                  #f
                  (format "~a is not a known string constant" datum) 
                  stx))
               (with-syntax ([(constants ...) (map (lambda (x)
                                                     (let ([val (assq datum (sc-constants x))])
                                                       (if val
                                                           (cadr val)
                                                           (cadr default-val))))
                                                   available-string-constant-sets)]
                             [(languages ...) (map sc-language-name available-string-constant-sets)]
                             [first-constant (cadr (assq datum (sc-constants first-string-constant-set)))])
                 (syntax (cond
                           [(eq? language 'languages) constants] ...
                           [else first-constant])))))]))
      
      (define (string-constants stx)
        (syntax-case stx ()
          [(_ name)
           (let ([assoc-table (sc-constants first-string-constant-set)]
                 [datum (syntax-object->datum (syntax name))])
             (unless (symbol? datum)
               (raise-syntax-error #f
                                   (format "expected name, got: ~s" datum)
                                   stx))
             (let ([default-val (assq datum assoc-table)])
               (unless default-val
                 (raise-syntax-error 
                  #f
                  (format "~a is not a known string constant" datum)
                  stx))
               (with-syntax ([(constants ...) (map (lambda (x) 
                                                     (let ([val (assq datum (sc-constants x))])
                                                       (if val
                                                           (cadr val)
                                                           (cadr default-val))))
                                                   available-string-constant-sets)])
                 (syntax (list constants ...)))))]))
      
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
