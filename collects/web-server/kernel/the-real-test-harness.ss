(module the-real-test-harness mzscheme
  (provide test
           tests
           run-suites
           run-suites/file
           reset-tests!
           report/concise
           report/failures
           raise/id
           require/expose)

  ;; Comment these out for 299
;  (define path->string (lambda (x) x))
;  (define string->path (lambda (x) x))


   ;; Requires a module and exposes some of its unprovided (non-syntax!)
  ;; identifiers.
  ;; USAGE: (require/expose MODULE-NAME (IDS ...))
  ;;   where MODULE-NAME is as in the MzScheme manual (i.e., a standard
  ;;   module spec) and IDS are the un-provided identifiers that you wish to
  ;;   expose in the current module.
  ;; Based on a macro by Raymond Racine (rracine@adelphia.net) posted to
  ;; plt-scheme in Dec 2003.
  (define-syntax require/expose
    (syntax-rules ()
      [(_ mod (ids ...))
       (begin
         (require mod)
         (define-values (ids ...)
           (let ([ns (module->namespace 'mod)])
             (parameterize ([current-namespace ns])
               (values
                (namespace-variable-value 'ids)...)))))]))

  (define-syntax tests
    (lambda (stx)
      (syntax-case stx ()
        [(_ rest ...) (syntax (begin rest ...))])))

  ;; Notes:
  ;; maybe want to get line information from tests?
  ;; syntax-line
  ;; create a syntax object at the point you want the line-number
  ;; e.g. (syntax-line #'here)

  (define next-result-id 0)
  (define the-results '())
  (define current-test-suite (make-parameter "test-harness.ss"))

  ;; reset-tests!: -> void
  ;; reset the results and the ids
  (define (reset-tests!)
    (set! next-result-id 0)
    (set! the-results '()))

  ;; two types of results, successes and failures and variants thereof
  (define-struct result (test-name id) (make-inspector))
  (define-struct (success result) () (make-inspector))
  (define-struct (failure result) () (make-inspector))
  (define-struct (failure/exn failure) (exception) (make-inspector))

  ;; test: string (-> boolean) -> void
  ;; run a test and update the globals
  (define (test test-name run-it)
    (let ([test-name
           (string-append (current-test-suite) "::" test-name)])
      (let ([result
             (with-handlers
                 ([exn? (lambda (the-exn)
                          (make-failure/exn test-name next-result-id the-exn))])
               (if (run-it)
                   (make-success test-name next-result-id)
                   (make-failure test-name next-result-id)))])
        (set! next-result-id (add1 next-result-id))
        (set! the-results (cons result the-results)))))


  ;; import-modules is a list of symbols, each symbol is a module name
  (define import-modules
    (map
     (lambda (m-spec)
       (if (symbol? m-spec)
           m-spec
           ((current-module-name-resolver) m-spec #f #f)))
     '(mzscheme "the-real-test-harness.ss")))


  ;; make-test-suite-namespace: -> namespace
  ;; create a namespace with the test-harness module attached
  (define (make-test-suite-namespace)
    (let ([test-harness-namespace (current-namespace)]
          [test-suite-namespace (make-namespace 'initial)])
      (parameterize ([current-namespace test-suite-namespace])
        (for-each
         (lambda (name)
           (namespace-attach-module test-harness-namespace name))
         import-modules)
        test-suite-namespace)))


  ;; load-test-suite/path string -> void
  (define (load-test-suite/path suite-path-str)
    (parameterize ([current-test-suite
                    (string-append (current-test-suite) "::" suite-path-str)]
                   [current-namespace (make-test-suite-namespace)]
                   [current-module-name-resolver (make-resolver
                   (suite-path->predicate suite-path-str))])
      (namespace-require `(lib "errortrace.ss" "errortrace"))
      (namespace-require `(file ,suite-path-str))))

  (define suite-path-re (regexp "(.*)(\\.ss|\\.scm)$"))
  ;; suite-path->predicate: string -> (symbol -> boolean)
  (define (suite-path->predicate suite-path-str)
    (let ([simple-symbol (string->symbol (cadr (regexp-match suite-path-re suite-path-str)))]
          [complex-symbol (initial-resolver suite-path-str #f #f)])
      (lambda (sym)
        (or (eqv? sym simple-symbol)
            (eqv? sym complex-symbol)))))

  (define initial-resolver (current-module-name-resolver))
;    (let ([ir (current-module-name-resolver)])
;      (lambda args
;        (let ([res (apply ir args)])
;          (printf "initial-resolver-returned: ~s~n" res)
;          res))))

  ;; make-resolver: (symbol -> boolean) -> arbitrary-value symbol (union syntax-object #f) -> symbol
  ;; A module name resolver
  (define (make-resolver is-source?)
    (lambda (module-spec source-module-symbol syntax-or-false)
      (cond
        [(and (is-source? source-module-symbol) (equal? module-spec "test-harness.ss"))
         (initial-resolver "the-real-test-harness.ss" source-module-symbol syntax-or-false)]
        [else
         (initial-resolver module-spec source-module-symbol syntax-or-false)])))

  ;; run all the test-suites identified by the-suites
  (define (run-suites a-suite . rest-suites)
    (for-each
     (lambda (suite-name)
       (load-test-suite/path suite-name))
     (cons a-suite rest-suites))
    (report/concise))

  ;; run-suites/file: string -> void
  (define (run-suites/file filename)
    (with-input-from-file filename
      (lambda ()
        (apply run-suites (read)))))


  ;; report/concise: ->
  ;; print a concise report of the current test results
  (define (report/concise)
    (let loop ([l the-results]
               [n-pass 0]
               [n-fail 0]
               [n-exn 0])
      (cond
       [(null? l)
        (printf "Test Results~a~a~a~a~n"
                (format "~n   total tests: ~a" (+ n-pass n-fail n-exn))
                (format "~n   number passed: ~a" n-pass)
                (format "~n   number failed: ~a" n-fail)
                (format "~n   number of exceptions: ~a" n-exn))]
       [(success? (car l))
        (loop (cdr l) (add1 n-pass) n-fail n-exn)]
       [(failure/exn? (car l))
        (loop (cdr l) n-pass n-fail (add1 n-exn))]
       [else
        (loop (cdr l) n-pass (add1 n-fail) n-exn)])))

  ;; print-results: (listof result) -> void
  ;; print a list of resuls
  (define (print-results l-res)
    (printf "   result-id   test-name~n")
    (for-each
     (lambda (res)
       (printf "       ~a       ~a~n"
               (result-id res)
               (result-test-name res)))
     l-res))

  ;; report/failures: -> void
  ;; print a verbose report of the failures in teh current test results
  (define (report/failures)
    (let loop ([l the-results]
               [l-fail '()]
               [l-exn '()])
      (cond
       [(null? l)
        (printf "Test Results (failures):~n")
        (printf "   FAILURES:~n")
        (print-results l-fail)
        (printf "   EXCEPTIONS:~N")
        (print-results l-exn)]
       [(success? (car l)) (loop (cdr l) l-fail l-exn)]
       [(failure/exn? (car l))
        (loop (cdr l) l-fail (cons (car l) l-exn))]
       [else
        (loop (cdr l) (cons (car l) l-fail) l-exn)])))

  ;; my-member: number (listof alpha) (number alpha -> boolean)
  ;;            -> (listof alpha)
  ;; srfi 1 was a hassle to compile just now so I'll write my own member...
  (define (my-member id l same?)
    (cond
     [(null? l) #f]
     [(same? id (car l)) l]
     [else
      (my-member id (cdr l) same?)]))

  ;; raise/id: number -> void
  ;; raise one of the exceptions from the list of exceptions
  (define (raise/id the-id)
    (let* ([the-result
            (my-member the-id the-results
                       (lambda (id res)
                         (= id (result-id res))))])
      (and the-result
           (failure/exn? (car the-result))
           (raise (failure/exn-exception (car the-result))))))

  )
