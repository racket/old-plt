(module utils mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide unpack-submission
	   
	   unpack-test-suite-submission
	   is-test-suite-submission?

	   make-evaluator
	   evaluate-all
	   evaluate-submission
	   reraise-exn-as-submission-problem)

  (define (unpack-submission str)
    (let* ([base (make-object editor-stream-in-string-base% str)]
	   [stream (make-object editor-stream-in% base)]
	   [definitions-text (make-object text%)]
	   [interactions-text (make-object text%)])
      (read-editor-version stream base #t)
      (read-editor-global-header stream)
      (send definitions-text read-from-file stream)
      (send interactions-text read-from-file stream)
      (read-editor-global-footer stream)
      (values definitions-text interactions-text)))

  (define (unpack-test-suite-submission str)
    (let* ([base (make-object editor-stream-in-string-base% str)]
	   [stream (make-object editor-stream-in% base)]
	   [ts (make-object ts-load%)])
      (read-editor-version stream base #t)
      (read-editor-global-header stream)
      (send ts read-from-file stream)
      (read-editor-global-footer stream)
      ts))

  (define (is-test-suite-submission? str)
    (with-handlers ([not-break-exn? (lambda (x) #f)])
      (send (unpack-test-suite-submission str)
	    got-program?)))

  ;; Test Suite Unpacking ----------------------------------------
  ;; This code duplicates just enough of the test-suite snips
  ;;  to make test-suite files readable.
  
  (define program-header-field-name "drscheme:test-suite:program")

  (define csc (new
	       (class snip-class%
		 (define/override (read f)
		   (let ([case (new case%)])
		     (send case read-from-file f)
		     case))
		 (super-new))))
  (send csc set-classname "case%")
  (send csc set-version 1)
  (send (get-the-snip-class-list) add csc)

  (define case%
    (class editor-snip%
      (inherit set-snipclass get-editor)

      (define call (new text%))
      (define expected (new text%))
      (define test (new text%))

      (define/public (read-from-file f)
	(send call read-from-file f)
	(send expected read-from-file f)
	(send test read-from-file f)
	(send f get-string))

      (super-new)

      (set-snipclass csc)
      (send (get-editor) insert (make-object editor-snip% call))
      (send (get-editor) insert (make-object editor-snip% expected))
      (send (get-editor) insert (make-object editor-snip% test))))

  (define ts-load%
    (class pasteboard%
      (define program (new text%))

      (define got-p? #f)
      (define/public (got-program?) got-p?)

      (rename [super-read-header-from-file read-header-from-file])
      (define/override (read-header-from-file stream name)
	(if (string=? name program-header-field-name)
	    (begin
	      (set! got-p? #t)
	      (send program read-from-file stream))
	    (super-read-header-from-file stream name)))

      (super-new)))


  ;; Execution ----------------------------------------

  (define (make-evaluator language teachpacks)
    (let ([ns (make-namespace-with-mred 'empty)])
      (parameterize ([current-namespace ns]
		     [read-case-sensitive #t])
	(parameterize ([current-eventspace (make-eventspace)])
	  (namespace-require `(lib ,(case language
				      [(beginner) "htdp-beginner.ss"]
				      [(beginner-abbr) "htdp-beginner-abbr.ss"]
				      [(intermediate) "htdp-intermediate.ss"]
				      [(intermediate-lambda) "htdp-intermediate-lambda.ss"]
				      [(advanced) "htdp-advanced.ss"])
				   "lang"))
	  (for-each (lambda (tp)
		      (namespace-require `(file ,tp)))
		    teachpacks)
	  (let ([ch (make-channel)]
		[result-ch (make-channel)])
	    (queue-callback
	     (lambda ()
	       (let loop ()
		 (let ([expr (channel-get ch)])
		   (unless (eof-object? expr)
		     (with-handlers ([void (lambda (exn)
					     (channel-put result-ch (cons 'exn exn)))])
		       (channel-put result-ch (cons 'val (eval expr))))
		     (loop))))
	       (let loop ()
		 (channel-put result-ch '(exn . no-more-to-evaluate))
		 (loop))))
	    (lambda (expr)
	      (channel-put ch expr)
	      (let ([r (channel-get result-ch)])
		(if (eq? (car r) 'exn)
		    (raise (cdr r))
		    (cdr r)))))))))

  (define (evaluate-all source port eval)
    (let loop ()
      (let ([expr (read-syntax source port)])
	(unless (eof-object? expr)
	  (eval expr)
	  (loop)))))

  (define (evaluate-submission str eval)
    (let-values ([(defs interacts) (unpack-submission str)])
      (evaluate-all 'handin (open-input-text-editor defs) eval)))

  (define (reraise-exn-as-submission-problem thunk)
    (with-handlers ([void (lambda (exn)
			    (error
			     (format "ERROR: ~a"
				     (if (exn? exn)
					 (exn-message exn)
					 (format "~s" exn)))))])
      (thunk)))
  
  )

