(module utils mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide unpack-submission
	   
	   unpack-test-suite-submission
	   is-test-suite-submission?)

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

      (super-new))))

