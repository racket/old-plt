(module tester mzscheme
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  (require (lib "thread.ss"))
  (require "private/growable-vector.ss"
	   "private/tester-view.ss"
	   "private/tester-model.ss"
	   "private/tester-structs.ss")
  
  (provide run-tests test test-error test-function
           test-manifest)

;; output-spec -> str
;; creates a printable string for the given output spec
(define (output-spec->str os)
  (cond
   [(error? os)
    (format "exception: ~a" (error-exception os))]
   [(finish? os)
    (format "result: ~n~v~n" (finish-value os))]
   [else (error (format "oops! received ~v" os))]))
  
(define (expect->str expect)
  (string-append
   (cond
     [(procedure? (expect-output-criterion expect))
      "<value passing given procedure>"]
     [else (output-spec->str (expect-output-criterion expect))])
   (if (expect-print expect)
       (format-printed-output-descrip (expect-print expect))
       "")))

;; format-printed-output-descrip : str -> str
(define (format-printed-output-descrip the-str)
  (format "~nPrinted output:~n~v" the-str))
     
;; received->str : received -> str
;; formats a received struct for printing
(define (received->str received)
  (string-append
   (output-spec->str (received-value received))
   (format-printed-output-descrip
    (received-print received))))
   
   
(define controller%
  (class* object% ()
    
    (public run-test-group test-manifest)
    
    (define print-error-message
      (lambda (test expect received)
        (send view set-text 
              (string-append
               "Test:\n"
               (format "~v" (test-text test))
               "\nDescription: "
               (format "~a" (test-description test))
               "\n\nExpected:\n"
               (expect->str expect)
               "\n\nReceived\n" 
               (received->str received)))))
    
    (field 
     [model (make-object model% 
              (lambda (number-of-loads)
                (send view single-msgarea-mode)
                (send view update-status "Loading tests")
                (send view set-gauge-size number-of-loads)
                (send view set-overall-size number-of-loads))
              (lambda (new-load-spec)
                (send view update (format "Loading ~v" new-load-spec)))
              (lambda () (send view tick-gauge))
              (lambda (total-tests)
                (send view reset-overall-gauge)
                (send view set-overall-size total-tests)
                (send view multi-msgarea-mode))
              (lambda (group-name size) 
                (send view update-status group-name)
                (send view set-gauge-size size))
              (case-lambda 
               [()
                (send view tick-gauge)]
               [(err) ;; (list str expect received)
                (begin
                  (apply print-error-message err)
                  (send view tick-gauge))])
              (lambda () (send view tick-gauge))
              (lambda () (wait-for-shutdown)))]
     [view
      (parameterize ([current-custodian  (make-custodian (current-custodian))]
                     [current-eventspace (make-eventspace)])
        (make-object view%
          (lambda (btn evnt)
            (if (send model prev-error?)
                (let ((new-err (send model get-prev-error)))
                  (apply print-error-message new-err))))
          (lambda (btn evnt)
            (if (send model next-error?)
                (let ((new-err (send model get-next-error)))
                  (apply print-error-message new-err))))))]
     [wait-thread (send view get-thread)])

    ;; run-test-group : test-group -> void
    ;; runs the given test-group
    (define run-test-group (lambda (x) (send model test x)))
    
    (define test-manifest
      (lambda (manifest)
        (send model run-all-tests manifest)))
    
    ;; wait-for-shutdown : -> void
    ;; blocks until the GUI has been closed
    (define wait-for-shutdown
        (lambda () (begin
                     (send view update-status "Done.")
                     (thread-wait wait-thread))))
    
    (super-instantiate ())))


      
  
;; =======================================================================
;; Program interface -- provides syntax and functions for attaching the
;; test suite to particular tests
  
;;; run-tests -- the main syntax for running tests. This is the syntax
;;; that the client test programs use.
;;; (run-tests str (listof test))
;;; test : (expr (error exn)) | (expr value) | (expr value str)
;;; see test-suite user's guide for usage
;(define-syntax (run-tests stx)
;  (syntax-case stx ()
;    [(_ name test ...)
;     (with-syntax
;      ([(real-test ...) (map
;                         (lambda (x)
;                           (syntax-case x ()
;                             [(test-type descrip body arg ...)
;                              (with-syntax
;                                  ([expect
;                                    (syntax-case (syntax test-type) (runtest test-error test-function)
;                                      [run-test
;                                       (syntax-case (syntax (arg ...)) ()
;                                         [(expect) (syntax (make-finish expect #f))]
;                                         [(expect out) (syntax (make-finish expect out))])]
;                                      [test-error
;                                       (syntax-case (syntax (arg ...)) ()
;                                         [(exn-str) (syntax (make-error exn-str))])]
;                                      [test-function
;                                       (syntax-case (syntax (arg ...)) ()
;                                         [(fn) (syntax fn)])])])
;                                (syntax (make-test descrip
;                                                   (quote body)
;                                                   (lambda () body)
;                                                   expect)))]))
;                         (syntax->list (syntax (test ...))))])
;       (syntax (run-tests-int name (list real-test ...))))]))

;; test-struct : (make-test str sexp (-> value) expectation)
;; test : (syntax (_ str sexp expectation)) -> (syntax test-struct)
;;        (syntax (_ str sexp expectation (union str #f))) -> (syntax test-struct)
;; test-error : (syntax (_ str sexp str))  -> (syntax test-struct)
;; test-function : (syntax (_ str sexp (value -> bool))) -> (syntax test-struct)
(define-syntaxes (test test-error test-function)
  (let ((syntax->test-struct
         (lambda (x)
           (syntax-case x ()
             [(test-type descrip body compare out ...)
              (with-syntax
                  ([expect-criterion
                    (syntax-case (syntax test-type) (test test-error test-function)
                      [test 
                       (syntax (make-finish compare))]
                      [test-error
                       (syntax (make-error compare))]
                      [test-function
                       (syntax compare)])]
                   [printed-output
                    (syntax-case (syntax (out ...)) ()
                      [() (syntax #f)]
                      [(str) (syntax str)])])
                (syntax (make-test descrip
                                   (quote body)
                                   (lambda () body)
                                   (make-expect expect-criterion printed-output))))]))))
    (values syntax->test-struct syntax->test-struct syntax->test-struct)))
           
(define tester (make-object controller%))
  
;; run-tests-int : str (listof test) -> void
;; side effect: runs all the tests.
(define run-tests
  (lambda (name . tests)
    (send tester run-test-group (make-test-group name tests))))

;; test-manifest : (listof require-spec-datum) -> void
;; side effect: tests the given manifest.
(define (test-manifest man)
  (send tester test-manifest man)))
  

