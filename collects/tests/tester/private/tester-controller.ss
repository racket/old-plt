(module tester-controller mzscheme
  
  (require (lib "mred.ss" "mred"))
  (require (lib "class.ss"))
  (require (lib "thread.ss"))
  (require "growable-vector.ss"
	   "tester-view.ss"
	   "tester-model.ss"
	   "tester-structs.ss")
  (provide controller%)
  
;; output-spec -> str
;; creates a printable string for the given output spec
(define (output-spec->str os)
  (cond
   [(error? os)
    (format "exception: ~a" (error-exception os))]
   [(finish? os)
    (format "result: ~n~v~n" (finish-value os))]
   [else (error (format "oops! received ~v" os))]))
  
;; expect->str : expectation -> str
;; creates a printable string for the given expectation
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
;; formats a description of printed output.
(define (format-printed-output-descrip the-str)
  (format "~nPrinted output:~n~v" the-str))
     
;; received->str : received -> str
;; formats a received struct for printing
(define (received->str received)
  (string-append
   (output-spec->str (received-value received))
   (format-printed-output-descrip
    (received-print received))))
   

;; The tester GUI's controller, responsible for translating
;; requests in the model's domain into requests in the view's domain
;; and vice versa.   
(define controller%
  (class* object% ()
    
    (public run-test-group 
            test-manifest
            test-one-group)

    ;; print-test-error : test x expectation x received -> void
    ;; side-effect: prints the given error message in the view
    (define print-test-error
      (lambda (test expect received)
        (send view set-test-text
              (string-append
               "Test:\n"
               (format "~v" (test-text test))
               "\nDescription: "
               (format "~a" (test-description test))
               "\n\nExpected:\n"
               (expect->str expect)
               "\n\nReceived\n" 
               (received->str received)))))
    
    (define selection-list null)
    (define view
      (make-object view%
        
        ; okay-press : (listof (list str val (listof (list str val)))
        (lambda (btn evnt)
          (thread
           (lambda ()
             (send model run-tests (send view get-current-test-selections)))))
        
        ; prev-error
        (lambda (btn evnt)
          (if (send model prev-error?)
              (let ((new-err (send model get-prev-error)))
                (apply print-test-error new-err))))
        
        ; next-error
        (lambda (btn evnt)
          (if (send model next-error?)
              (let ((new-err (send model get-next-error)))
                (apply print-test-error new-err))))))
     
    (define model 
      (make-object model%
        
        ; gui:in-init : -> void
        (lambda () (void))
        
        ; gui:in-choose : (listof test-group) -> void
        (lambda (tgs) 
          (send view set-mode 'choose)
          (set! selection-list tgs)
          (let ((selection-list
                 (map (lambda (tg)
                        (list (test-group-description tg)
                              tg
                              (map 
                               (lambda (t)
                                 (list (test-description t)
                                       t))
                               (test-group-tests tg))))
                      tgs)))
            (send view set-selections selection-list)))
        
        ; gui:in-done : -> void
        (lambda () (send view update-status "Done."))
        
        ; gui:init->choose : natnum -> void
        (lambda (number-of-loads)
          (send view set-mode 'load)
          (send view update-status "Loading tests")
          (send view set-loading-gauge-size number-of-loads))
        
        ; gui:loading-new-module : require-spec-datum -> void
        (lambda (new-load-spec)
          (send view update-load-text (format "Loading ~v" new-load-spec)))
        
        ; gui:successful-module-load : -> void
        (lambda () (send view tick-loading-gauge))
        
        ; gui:failed-module-load : exn -> void
        (lambda (exn) 
          (begin
            (send view tick-loading-gauge)
            (send view update-load-text (format "Error occured: ~a" (exn-message exn)))
            (printf "loading error: ~a" (exn-message exn))))
        
        ; gui:choose->done
        (lambda (total-tests)
          (send view set-mode 'test)
          (send view set-test-text "No failed tests.")
          (if (> total-tests 0)
              (send view set-overall-gauge-size total-tests)))
        
        ; gui:testing-new-group : str x natnum -> void
        (lambda (group-name size) 
          (send view update-status group-name)
          (send view set-this-group-gauge-size size))
        
        ; gui:report-test-passed : test -> void
        (lambda (test) (send view tick-testing-gauges))
        
        ; gui:report-test-failed : test expect received -> void
        (let ((first-error? #t))
          (lambda (test expect resl)
            (if first-error?
                (begin
                  (print-test-error test expect resl)
                  (set! first-error? #f)))
            (send view tick-testing-gauges)))))

    ;; run-test-group : test-group -> void
    ;; runs the given test-group
    (define run-test-group (lambda (x) (send model test x)))

    ;; test-manifest : (listof module-spec-datum) -> ??ANSWER??
    ;; tests the given manifest. returns the thread on which
    ;; the gui is running.
    (define test-manifest
      (lambda (manifest)
        (send model load-tests manifest)))
    
    ;; test-one-group : test-group -> ??ANSWER??
    ;; does a complete test (skipping queueing) with just the one
    ;; given group. Returns the thread on which the gui is running
    (define (test-one-group group)
      (send model run-test-groups (list group)))
    
    (super-instantiate ()))))

