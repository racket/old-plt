; this module is a cheap hack; it interacts with the debugger 
; REPL by getting & setting values in the top-level environment

(module debugger-bindings mzscheme
  (require (lib "contract.ss")
           "marks.ss"
           (lib "etc.ss")
           (lib "list.ss"))
  
  (provide/contract [set-event-num! (-> number? void?)] 
                    [bt (-> void?)] 
                    [set-frame-num! (-> number? void?)]
                    [src (-> void?)]
                    [binding (-> symbol? any)])

  (provide install-debugger-bindings)

  (define (install-debugger-bindings)
    ; yuck!  dependence on the list of names provided by the module
    (namespace-set-variable-value! 'e set-event-num!)
    (namespace-set-variable-value! 'bt bt)
    (namespace-set-variable-value! 'f set-frame-num!)
    (namespace-set-variable-value! 'src src)
    (namespace-set-variable-value! 'v binding)
    (namespace-set-variable-value! 'c continue)
    (namespace-set-variable-value! 'help help))
  
  (define (help)
    (printf "Help Summary:\n")
    (call-with-input-file (build-path (collection-path "stepper" "private") "debugger-summary.txt")
      (lambda (port)
	(let loop ([line (read-line port)])
	  (unless (eof-object? line)
          (printf "~a\n" line)
          (loop (read-line port)))))))

  (define (continue)
    (semaphore-post (namespace-variable-value 'go-semaphore)))
  
  (define (events)
    ((namespace-variable-value 'events)))
  
  (define (current-event-num)
    (namespace-variable-value 'current-event-num))

  (define (current-event)
    (list-ref (events) (current-event-num)))
  
  (define (current-mark-list)
    (unless (normal-breakpoint-info? (current-event))
      (error 'current-mark-list "current event is not an event with a mark list: ~v" (current-event)))
    (normal-breakpoint-info-mark-list (current-event)))
  
  (define (current-frame-num)
    (namespace-variable-value 'current-frame-num))
  
  (define (current-frame)
    (list-ref (current-mark-list) (current-frame-num)))
  
  (define (check-range num bottom top)
    (when (or (< num bottom) (> num top))
      (error 'check-range "argument ~v out of range [~v ... ~v]" num bottom top)))
  
  (define (set-event-num! num)
    (check-range num 0 (- (length (events)) 1))
    (namespace-set-variable-value! 'current-event-num num)
    (namespace-set-variable-value! 'current-frame-num 0))
  
  (define (set-frame-num! num)
    (check-range num 0 (- (length (current-mark-list)) 1))
    (namespace-set-variable-value! 'current-frame-num num))
  
  (define (bt)
    (for-each 
     (lambda (mark num)
       (printf "~v: ~v\n" num (syntax-object->datum (mark-source mark))))
     (current-mark-list)
     (build-list (length (current-mark-list)) (lambda (x) x))))

  (define (src)
    (printf "~v\n" (syntax-object->datum (mark-source (list-ref (current-mark-list) (current-frame-num))))))

  (define (binding sym)
    (lookup-binding-with-symbol (do-n-times cdr (current-frame-num) (current-mark-list)) sym))

  (define (do-n-times fn n arg)
    (foldl (lambda (x arg) (fn arg)) arg (build-list n (lambda (x) x)))))