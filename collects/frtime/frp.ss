; To do:
; generalize and improve notion of time
;  (e.g., combine seconds & milliseconds,
;  give user general "timer : number -> behavior (event?)"
;  'loose' timers that don't exactly measure real time
;   (e.g., during garbage-collection)
; completely restructure:
;  eliminate alarms, give processes timeouts
;  make special constructor behaviors (?)
;   (have tried and achieved unencouraging results)
;  separate behavior and event structures (?)
;    - could make event a substructure of behavior,
;      but this could be problematic for letrec
;    - better option seems to be explicit tag
; partial-order based evaluation:
;  - add a 'depth' field to behavior structure (DONE)
;  - make 'register' responsible for maintaining consistency
;  - 'switch' can result in cycle, in which case consistent
;    depths cannot be assigned
;  - should perhaps tag delay, integral nodes
; selective evaluation (?)
; replace #%app macro with #%top macro, define macros,
;   hash-table; make behaviors applicable as functions
;   (have tried and achieved unencouraging results)
; remove #%app, lambda, and define macros; lift all
;   primitives, redefine higher-order procedures
;   macro to automate definition of lifted primitives
;   make behaviors directly applicable
; consider adding placeholders again, this time as part
;   of the FRP system
;   (probably not necessary, since behaviors can serve
;    in this role)
; consider whether any other syntax should be translated
;   (e.g. 'begin')
; consider 'strict structure'
;
; Done:
; mutual dependencies between behaviors (sort of ...)
; fix delay bug
; allow delay to take a time behavior (sort of ...)
; events:
;   use behavior structs where value is tail of stream
;   interface with other libraries (e.g. graphics), other threads
;   hold : event * val -> behavior
;   changes : behavior -> event
;   map-e : event[a] * (a -> b) -> event[b]
;   merge-e : event[a1] * event[a2] * ... -> event[a1 U a2 U ...]
;   filter-e : event[a] * (a -> bool) -> event[a]
;   accum : event[a] * b * (a -> b -> b) -> event[b]
; modify graphics library to send messages for events
; behavior manager's priority queue should
;   - maintain weak boxes
;   - check stale flag before enqueuing for update
; delete dead weak references
; fix letrec-b macro to use switch
; - allow fn behaviors
; eliminate letrec-b, make appropriate letrec macro
;  ('undefined' value) (probably)
; rewrite lift
; - provide specialized lift for 0-3 (?) arguments
; fix subtle concurrency issue between
;  behavior creation outside manager thread
;  and activities of manager, particularly
;  involving registration/deregistration
;  (solution: send reg/unreg requests to manager if necessary)
; make separate library for graphics
;

(module frp mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "class.ss")
           (all-except (lib "mred.ss" "mred") send-event)
           (lib "string.ss")
           "erl.ss"
           (lib "match.ss")
           "treap.scm")
  
  ; also models events, where 'value' is all the events that
  ; haven't yet occurred (more specifically, a cons cell whose
  ; tail is *undefined*)
  (define-struct behavior (value dependents stale? thunk depth))
  
  (define (safe-behavior-depth v)
    (if (behavior? v)
        (behavior-depth v)
        0))
  
  (define proc->behavior
    (opt-lambda ([thunk void] [value (thunk)] . producers)
      (register
       (make-behavior
        value
        empty
        #f
        thunk
        ; temporary: this logic should move to 'register'
        (add1 (apply max 0 (map safe-behavior-depth
                                producers))))
       producers)))
  
  ; messages for behavior manager; we now ensure that
  ; only the manager manipulates the dependency graph
  (define-struct reg (inf sup ret))
  (define-struct unreg (inf sup))
  
  ; an external event; val is passed to recip's thunk
  (define-struct event (val recip))
  
  ; update the given behavior at the given time
  (define-struct alarm (time behavior))
  
  (define-values (struct:behavior-proc
                  make-behavior-proc
                  behavior-proc?
                  bp-ref
                  bp-set!)
    (make-struct-type 'behavior-proc #f 1 0 #f null #f 0))
  
  (define-syntax frp:lambda
    (syntax-rules ()
      [(_ formals expr ...) (make-behavior-proc
                             (lambda formals expr ...))]))
  
  (define-syntax frp:case-lambda
    (syntax-rules ()
      [(_ (formals expr ...) ...) (make-behavior-proc
                                   (case-lambda (formals expr ...) ...))]))
  
  (define-syntax frp:define
    (syntax-rules ()
      [(_ (fn . args) expr ...) (define fn
                                  (frp:lambda args expr ...))]
      [(_ var expr ...) (define var expr ...)]))
  ;                               (set-cell! var (begin expr ...)))]))
  
  (define (frp:if-helper test then-thunk else-thunk)
    (let ([if-fun (lambda (b) (if b
                                  (then-thunk)
                                  (else-thunk)))])
      (if (behavior? test)
          (switch
           (if-fun (get-value test))
           ((changes test) . ==> .
            if-fun))
          (if-fun test))))
  
  (define-syntax frp:if
    (syntax-rules ()
      [(_ test then) (frp:if-helper test
                                    (lambda () then)
                                    (void))]
      [(_ test then else) (frp:if-helper test
                                         (lambda () then)
                                         (lambda () else))]))
  
  (define-syntax frp:cond
    (syntax-rules (else =>)
      [(_ [else result1 result2 ...])
       (begin result1 result2 ...)]
      [(_ [test => result])
       (let ([temp test])
         (frp:if temp (result temp)))]
      [(_ [test => result] clause1 clause2 ...)
       (let ([temp test])
         (frp:if temp
                 (result temp)
                 (frp:cond clause1 clause2 ...)))]
      [(_ [test]) test]
      [(_ [test] clause1 clause2 ...)
       (let ((temp test))
         (frp:if temp
                 temp
                 (frp:cond clause1 clause2 ...)))]
      [(_ [test result1 result2 ...])
       (frp:if test (begin result1 result2 ...))]
      [(_ [test result1 result2 ...]
          clause1 clause2 ...)
       (frp:if test
               (begin result1 result2 ...)
               (frp:cond clause1 clause2 ...))]))
  
  (define-syntax frp:and
    (syntax-rules ()
      [(_) true]
      [(_ exp) exp]
      [(_ exp exps ...) (frp:if exp
                                (frp:and exps ...)
                                false)]))
  
  (define-syntax frp:or
    (syntax-rules ()
      [(_) false]
      [(_ exp) exp]
      [(_ exp exps ...) (let ([v exp])
                          (frp:if v
                                  v
                                  (frp:or exps ...)))]))
  
  ; get-value : behavior[a] -> a
  (define get-value
    (frp:lambda (val)
      (if (behavior? val)
          (behavior-value val)
          val)))
  
  ;   (define get-value/copy
  ;     (frp:lambda (val)
  ;       (match val
  ;         [($ behavior value _ _ _) (cond
  ;                                     [(cons? value)
  ;                                      (cons (first value) (rest value))]
  ;                                     [(posn? value)
  ;                                      (make-posn (posn-x value) (posn-y value))]
  ;                                     [else value])]
  ;         [_ val])))
  
  ; *** will have to change significantly to support depth-guided recomputation ***
  ; Basically, I'll have to check that I'm not introducing a cycle.
  ; If there is no cycle, then I simply ensure that inf's depth is at least one more than
  ; sup's.  If this requires an increase to inf's depth, then I need to propagate the
  ; new depth to inf's dependents.  Since there are no cycles, this step is guaranteed to
  ; terminate.  When checking for cycles, I should of course stop when I detect a pre-existing
  ; cycle.
  ; If there is a cycle, then 'inf' has (and retains) a lower depth than 'sup' (?), which
  ; indicates the cycle.  Importantly, 'propagate' uses the external message queue whenever
  ; a dependency crosses an inversion of depth.
  (define register
    (frp:lambda (inf sup)
      (if (eq? (self) man)
          (match sup
            [(and (? behavior?)
                  (= behavior-dependents dependents))
             (set-behavior-dependents!
              sup
              (cons (make-weak-box inf) dependents))]
            [(? list?) (for-each (lambda (sup1) (register inf sup1)) sup)]
            [_ (void)])
          (begin
            (! man (make-reg inf sup (self)))
            (receive [(? (lambda (v) (eq? v man))) (void)])))
      inf))
  
  (define (remove-by p l)
    (match l
      [() empty]
      [(a . d) (if (p a)
                   (remove-by p d)
                   (cons a (remove-by p d)))]))
  
  (define unregister
    (frp:lambda (inf sup)
      (if (eq? (self) man)
          (match sup
            [(and (? behavior?)
                  (= behavior-dependents dependents))
             (set-behavior-dependents!
              sup
              (remove-by (lambda (a)
                           (let ([v (weak-box-value a)])
                             (or (eq? v inf)
                                 (eq? v #f))))
                         dependents))]
            [_ (void)])
          (! man (make-unreg inf sup)))))
  
  (define-struct *undefined* ())
  (define undefined (make-*undefined*))
    ;(string->uninterned-symbol "*undefined*"))
  (define (undefined? x)
    (eq? x undefined))
  
  (define-syntax safe-eval
    (syntax-rules ()
      [(_ expr ...) (with-handlers ([exn?
                                     (lambda (exn)
                                       (cond
                                         [(and (exn:application:type? exn)
                                               (undefined? (exn:application-value exn)))]
                                         [else (thread (lambda () (raise exn)))])
                                       undefined)])
                      expr ...)]))
  
  ; could use special treatment for constructors
  ; to avoid making lots of garbage (?)
  (define create-thunk
    (frp:case-lambda
      [(fn) fn]
      [(fn arg1) (lambda () (fn (get-value arg1)))]
      [(fn arg1 arg2) (lambda () (fn (get-value arg1) (get-value arg2)))]
      [(fn arg1 arg2 arg3) (lambda () (fn (get-value arg1)
                                          (get-value arg2)
                                          (get-value arg3)))]
      [(fn . args) (lambda () (apply fn (map get-value args)))]))
  
  (define lift
    (frp:lambda (fn . args)
      (cond
        [(behavior? fn)
         (unregister #f fn) ; clear out stale dependencies from previous apps
         (let* ([cur-fn (get-value fn)]
                [cur-app (safe-eval (apply lift cur-fn args))])
           (rec ret (proc->behavior
                     (lambda ()
                       (when (not (eq? cur-fn (get-value fn)))
                         (unregister ret cur-app)
                         (set! cur-fn (get-value fn))
                         (set! cur-app (safe-eval (apply lift cur-fn args)))
                         (register ret cur-app))
                       (get-value cur-app))
                     (get-value cur-app)
                     ; deps
                     fn cur-app)))]
        [(and (not (behavior-proc? fn))
              (ormap behavior? args)
              (not (member fn behavior-consumers)))
         (let* ([thunk (apply create-thunk fn args)])
           (apply
            proc->behavior
            thunk
            (safe-eval (thunk))
            args))]
        [else (apply fn args)])))
  
  (define (last)
    (let ([prev #f])
      (lambda (v)
        (let ([ret (if prev prev v)])
          (set! prev v)
          ret))))
  
  (define (extract k evs)
    (if (cons? evs)
        (let ([ev (first evs)])
          (if (or (eq? ev undefined) (undefined? (rest ev)))
              (extract k (rest evs))
              (begin
                (let ([val (cadr ev)])
                  (set-first! evs (rest ev))
                  (k val)))))))
  
  ; until : behavior behavior -> behavior
  (frp:define (b1 . until . b2)
    (proc->behavior
     (lambda () (if (undefined? (get-value b2))
                    (get-value b1)
                    (get-value b2)))
     (get-value b1)
     ; deps
     b1 b2))
  
  (define (fix-streams streams args)
    (if (empty? streams)
        empty
        (cons
         (if (undefined? (first streams))
             (let ([stream (get-value (first args))])
               (if (undefined? stream)
                   stream
                   (if (equal? stream (cons undefined undefined))
                       stream
                       (cons undefined stream))))
             (first streams))
         (fix-streams (rest streams) (rest args)))))
  
  (define-syntax (event-filter stx)
    (syntax-case stx ()
      [(src-event-filter proc args)
       (with-syntax ([emit (datum->syntax-object (syntax src-event-filter) 'emit)]
                     [the-event (datum->syntax-object
                                 (syntax src-event-filter) 'the-event)])
         (syntax (let* ([out (cons undefined undefined)]
                        [emit (lambda (val)
                                (set-rest! out (cons val undefined))
                                (set! out (rest out)))]
                        [streams (map get-value args)]
                        [thunk (lambda ()
                                 (when (ormap undefined? streams)
                                   (printf "had an undefined stream~n")
                                   (set! streams (fix-streams streams args)))
                                 (let loop ()
                                   (extract (lambda (the-event) proc (loop))
                                            streams))
                                 out)])
                   (apply proc->behavior thunk out args))))]))
  
  (define-syntax (event-producer stx)
    (syntax-case stx ()
      [(src-event-producer expr ...)
       (with-syntax ([emit (datum->syntax-object (syntax src-event-producer) 'emit)]
                     [the-args (datum->syntax-object
                                (syntax src-event-producer) 'the-args)])
         (syntax (let* ([out (cons undefined undefined)]
                        [emit (lambda (val)
                                (set-rest! out (cons val undefined))
                                (set! out (rest out)))])
                   (proc->behavior (lambda the-args expr ... out) out))))]))
  
  ; switch : event[behavior] -> behavior
  (define switch
    (frp:lambda (init e)
      (let ([e-b (hold init e)])
        (rec ret (proc->behavior
                  (case-lambda
                    [()
                     (when (not (eq? init (get-value e-b)))
                       (unregister ret init)
                       (set! init (get-value e-b))
                       (register ret init))
                     (get-value init)]
                    [(msg) e])
                  (get-value init)
                  ; deps
                  e-b init)))))
  
  ; event* -> event
  (define merge-e
    (frp:lambda args
      (event-filter
       (emit the-event)
       args)))
  
  (define once-e
    (frp:lambda (e)
      (let ([b true])
        (event-filter
         (when b
           (set! b false)
           (emit the-event))
         (list e)))))
  
  ; behavior[a] -> event[a]
  (define changes
    (frp:lambda (b)
      (register
       (event-producer
        (emit (get-value b)))
       b)))
  
  (define (event-forwarder sym evt f+l)
    (event-filter
     (for-each (lambda (tid) (! tid (list 'remote-evt sym the-event))) (rest f+l))
     (list evt)))
  
  ; event-receiver : () -> event
  (define (event-receiver)
    (event-producer
     (when (not (empty? the-args))
       (emit (first the-args)))))
  
  ; when-e : behavior[bool] -> event
  (define when-e
    (frp:lambda (b)
      (let* ([last (get-value b)])
        (register
         (event-producer
          (let ([current (get-value b)])
            (when (and (not last) current)
              (emit current))
            (set! last current)))
         b))))
  
  ; ==> : event[a] (a -> b) -> event[b]
  (define ==>
    (frp:lambda (e f)
      (event-filter
       (emit ((get-value f) the-event))
       (list e))))
  
  ; -=> : event[a] b -> event[b]
  (define -=>
    (frp:lambda (e v)
      (e . ==> . (lambda (_) v))))
  
  ; =#> : event[a] (a -> bool) -> event[a]
  (define =#>
    (frp:lambda (e p)
      (event-filter
       (if (p the-event)
           (emit the-event))
       (list e))))
  
  (define nothing (string->uninterned-symbol "nothing"))
  
  ; =#=> : event[a] (a -> b U nothing) -> event[b]
  (define =#=>
    (frp:lambda (e f)
      (event-filter
       (let ([x (f the-event)])
         (if (not (eq? x nothing))
             (emit x)))
       (list e))))
  
  (define map-e ==>)
  (define map-const-e -=>)
  (define filter-e =#>)
  (define filter-map-e =#=>)
  
  ; event[a] b (a b -> b) -> event[b]
  (define collect-e
    (frp:lambda (e init trans)
      (event-filter
       (let ([ret (trans the-event init)])
         (set! init ret)
         (emit ret))
       (list e))))
  
  ; event[(a -> a)] a -> event[a]
  (define accum-e
    (frp:lambda (e init)
      (event-filter
       (let ([ret (the-event init)])
         (set! init ret)
         (emit ret))
       (list e))))
  
  ; event[a] behavior[b]* -> event[(list a b*)]
  (define snapshot-e
    (frp:lambda (e . bs)
      (event-filter
       (emit (cons the-event (map get-value bs)))
       (list e))))
  
  ; (a b* -> c) event[a] behavior[b]* -> event[c]
  (define snapshot-map-e
    (frp:lambda (fn ev . bs)
      (event-filter
       (emit (apply fn the-event (map get-value bs)))
       (list ev))))
  
  ; event[a] b (a b -> b) -> behavior[b]
  (define collect-b
    (frp:lambda (ev init trans)
      (hold init (collect-e ev init trans))))
  
  ; event[(a -> a)] a -> behavior[a]
  (define accum-b
    (frp:lambda (ev init)
      (hold init (accum-e ev init))))
  
  ; hold : a event[a] -> behavior[a]
  (define hold
    (frp:lambda (v e)
      (proc->behavior
       (lambda () (first (get-value e)))
       v
       e)))
  
  (define update
    (case-lambda
      [(b) (update0 b)]
      [(b a) (update1 b a)]))
  
  (define-values (iq-enqueue iq-dequeue iq-empty?)
    (let* ([treap (make-treap -)]
           [enq (treap 'enqueue)])
      (values
       (lambda (b)
         (enq (behavior-depth b) b))
       (lambda ()
         (treap 'dequeue))
       (lambda ()
         (treap 'empty?)))))
  
;  (define-values (iq-enqueue iq-dequeue iq-empty?)
;    (let* ([treap (make-treap -)]
;           [put (treap 'put!)]
;           [get (treap 'get)])
;      (values
;       (let ([cell (cons #f empty)])
;         (lambda (b)
;           (let ([depth (behavior-depth b)])
;             (put depth (cons b (rest (get depth (lambda () cell))))))))
;       (lambda ()
;         (let* ([depth&bhvrs (treap 'get-min)]
;                [bhvrs (rest depth&bhvrs)])
;           (if (empty? (rest bhvrs))
;               (treap 'delete-min!)
;               (put (first depth&bhvrs) (rest bhvrs)))
;           (first bhvrs)))
;       (lambda ()
;         (treap 'empty?)))))

  ; *** will have to change ... ***
  (define (propagate b)
    (let ([empty-boxes 0]
          [dependents (behavior-dependents b)]
          [depth (behavior-depth b)])
      (for-each
       (lambda (wb)
         (match (weak-box-value wb)
           [(and dep (? behavior?) (= behavior-stale? #f))
            (set-behavior-stale?! dep #t)
            ; If I'm crossing a "back" edge (one potentially causing a cycle),
            ; then I send a message.  Otherwise, I add to the internal
            ; priority queue.
            (if (< depth (behavior-depth dep))
                (iq-enqueue dep)
                (! man dep))]
           [_
            (set! empty-boxes (add1 empty-boxes))]))
       dependents)
      (when (> empty-boxes 9)
        (set-behavior-dependents!
         b
         (filter weak-box-value dependents)))))
  
  (define (update0 b)
    (match b
      [(and (? behavior?)
            (= behavior-value value)
            (= behavior-thunk thunk))
       (set-behavior-stale?! b #f)
       (let ([new-value (thunk)])
         ; consider modifying this test in order to support, e.g., mutable structs
         (when (not (equal? value new-value))
           (set-behavior-value! b new-value)
           (propagate b)))]
      [_ (void)]))
  
  (define (update1 b a)
    (match b
      [(and (? behavior?)
            (= behavior-value value)
            (= behavior-thunk thunk))
       (set-behavior-stale?! b #f)
       (let ([new-value (thunk a)])
         ; consider modifying this test in order to support, e.g., mutable structs
         (when (not (equal? value new-value))
           (set-behavior-value! b new-value)
           (propagate b)))]
      [_ (void)]))
  
  (define (undef b)
    (match b
      [(and (? behavior?)
            (= behavior-value value))
       (set-behavior-stale?! b #f)
       (when (not (eq? value undefined))
         (set-behavior-value! b undefined)
         (propagate b))]
      [_ (void)]))
  
  (define named-dependents (make-hash-table))
  
  (frp:define (bind sym evt)
    (! man (list 'bind sym evt))
    evt)
  
  (define (remote-reg tid sym)
    (hash-table-get named-dependents sym
                    (lambda ()
                      (let ([ret (event-receiver)])
                        (hash-table-put! named-dependents sym ret)
                        (! tid (list 'remote-reg man sym))
                        ret))))
  
  ; the manager of all behaviors and event streams
  (define man
    (spawn/name
     'frp-man
     (let ([alarm-q (make-treap -)]
           [named-providers (make-hash-table)]
           [cur-beh #f])
       (let outer ()
         (with-handlers ([exn?
                          (lambda (exn)
                            (cond
                              [(and (exn:application:type? exn)
                                    (undefined? (exn:application-value exn)))]
;                              [(exn:application:divide-by-zero? exn)]
                              [else ;(thread (lambda () (raise exn)))
                                    (fprintf (current-error-port) "~a~n" (exn-message exn))])
                            (undef cur-beh)
                            ;(sleep 2)
                            (outer))])
           (let loop ()
             ; should rewrite this entire block:
             ; (1) extract messages until none waiting (or N ms elapse?)
             ; (2) extract alarms
             ; (3) process internal queue until empty
             (if (iq-empty?)
                 ; no internal updates
                 (let ([timeout (if (alarm-q 'empty?)
                                    #f
                                    (- (first (alarm-q 'get-min))
                                       (current-milliseconds)))])
                   (receive
                    [after
                     timeout
                     (let inner ()
                       (when (not (alarm-q 'empty?))
                         (let ([next (alarm-q 'get-min)])
                           (when (>= (current-milliseconds) (first next))
                             (alarm-q 'delete-min!)
                             (for-each
                              (lambda (wb)
                                (let ([beh (weak-box-value wb)])
                                  (when (and beh (not (behavior-stale? beh)))
                                    (set-behavior-stale?! beh #t)
                                    (iq-enqueue beh))))
                              (rest next))
                             (inner)))))]
                    [(? behavior? b)
                     (set! cur-beh b)
                     (update b)
                     (set! cur-beh #f)]
                    [($ event val recip)
                     ; should this really be here?
                     (set! cur-beh recip)
                     (update recip val)
                     (set! cur-beh #f)]
                    [($ alarm ms beh)
                     (when (> ms 1073741824)
                       (set! ms (- ms 2147483647)))
                     (let* ([prev ((alarm-q 'get) ms (lambda () (cons ms empty)))]
                            [new (cons (make-weak-box beh) (rest prev))])
                       ((alarm-q 'put!) ms new))]
                    [($ reg inf sup ret)
                     (register inf sup)
                     (! ret man)]
                    [($ unreg inf sup) (unregister inf sup)]
                    [('bind sym evt)
                     (let ([forwarder+listeners (cons #f empty)])
                       (set-car! forwarder+listeners
                                 (event-forwarder sym evt forwarder+listeners))
                       (hash-table-put! named-providers sym forwarder+listeners))]
                    [('remote-reg tid sym)
                     (let ([f+l (hash-table-get named-providers sym)])
                       (when (not (member tid (rest f+l)))
                         (set-rest! f+l (cons tid (rest f+l)))))]
                    [('remote-evt sym val) ; should probably set cur-beh here too (?)
                     (update (hash-table-get named-dependents sym (lambda () dummy)) val)]
                    [x (fprintf (current-error-port) "msg not understood: ~a~n" x)]))
                 ; internal updates
                 (let ([b (iq-dequeue)])
                   (set! cur-beh b)
                   (update b)
                   (set! cur-beh #f)))
             (loop)))))))
  
  (define dummy
    (proc->behavior))
  
  (define (silly)
    (letrec ([res (proc->behavior
                   (let ([x 0]
                         [init (current-milliseconds)])
                     (lambda ()
                       (if (< x 400000)
                           (begin
                             (set! x (+ x 1)))
                           (begin
                             (printf "time = ~a~n" (- (current-milliseconds) init))
                             (set-behavior-dependents! res empty)))
                       x)))])
      (set-behavior-dependents! res (cons (make-weak-box res) empty))
      (! man res)
      res))
  
  (define (simple-b fn)
    (let ([ret (proc->behavior)])
      (set-behavior-thunk! ret (fn ret))
      (set-behavior-value! ret ((behavior-thunk ret)))
      ret))
  
  (define (make-time-b ms)
    (let ([ret (proc->behavior)])
      (set-behavior-thunk! ret
                           (lambda ()
                             (let ([t (current-milliseconds)])
                               (! man (make-alarm (+ ms t) ret))
                               t)))
      (set-behavior-value! ret ((behavior-thunk ret)))
      ret))
  
  (define milliseconds (make-time-b 20))
  (define time-b milliseconds)
  
  (define seconds
    (let ([ret (proc->behavior)])
      (set-behavior-thunk! ret
                           (lambda ()
                             (let ([s (current-seconds)]
                                   [t (current-milliseconds)])
                               (! man (make-alarm (* 1000 (add1 (floor (/ t 1000)))) ret))
                               s)))
      (set-behavior-value! ret ((behavior-thunk ret)))
      ret))
  
  ; general efficiency fix for delay
  ; behavior[a] behavior[num] -> behavior[a]
  (define delay-by
    (frp:lambda (beh ms-b)
      (if (and (number? ms-b) (<= ms-b 0))
          beh
          (let* ([last (cons (cons undefined
                                   (current-milliseconds))
                             empty)]
                 [head last]
                 [ret (proc->behavior)]
                 [thunk (lambda () (let* ([now (current-milliseconds)]
                                          [new (get-value beh)]
                                          [ms (get-value ms-b)])
                                     (when (not (equal? new (caar last)))
                                       (set-rest! last (cons (cons new now)
                                                             empty))
                                       (set! last (rest last))
                                       (! man (make-alarm
                                               (+ now ms) ret)))
                                     (let loop ()
                                       (if (or (empty? (rest head))
                                               (< now (+ ms (cdadr head))))
                                           (caar head)
                                           (begin
                                             (set! head (rest head))
                                             (loop))))))])
            (set-behavior-thunk! ret thunk)
            (set-behavior-value! ret (thunk))
            (register ret (list beh ms-b))))))
  
  ; fix to take arbitrary monotonically increasing number
  ; (instead of milliseconds)
  ; integral : behavior[num] behavior[num] -> behavior[num]
  (define integral
    (frp:case-lambda
      [(b) (integral b 20)]
      [(b ms-b) (let* ([accum 0]
                       [last-time (current-milliseconds)]
                       [last-val (get-value b)]
                       [ret (proc->behavior)]
                       [last-alarm 0]
                       [thunk (lambda ()
                                (let ([now (current-milliseconds)])
                                  (if (> now (+ last-time 10))
                                      (begin
                                        (when (not (number? last-val))
                                          (set! last-val 0))
                                        (set! accum (+ accum
                                                       (* last-val
                                                          (- now last-time))))
                                        (set! last-time now)
                                        (set! last-val (get-value b))
                                        (when (get-value ms-b)
                                          (! man (make-alarm
                                                  (+ last-time (get-value ms-b))
                                                  ret))))
                                      (when (or (>= now last-alarm)
                                                (and (< now 0)
                                                     (>= last-alarm 0)))
                                        (set! last-alarm (+ now 20))
                                        (! man (make-alarm last-alarm ret))))
                                  accum))])
                  (set-behavior-thunk! ret thunk)
                  (set-behavior-value! ret (thunk))
                  (register ret (list b ms-b)))]))
  
  ; fix for accuracy
  ; derivative : behavior[num] -> behavior[num]
  (define derivative
    (frp:lambda (b)
      (let* ([last-value (get-value b)]
             [last-time (current-milliseconds)]
             [thunk (lambda ()
                      (let* ([new-value (get-value b)]
                             [new-time (current-milliseconds)]
                             [result (if (or (eq? new-value last-value)
                                             (eq? new-time last-time)
                                             (> new-time
                                                (+ 500 last-time))
                                             (not (number? last-value))
                                             (not (number? new-value)))
                                         0
                                         (/ (- new-value last-value)
                                            (- new-time last-time)))])
                        (set! last-value new-value)
                        (set! last-time new-time)
                        result))])
        (proc->behavior thunk 0 b))))
  
  ; new-cell : a -> behavior[a] (cell)
  (frp:define (new-cell init)
    (switch init (event-receiver)))
  
  ; set-cell! : cell[a] a -> void
  (frp:define (set-cell! ref beh)
    (! man (make-event beh ((behavior-thunk ref) #t))))
  
  (frp:define (send-event rcvr evt)
    (! man (make-event evt rcvr)))
  
  (define c-v get-value)
  
  (define cur-vals
    (frp:lambda args
      (apply values (map get-value args))))
  
  (define behavior-consumers (list behavior?
                                   behavior-dependents
                                   behavior-depth
                                   make-unreg
                                   values))
  
  (define (curried-apply fn)
    (lambda (lis) (apply fn lis)))
  
  (define-syntax frp:app
    (syntax-rules ()
      [(_ fn arg ...) (lift fn arg ...)]))
  
  (define-syntax frp:letrec
    (syntax-rules ()
      [(_ ([id val] ...) expr ...)
       (let ([id (new-cell undefined)] ...)
         (set-cell! id val) ...
         expr ...)]))
  
  (define-syntax frp:rec
    (syntax-rules ()
      [(_ name value-expr)
       (frp:letrec ([name value-expr]) name)]))
  
  (define-syntax match-b
    (syntax-rules ()
      [(_ expr clause ...) (lift (match-lambda clause ...) expr)]))
  
  (define (geometric)
    (- (log (/ (random 2147483647) 2147483647.0))))
  
  (define (make-geometric mean)
    (simple-b (lambda (ret)
                (let ([cur 0])
                  (lambda ()
                    (! man (make-alarm (+ (current-milliseconds)
                                          (inexact->exact (ceiling (* mean (geometric)))))
                                       ret))
                    (set! cur (- 1 cur))
                    cur)))))
  
  (define (make-constant ms)
    (simple-b (lambda (ret)
                (let ([cur 0])
                  (lambda ()
                    (! man (make-alarm (+ (current-milliseconds) ms)
                                       ret))
                    (set! cur (- 1 cur))
                    cur)))))
  
  (define value-snip-copy%
    (class string-snip%
      (init-field current parent)
      (inherit get-admin)
      (define/public (set-current c)
        (set! current c)
        (let ([admin (get-admin)])
          (when admin
            (send admin needs-update this 0 0 1000 100))))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (send current draw dc x y left top right bottom dx dy draw-caret))
      (super-instantiate (" "))))
  
  (define value-snip%
    (class string-snip%
      (init-field bhvr)
      (field [copies empty]
             [loc-bhvr (proc->behavior (lambda () (update)) (void) bhvr)]
             [current (make-object string-snip% (expr->string (get-value bhvr)))])
      
      (rename [std-copy copy])
      (define/override (copy)
        (let ([ret (make-object value-snip-copy% current this)])
          (set! copies (cons ret copies))
          ret))
      
      (define/public (update)
        (set! current (make-object string-snip% (expr->string (get-value bhvr))))
        (for-each (lambda (copy) (send copy set-current current)) copies))
      
      (super-instantiate (" "))))
  
  (frp:define (watch beh)
    (if (behavior? beh)
        (if (and (cons? (get-value beh))
                 (undefined? (rest (get-value beh))))
            '<event>
            (make-object value-snip% beh))
        beh))
  
  (provide (rename #%app app-prim)
           (rename if if-prim)
           (rename cond cond-prim)
           (rename and and-prim)
           (rename or or-prim)
           (rename lambda lambda-prim)
           (rename case-lambda case-lambda-prim)
           (rename define define-prim)
           (rename letrec letrec-prim)
           (rename rec rec-prim)
           (rename frp:app #%app)
           (rename frp:if if)
           (rename frp:cond cond)
           (rename frp:and and)
           (rename frp:or or)
           (rename frp:lambda lambda)
           (rename frp:case-lambda case-lambda)
           (rename frp:define define)
           (rename frp:letrec letrec)
           (rename frp:rec rec)
           (rename match-b match)
           (rename get-value cur-val)
           start-network
           ; !
           make-tid
           (all-defined-except)
           (all-from-except (lib "list.ss"))
           (all-from-except (lib "etc.ss") rec)
           (all-from-except (lib "string.ss"))
           (all-from-except mzscheme define if #%app lambda case-lambda letrec and or cond)))
