; Ideas:
;  make smart 'if'
;  tag impure and imperative signals (pure vs. stateful vs. effectful)
;
; To do:
; deal with multiple values (?)
; handle structs, vectors (done?)
; flip arguments in event-handling combinators (done)
; localized exception-handling mechanism
;
;
; generalize and improve notion of time
;  (e.g., combine seconds & milliseconds,
;  give user general "timer : number -> signal (event?)"
;  'loose' timers that don't exactly measure real time
;   (e.g., during garbage-collection)
; completely restructure:
;  eliminate alarms, give processes timeouts
;  make special constructor signals (?)
;   (have tried and achieved unencouraging results)
;  separate signal and event structures (?)
;    - could make event a substructure of signal,
;      but this could be problematic for letrec
;    - better option seems to be explicit tag
; partial-order based evaluation:
;  - add a 'depth' field to signal structure (DONE)
;  - make 'register' responsible for maintaining consistency
;  - 'switch' can result in cycle, in which case consistent
;    depths cannot be assigned
;  - should perhaps tag delay, integral nodes
; selective evaluation (?)
; consider adding placeholders again, this time as part
;   of the FRP system
;   (probably not necessary, since signals can serve
;    in this role)
; consider whether any other syntax should be translated
;   (e.g. 'begin')
; consider 'strict structure'
;
; Done:
; mutual dependencies between signals (sort of ...)
; fix delay bug
; allow delay to take a time signal (sort of ...)
; events:
;   use signal structs where value is tail of stream
;   interface with other libraries (e.g. graphics), other threads
;   hold : event * val -> signal
;   changes : signal -> event
;   map-e : event[a] * (a -> b) -> event[b]
;   merge-e : event[a1] * event[a2] * ... -> event[a1 U a2 U ...]
;   filter-e : event[a] * (a -> bool) -> event[a]
;   accum : event[a] * b * (a -> b -> b) -> event[b]
; modify graphics library to send messages for events
; signal manager's priority queue should
;   - maintain weak boxes
;   - check stale flag before enqueuing for update
; delete dead weak references
; fix letrec-b macro to use switch
; - allow fn signals
; eliminate letrec-b, make appropriate letrec macro
;  ('undefined' value) (probably)
; rewrite lift
; - provide specialized lift for 0-3 (?) arguments
; fix subtle concurrency issue between
;  signal creation outside manager thread
;  and activities of manager, particularly
;  involving registration/deregistration
;  (solution: send reg/unreg requests to manager if necessary)
; make separate library for graphics
;
; need to get rid of #%app macro, explicitly lift procedures, usually
; bottom-strictly
;
; remove #%app, lambda, and define macros; lift all
;   primitives, redefine higher-order procedures
;   macro to automate definition of lifted primitives
;   make signals directly applicable
;

(module frp mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "class.ss")
           (all-except (lib "mred.ss" "mred") send-event)
           (lib "string.ss")
           "erl.ss"
           (lib "match.ss")
           "heap.ss"
           "treap.scm")
  
  (require-for-syntax (lib "list.ss"))
  
  ; also models events, where 'value' is all the events that
  ; haven't yet occurred (more specifically, an event-cons cell whose
  ; tail is *undefined*)
  (define-values (make-signal
                  signal?
                  signal-value
                  signal-dependents
                  signal-stale?
                  signal-thunk
                  signal-depth
                  set-signal-value!
                  set-signal-dependents!
                  set-signal-stale?!
                  set-signal-thunk!
                  set-signal-depth!)
    (let-values ([(desc make-signal signal? acc mut)
                  (make-struct-type
                   'signal #f 5 0 #f null (make-inspector)
                   (lambda (fn . args)
                     (unregister #f fn) ; clear out stale dependencies from previous apps
                     (let* ([cur-fn (get-value fn)]
                            [cur-app (safe-eval (apply cur-fn args))]
                            [ret (proc->signal void fn cur-app)]
                            [thunk (lambda ()
                                     (when (not (eq? cur-fn (get-value fn)))
                                       (unregister ret cur-app)
                                       (set! cur-fn (get-value fn))
                                       (set! cur-app (safe-eval (apply cur-fn args)))
                                       (register ret cur-app))
                                     (get-value cur-app))])
                       (set-signal-thunk! ret thunk)
                       (set-signal-value! ret (thunk))
                       ret)))]
                 [(field-name-symbols) (list 'value 'dependents 'stale? 'thunk 'depth)]
                 [(0->4) (build-list 5 identity)])
      (apply values
             make-signal
             signal?
             (append (map (lambda (idx name) (make-struct-field-accessor acc idx name))
                          0->4 field-name-symbols)
                     (map (lambda (idx name) (make-struct-field-mutator mut idx name))
                          0->4 field-name-symbols)))))
  
  (define-struct event-cons (head tail))
  (define econs make-event-cons)
  (define efirst event-cons-head)
  (define erest event-cons-tail)
  (define econs? event-cons?)
  (define set-efirst! set-event-cons-head!)
  (define set-erest! set-event-cons-tail!)
  
  (define (event-source? v)
    (and (signal? v) (event-cons? (signal-value v))))
  
  (define (behavior? v)
    (and (signal? v) (not (event-cons? (signal-value v)))))
  
  (define (safe-signal-depth v)
    (if (signal? v)
        (signal-depth v)
        0))
  
  (define (proc->signal thunk . producers)
    (let ([sig (make-signal
                undefined empty #f thunk
                (add1 (apply max 0 (map safe-signal-depth
                                        producers))))])
      (when (cons? producers)
        (register sig producers))
      (set-signal-value! sig (safe-eval (thunk)))
      sig))
  
  ; messages for signal manager; we now ensure that
  ; only the manager manipulates the dependency graph
  (define-struct reg (inf sup ret))
  (define-struct unreg (inf sup))
  
  ; an external event; val is passed to recip's thunk
  (define-struct event (val recip))
  
  ; update the given signal at the given time
  (define-struct alarm (time signal))
  
  (define (frp:if-helper test then-thunk else-thunk)
    (let ([if-fun (lambda (b)
                    (cond
                      [(undefined? b) undefined]
                      [b (then-thunk)]
                      [else (else-thunk)]))])
      (if (behavior? test)
          (switch
           (if-fun (get-value test))
           ((changes test) . ==> .
                           if-fun))
          (if-fun test))))
  
  (define (weakly-cache thunk)
    (let ([cache (make-weak-box #f)])
      (lambda ()
        (cond
          [(weak-box-value cache) => identity]
          [else (let ([result (thunk)])
                  (set! cache (make-weak-box result))
                  result)]))))
  
  (define-syntax frp:if
    (syntax-rules ()
      [(_ test then)
       (frp:if-helper test (weakly-cache (lambda () then)) void)]
      [(_ test then else)
       (frp:if-helper test (weakly-cache (lambda () then)) (weakly-cache (lambda () else)))]))
  
  ; get-value : signal[a] -> a
  (define (get-value val)
    (if (behavior? val)
        (signal-value val)
        val))
  
  (define (get-value/copy val)
    (if (behavior? val)
        (let ([v1 (signal-value val)])
          (if (vector? v1)
              (build-vector (vector-length v1) (lambda (i) (vector-ref v1 i)))
              v1))
        val))
            
  
  ;   (define get-value/copy
  ;     (frp:lambda (val)
  ;       (match val
  ;         [($ signal value _ _ _) (cond
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
  (define (register inf sup)
    (if (eq? (self) man)
        (match sup
          [(and (? signal?)
                (= signal-dependents dependents))
           (set-signal-dependents!
            sup
            (cons (make-weak-box inf) dependents))]
          [(? list?) (for-each (lambda (sup1) (register inf sup1)) sup)]
          [_ (void)])
        (begin
          (! man (make-reg inf sup (self)))
          (receive [(? (lambda (v) (eq? v man))) (void)])))
    inf)
    
  (define (unregister inf sup)
    (if (eq? (self) man)
        (match sup
          [(and (? signal?)
                (= signal-dependents dependents))
           (set-signal-dependents!
            sup
            (filter (lambda (a)
                      (let ([v (weak-box-value a)])
                        (nor (eq? v inf)
                             (eq? v #f))))
                    dependents))]
          [_ (void)])
        (! man (make-unreg inf sup))))
  
  ;(define-struct *undefined* ())
  (define undefined ;(make-*undefined*))
    (string->uninterned-symbol "<undefined>"))
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
  (define create-strict-thunk
    (case-lambda
      [(fn) fn]
      [(fn arg1) (lambda () (if (undefined? arg1)
                                undefined
                                (fn (get-value arg1))))]
      [(fn arg1 arg2) (lambda () (if (or (undefined? arg1) (undefined? arg2))
                                     undefined
                                     (fn (get-value arg1) (get-value arg2))))]
      [(fn arg1 arg2 arg3) (lambda () (if (or (undefined? arg1) (undefined? arg2) (undefined? arg3))
                                          undefined
                                          (fn (get-value arg1)
                                              (get-value arg2)
                                              (get-value arg3))))]
      [(fn . args) (lambda () (if (ormap undefined? args)
                                  undefined
                                  (apply fn (map get-value args))))]))
  
  (define create-thunk
    (case-lambda
      [(fn) fn]
      [(fn arg1) (lambda () (fn (get-value arg1)))]
      [(fn arg1 arg2) (lambda () (fn (get-value arg1) (get-value arg2)))]
      [(fn arg1 arg2 arg3) (lambda () (fn (get-value arg1)
                                              (get-value arg2)
                                              (get-value arg3)))]
      [(fn . args) (lambda () (apply fn (map get-value args)))]))

  (define (lift-app strict? fn . args)
    (if (ormap behavior? args)
        (apply
         proc->signal
         (apply (if strict? create-strict-thunk create-thunk) fn args)
         args)
        (apply fn args)))
  
  (define (last)
    (let ([prev #f])
      (lambda (v)
        (let ([ret (if prev prev v)])
          (set! prev v)
          ret))))
  
  (define (extract k evs)
    (if (cons? evs)
        (let ([ev (first evs)])
          (if (or (eq? ev undefined) (undefined? (erest ev)))
              (extract k (rest evs))
              (begin
                (let ([val (efirst (erest ev))])
                  (set-first! evs (erest ev))
                  (k val)))))))
  
  ; until : behavior behavior -> behavior
  (define (b1 . until . b2)
    (proc->signal
     (lambda () (if (undefined? (get-value b2))
                    (get-value b1)
                    (get-value b2)))
     ; deps
     b1 b2))
  
  (define (fix-streams streams args)
    (if (empty? streams)
        empty
        (cons
         (if (undefined? (first streams))
             (let ([stream (signal-value (first args))])
               (if (undefined? stream)
                   stream
                   (if (equal? stream (econs undefined undefined))
                       stream
                       (econs undefined stream))))
             (first streams))
         (fix-streams (rest streams) (rest args)))))
  
  (define-syntax (event-filter stx)
    (syntax-case stx ()
      [(src-event-filter proc args)
       (with-syntax ([emit (datum->syntax-object (syntax src-event-filter) 'emit)]
                     [the-event (datum->syntax-object
                                 (syntax src-event-filter) 'the-event)])
         (syntax (let* ([out (econs undefined undefined)]
                        [emit (lambda (val)
                                (set-erest! out (econs val undefined))
                                (set! out (erest out)))]
                        [streams (map signal-value args)]
                        [thunk (lambda ()
                                 (when (ormap undefined? streams)
                                   (fprintf (current-error-port) "had an undefined stream~n")
                                   (set! streams (fix-streams streams args)))
                                 (let loop ()
                                   (extract (lambda (the-event) proc (loop))
                                            streams))
                                 out)])
                   (apply proc->signal thunk args))))]))
  
  (define-syntax (event-producer stx)
    (syntax-case stx ()
      [(src-event-producer expr dep ...)
       (with-syntax ([emit (datum->syntax-object (syntax src-event-producer) 'emit)]
                     [the-args (datum->syntax-object
                                (syntax src-event-producer) 'the-args)])
         (syntax (let* ([out (econs undefined undefined)]
                        [emit (lambda (val)
                                (set-erest! out (econs val undefined))
                                (set! out (erest out)))])
                   (proc->signal (lambda the-args expr out) dep ...))))]))
  
  ; switch : behavior event[behavior] -> behavior
  (define (switch init e)
    (let ([e-b (hold init e)])
      (rec ret
        (proc->signal
         (case-lambda
           [()
            (when (not (eq? init (get-value e-b)))
              (unregister ret init)
              (set! init (get-value e-b))
              (register ret init)
              (set-signal-depth! ret (max (signal-depth ret)
                                          (add1 (safe-signal-depth init)))))
            (get-value init)]
           [(msg) e])
         e-b init))))
  
  ; event* -> event
  (define (merge-e . args)
    (event-filter
     (emit the-event)
     args))
  
  (define (once-e e)
    (let ([b true])
      (event-filter
       (when b
         (set! b false)
         (emit the-event))
       (list e))))
  
  ; behavior[a] -> event[a]
  (define (changes b)
    (event-producer
     (emit (get-value b))
     b))
  
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
  (define (when-e b)
    (let* ([last (get-value b)])
      (event-producer
       (let ([current (get-value b)])
         (when (and (not last) current)
           (emit current))
         (set! last current))
       b)))
  
  ; ==> : event[a] (a -> b) -> event[b]
  (define (e . ==> . f)
    (event-filter
     (emit ((get-value f) the-event))
     (list e)))
  
  ; -=> : event[a] b -> event[b]
  (define-syntax -=>
    (syntax-rules ()
      [(_ e k-e) (==> e (lambda _ k-e))]))
  
  ; =#> : event[a] (a -> bool) -> event[a]
  (define (e . =#> . p)
    (event-filter
     (when (p the-event)
       (emit the-event))
     (list e)))
  
  (define nothing (string->uninterned-symbol "nothing"))
  
  ; =#=> : event[a] (a -> b U nothing) -> event[b]
  (define (e . =#=> . f)
    (event-filter
     (let ([x (f the-event)])
       (when (not (eq? x nothing))
         (emit x)))
     (list e)))
  
  (define (map-e f e)
    (==> e f))
  (define (filter-e p e)
    (=#> e p))
  (define (filter-map-e f e)
    (=#=> e f))
  
  ; event[a] b (a b -> b) -> event[b]
  (define (collect-e e init trans)
    (event-filter
     (let ([ret (trans the-event init)])
       (set! init ret)
       (emit ret))
     (list e)))
  
  ; event[(a -> a)] a -> event[a]
  (define (accum-e e init)
    (event-filter
     (let ([ret (the-event init)])
       (set! init ret)
       (emit ret))
     (list e)))
  
  ; event[a] signal[b]* -> event[(list a b*)]
  (define (snapshot-e e . bs)
    (event-filter
     (emit (cons the-event (map get-value bs)))
     (list e)))
  
  ; (a b* -> c) event[a] signal[b]* -> event[c]
  (define (snapshot-map-e fn ev . bs)
    (event-filter
     (emit (apply fn the-event (map get-value bs)))
     (list ev)))
  
  ; event[a] b (a b -> b) -> signal[b]
  (define (collect-b ev init trans)
    (hold init (collect-e ev init trans)))
  
  ; event[(a -> a)] a -> signal[a]
  (define (accum-b ev init)
    (hold init (accum-e ev init)))
  
  ; hold : a event[a] -> signal[a]
  (define (hold v e)
    (proc->signal
     (let ([b true])
       (lambda ()
         (if b
             (begin (set! b false) v)
             (efirst (signal-value e)))))
     e))
  
  (define update
    (case-lambda
      [(b) (update0 b)]
      [(b a) (update1 b a)]))
  
  (define-values (iq-enqueue iq-dequeue iq-empty?)
    (let ([heap (make-heap
                 (lambda (b1 b2)
                   (< (signal-depth b1)
                      (signal-depth b2)))
                 eq?)])
      (values
       (lambda (b) (heap-insert heap b))
       (lambda () (heap-pop heap))
       (lambda () (heap-empty? heap)))))
  
;  (define-values (iq-enqueue iq-dequeue iq-empty?)
;    (let* ([treap (make-treap -)]
;           [enq (treap 'enqueue)])
;      (values
;       (lambda (b)
;         (enq (signal-depth b) b))
;       (lambda ()
;         (treap 'dequeue))
;       (lambda ()
;         (treap 'empty?)))))
  
;  (define-values (iq-enqueue iq-dequeue iq-empty?)
;    (let* ([treap (make-treap -)]
;           [put (treap 'put!)]
;           [get (treap 'get)])
;      (values
;       (let ([cell (cons #f empty)])
;         (lambda (b)
;           (let ([depth (signal-depth b)])
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
          [dependents (signal-dependents b)]
          [depth (signal-depth b)])
      (for-each
       (lambda (wb)
         (match (weak-box-value wb)
           [(and dep (? signal?) (= signal-stale? #f))
            (set-signal-stale?! dep #t)
            ; If I'm crossing a "back" edge (one potentially causing a cycle),
            ; then I send a message.  Otherwise, I add to the internal
            ; priority queue.
            (if (< depth (signal-depth dep))
                (iq-enqueue dep)
                (! man dep))]
           [_
            (set! empty-boxes (add1 empty-boxes))]))
       dependents)
      (when (> empty-boxes 9)
        (set-signal-dependents!
         b
         (filter weak-box-value dependents)))))
  
  (define (update0 b)
    (match b
      [(and (? signal?)
            (= signal-value value)
            (= signal-thunk thunk))
       (set-signal-stale?! b #f)
       (let ([new-value (thunk)])
         ; consider modifying this test in order to support, e.g., mutable structs
         (when (not (equal? value new-value))
           (set-signal-value! b new-value)
           (propagate b)))]
      [_ (void)]))
  
  (define (update1 b a)
    (match b
      [(and (? signal?)
            (= signal-value value)
            (= signal-thunk thunk))
       (set-signal-stale?! b #f)
       (let ([new-value (thunk a)])
         ; consider modifying this test in order to support, e.g., mutable structs
         (when (not (equal? value new-value))
           (set-signal-value! b new-value)
           (propagate b)))]
      [_ (void)]))
  
  (define (undef b)
    (match b
      [(and (? signal?)
            (= signal-value value))
       (set-signal-stale?! b #f)
       (when (not (undefined? value))
         (set-signal-value! b undefined)
         (propagate b))]
      [_ (void)]))
  
  (define named-dependents (make-hash-table))
  
  (define (bind sym evt)
    (! man (list 'bind sym evt))
    evt)
  
  (define (remote-reg tid sym)
    (hash-table-get named-dependents sym
                    (lambda ()
                      (let ([ret (event-receiver)])
                        (hash-table-put! named-dependents sym ret)
                        (! tid (list 'remote-reg man sym))
                        ret))))
  
  ; the manager of all signals and event streams
  (define man
    (spawn/name
     'frp-man
     (let ([alarm-q (make-treap -)]
           [named-providers (make-hash-table)]
           [cur-beh #f])
       (let outer ()
         (with-handlers ([exn?
                          (lambda (exn)
                            (! (self) (make-event (cons exn cur-beh) exceptions))
                            (when (behavior? cur-beh)
                              (undef cur-beh))
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
                                  (when (and beh (not (signal-stale? beh)))
                                    (set-signal-stale?! beh #t)
                                    (iq-enqueue beh))))
                              (rest next))
                             (inner)))))]
                    [(? signal? b)
                     (set! cur-beh b)
                     (update0 b)
                     (set! cur-beh #f)]
                    [($ event val recip)
                     ; should this really be here?
                     (set! cur-beh recip)
                     (update1 recip val)
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
  
  (define exceptions
    (event-receiver))
  
  (define dummy
    (proc->signal void))
  
  (define (silly)
    (letrec ([res (proc->signal
                   (let ([x 0]
                         [init (current-milliseconds)])
                     (lambda ()
                       (if (< x 400000)
                           (begin
                             (set! x (+ x 1)))
                           (begin
                             (printf "time = ~a~n" (- (current-milliseconds) init))
                             (set-signal-dependents! res empty)))
                       x)))])
      (set-signal-dependents! res (cons (make-weak-box res) empty))
      (! man res)
      res))
  
  (define (simple-b fn)
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret (fn ret))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  (define (make-time-b ms)
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret
                           (lambda ()
                             (let ([t (current-milliseconds)])
                               (! man (make-alarm (+ ms t) ret))
                               t)))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  (define milliseconds (make-time-b 20))
  (define time-b milliseconds)
  
  (define seconds
    (let ([ret (proc->signal void)])
      (set-signal-thunk! ret
                           (lambda ()
                             (let ([s (current-seconds)]
                                   [t (current-milliseconds)])
                               (! man (make-alarm (* 1000 (add1 (floor (/ t 1000)))) ret))
                               s)))
      (set-signal-value! ret ((signal-thunk ret)))
      ret))
  
  ; general efficiency fix for delay
  ; signal[a] signal[num] -> signal[a]
  (define (delay-by beh ms-b)
    (if (and (number? ms-b) (<= ms-b 0))
        beh
        (let* ([last (cons (cons undefined
                                 (current-milliseconds))
                           empty)]
               [head last]
               [ret (proc->signal void)]
               [thunk (lambda () (let* ([now (current-milliseconds)]
                                        [new (get-value/copy beh)]
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
          (set-signal-thunk! ret thunk)
          (set-signal-value! ret (thunk))
          (register ret (list beh ms-b)))))
  
  ; fix to take arbitrary monotonically increasing number
  ; (instead of milliseconds)
  ; integral : signal[num] signal[num] -> signal[num]
  (define integral
    (case-lambda
      [(b) (integral b 20)]
      [(b ms-b) (let* ([accum 0]
                       [last-time (current-milliseconds)]
                       [last-val (get-value b)]
                       [ret (proc->signal void)]
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
                  (set-signal-thunk! ret thunk)
                  (set-signal-value! ret (thunk))
                  (register ret (list b ms-b)))]))
  
  ; fix for accuracy
  ; derivative : signal[num] -> signal[num]
  (define (derivative b)
    (let* ([last-value (get-value b)]
           [last-time (current-milliseconds)]
           [thunk (lambda ()
                    (let* ([new-value (get-value b)]
                           [new-time (current-milliseconds)]
                           [result (if (or (= new-value last-value)
                                           (= new-time last-time)
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
      (proc->signal thunk b)))
  
  ; new-cell : behavior[a] -> behavior[a] (cell)
  (define (new-cell init)
    (switch init (event-receiver)))
  
  ; set-cell! : cell[a] a -> void
  (define (set-cell! ref beh)
    (! man (make-event beh ((signal-thunk ref) #t))))
  
  (define (send-event rcvr evt)
    (! man (make-event evt rcvr)))
  
  (define c-v get-value)
  
  (define (cur-vals . args)
    (apply values (map get-value args)))
  
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
      [(_ expr clause ...) (lift-app #t (match-lambda clause ...) expr)]))
  
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
  
  (define drs-eventspace #f)
  
  (define (set-eventspace evspc)
    (set! drs-eventspace evspc))
  
  (define value-snip-copy%
    (class string-snip%
      (init-field current parent)
      (inherit get-admin)
      (define/public (set-current c)
        (parameterize ([current-eventspace drs-eventspace])
          (queue-callback
           (lambda ()
             (set! current c)
             (let ([admin (get-admin)])
               (when admin
                 (send admin needs-update this 0 0 1000 100)))))))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (send current draw dc x y left top right bottom dx dy draw-caret))
      (super-instantiate (" "))))
  
  (define (make-snip bhvr)
    (make-object string-snip%
      (let ([tmp (if (signal? bhvr)
                     (signal-value bhvr)
                     bhvr)])
        (cond
          [(econs? tmp) (format "#<event (last: ~a)>" (efirst tmp))]
          [(undefined? tmp) "<undefined>"]
          [else (expr->string tmp)]))))
  
  (define value-snip%
    (class string-snip%
      (init-field bhvr)
      (field [copies empty]
             [loc-bhvr (proc->signal (lambda () (update)) bhvr)]
             [current (make-snip bhvr)])
      
      (rename [std-copy copy])
      (define/override (copy)
        (let ([ret (make-object value-snip-copy% current this)])
          (set! copies (cons ret copies))
          ret))
      
      (define/public (update)
        (set! current (make-snip bhvr))
        (for-each (lambda (copy) (send copy set-current current)) copies))
      
      (super-instantiate (" "))))
  
  (define (watch beh)
    (cond
      [(undefined? beh)
       (make-object string-snip% "<undefined>")]
      [(signal? beh) (make-object value-snip% beh)]
      [else beh]))
  
  (define-syntax (lift-procs stx)
    (syntax-case stx ()
      [(src-lift-procs . ids)
       (with-syntax ([(old-name ...) (syntax ids)]
                     [(new-name ...) (generate-temporaries (syntax ids))])
         (syntax
          (begin
            (define-values (new-name ...)
              (values
               (lambda args (apply lift-app #f old-name args)) ...))
            (provide (rename new-name old-name) ...))))]))

  (define-syntax (frp:provide stx)
    (syntax-case stx ()
      [(_ . clauses)
       (foldl
        (lambda (c prev)
          (syntax-case prev ()
            [(begin clause ...)
             (syntax-case c (lifted lifted/nonstrict)
               [(lifted . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                        (apply lift-app true fun-name args))
                     ...
                     (provide (rename tmp-name fun-name) ...))))]
               [(lifted/nonstrict . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (define (tmp-name . args)
                        (apply lift-app false fun-name args))
                     ...
                     (provide (rename tmp-name fun-name) ...))))]
               [provide-spec
                (syntax (begin clause ... (provide provide-spec)))])]))
        (syntax (begin))
        (syntax->list (syntax clauses)))]))  

  (define-syntax (frp:require stx)
    (syntax-case stx ()
      [(_ . clauses)
       (foldl
        (lambda (c prev)
          (syntax-case prev ()
            [(begin clause ...)
             (syntax-case c (lifted lifted/nonstrict as-is)
               [(lifted/nonstrict module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (require (rename module tmp-name fun-name) ...)
                     (define (fun-name . args)
                        (apply lift-app false tmp-name args))
                     ...)))]
               [(lifted module . ids)
                (with-syntax ([(fun-name ...) (syntax ids)]
                              [(tmp-name ...)
                               (map (lambda (id)
                                      (datum->syntax-object stx (syntax-object->datum id)))
                                    (generate-temporaries (syntax ids)))])
                  (syntax
                   (begin
                     clause ...
                     (require (rename module tmp-name fun-name) ...)
                     (define (fun-name . args)
                        (apply lift-app true tmp-name args))
                     ...)))]
               [(as-is module id ...)
                (syntax (begin clause ... (require (rename module id id) ...)))]
               [require-spec
                (syntax (begin clause ... (require require-spec)))])]))
        (syntax (begin))
        (syntax->list (syntax clauses)))]))
  
  (lift-procs undefined?)
  
  (provide (rename frp:if if)
           (rename frp:require require)
           (rename frp:provide provide)
           (rename frp:letrec letrec)
           (rename match-b match)
           (rename get-value cur-val)
           (rename lift-app lift)
           module
           #%app
           #%top
           #%datum
           #%plain-module-begin
           #%module-begin
           lambda
           case-lambda
           define-values
           define
           let
           let-values
           let*
           begin
           begin0
           quote
           quasiquote
           unquote
           values
           (all-defined-except undefined?
                               lift-app
                               frp:if
                               frp:require
                               frp:provide
                               frp:letrec
                               frp:rec)))
