(module mztext mzscheme

(require (lib "string.ss")
         (all-except (lib "pp-utils.ss" "preprocessor") stdin))
(provide (all-from (lib "pp-utils.ss" "preprocessor")))

;;=============================================================================
;; Composite port
;; A composite port is an input port and a procedure that can be used to
;; prepend stuff to this port.  The implementation uses a custom input port
;; that implements its own peeking, because peeking is not always consistent
;; (in case stuff was prepended, previous peeks are invalidated), so we cannot
;; rely on the internal default peek.  The port holds a list of input ports and
;; strings, which are being used as necessary when input is required.  This
;; list can also hold thunk values -- these thunks will be executed when
;; reading input goes past them (when peeking goes past them, nothing happens).

(define (copy-string! dst skip-k src . ks)
  (if (zero? (string-length src))
    0
    (apply peek-string-avail! dst skip-k (open-input-string src) ks)))

(define (make-composite-input . ports)
  ;; don't care about concurrency, since multiple uses should use different
  ;; input ports.
  (define (->string-or-port x)
    (if (or (string? x) (input-port? x)
            (and (procedure? x) (procedure-arity-includes? x 0)))
      x (and x (not (null? x)) (not (void? x)) (format "~a" x))))
  (define (add! . ps)
    (set! ports (append! (map ->string-or-port ps) ports)))
  (define (read! str)
    (let ([len (string-length str)])
      (let loop ([so-far 0])
        (cond [(null? ports) eof]
              [(not (car ports)) (set! ports (cdr ports)) (loop so-far)]
              [(procedure? (car ports)) ; reading past a thunk: execute it
               (set-car! ports (->string-or-port ((car ports))))
               (loop so-far)]
              [(<= len so-far) so-far]
              [(not (string? (car ports)))
               (let ([r (read-string-avail!* str (car ports) so-far len)])
                 (cond [(eof-object? r) (set! ports (cdr ports)) (loop so-far)]
                       ;; this probably doesn't happen
                       [(not (number? r)) (if (zero? so-far) r so-far)]
                       [(zero? (+ r so-far)) (car ports)]
                       [else (+ r so-far)]))]
              [(<= (- len so-far) (string-length (car ports)))
               (copy-string! str 0 (car ports) so-far len)
               (if (< (- len so-far) (string-length (car ports)))
                 (set-car! ports (substring (car ports) (- len so-far)))
                 (set! ports (cdr ports)))
               len]
              [else
               (loop (begin0 (+ (copy-string! str 0 (car ports) so-far len)
                                so-far)
                       (set! ports (cdr ports))))]))))
  ;; `peek!' could just call `read!' to fill in a string, then copy the
  ;; relevant stuff to the target buffer, then call `add!' to add the result
  ;; back to ports -- but that would lead to a lot of excessive copying.
  (define (peek! str skip)
    (let ([len (string-length str)])
      (let loop ([skip skip] [so-far 0] [ports ports])
        (cond [(null? ports) (if (zero? so-far) eof so-far)]
              [(or (not (car ports)) (procedure? (car ports))) ; skip #f+thunks
               (loop skip so-far (cdr ports))]
              [(and (zero? skip) (<= len so-far)) so-far]
              [(input-port? (car ports))
               (let* ([buf  (make-string (+ skip len))]
                      [blen (read-string-avail!* buf (car ports) 0)])
                 (cond [(eof-object? blen)
                        (set-car! ports #f) ; mark for removal
                        (loop skip so-far ports)]
                       [(or (eq? 0 blen) (not (number? blen)))
                        (if (> skip 0) (car ports) so-far)]
                       [else
                        (set-cdr! ports (cons (car ports) (cdr ports)))
                        (set-car! ports (if (= blen (+ skip len))
                                          buf (substring buf 0 blen)))
                        (loop skip so-far ports)]))]
              [(and (> skip 0) (<= (string-length (car ports)) skip))
               (loop (- skip (string-length (car ports))) so-far (cdr ports))]
              [else
               (let ([n (copy-string! str skip (car ports) so-far)])
                 (loop 0 (+ n so-far) (cdr ports)))]))))
  (define (close)
    (for-each (lambda (p) (when (input-port? p) (close-input-port p))) ports))
  (let ([p (make-custom-input-port read! peek! close)])
    (port->adder-op p 'set! add!)
    p))

(provide add-to-input)
(define (add-to-input . args)
  (apply port->adder-op (stdin) 'add args))

(define port->adder-op
  (let ([table (make-hash-table 'weak)])
    (lambda (port msg . args)
      (case msg
        [(add) (apply (hash-table-get table port
                        (lambda ()
                          (error 'add-to-input
                                 "current input is not a composite port")))
                      args)]
        [(set!) (apply hash-table-put! table port args)]
        [(get?) (hash-table-get table port (lambda () #f))]
        [else (error 'port->adder-op "unknown message: ~e" msg)]))))

;; Define stdin as a parameter that takes care of changing its argument to a
;; newly generated composite-input-port if needed, and also propagate changes
;; to current-input-port.
(provide stdin)
(define stdin
  (make-parameter (current-input-port)
    (lambda (newport)
      (let ([p (if (port->adder-op newport 'get?)
                 newport
                 (apply make-composite-input
                        (if (list? newport) newport (list newport))))])
        (current-input-port p)
        p))))

;;=============================================================================
;; Dispatching
;; A dispatcher is a pair of a regexp and a list of dispatch functions.  The
;; regexp should have some parenthesized subexpressions, and the one that
;; actually matched is used to select the dispatching functions, which is
;; invoked on the match.  This functionality is used for the main loop (with
;; the default single dispatcher for "@") and for `get-arg'.

(define (dispatch dispatcher continue failure . copy?)
  (let ([m (if (and (pair? copy?) (car copy?))
             (regexp-match (car dispatcher) (stdin) 0 #f (stdout))
             (regexp-match/fail-without-reading (car dispatcher) (stdin)))])
    (if m
      (ormap (lambda (x y) (and x (y x continue))) (cdr m) (cdr dispatcher))
      (failure))))

;; dispatchers is a list of (string dispatcher) lists
(define (make-dispatcher prefix dispatchers . regexps?)
  (define re
    (if (and (pair? regexps?) (car regexps?)) (lambda (x) x) regexp-quote))
  (cons (string-append
         prefix "(?:"
         (apply string-append
                (cdr (apply append
                            (map (lambda (d)
                                   (list "|" (format "(~a)" (re (car d)))))
                                 dispatchers))))
         ")")
        (map cadr dispatchers)))

;;=============================================================================
;; Dispatchers

(provide dispatchers)
(define dispatchers
  (make-parameter
   '() (lambda (ds) (rebuild-dispatcher-table ds (command-marker)) ds)))
(define dispatcher-table (make-parameter #f))

(provide command-marker)
(define command-marker
  (make-parameter
   #f (lambda (marker)
        (command-marker-here-re
         (regexp (string-append "^" (regexp-quote marker))))
        (rebuild-dispatcher-table (dispatchers) marker)
        marker)))
(define command-marker-here-re (make-parameter #f))

(define (rebuild-dispatcher-table dispatchers command-marker)
  (dispatcher-table
   (make-dispatcher
    "" (if command-marker
         `(,@dispatchers (,(regexp-quote command-marker) ,command-dispatcher))
         dispatchers)
    #t)))

(define (command-dispatcher match cont)
  (define (do-thunk thunk)
    (call-with-values thunk
      (lambda vs
        (define (value->cont v cont)
          (cond [(or (void? v) (not v) (null? v)) cont]
                [(pair? v) (value->cont (car v) (value->cont (cdr v) cont))]
                [(promise? v) (value->cont (force v) cont)]
                [(not (procedure? v))
                 (when (or (string? v) (symbol? v) (integer? v) (char? v)
                           (input-port? v))
                   (add-to-input v))
                 cont]
                [(procedure-arity-includes? v 0) (do-thunk v) cont]
                [(procedure-arity-includes? v 1) (lambda () (v cont))]
                [else (error 'mztext "got a bad procedure value: ~e" v)]))
        ((if (andmap (lambda (x) (or (not x) (void? x))) vs)
           (begin (swallow-newline) cont)
           (value->cont vs cont))))))
  (cond [(regexp-match/fail-without-reading (command-marker-here-re) (stdin))
         => (lambda (here) (display (car here)) (cont))]
        [else (do-thunk (lambda () (eval (read))))]))

(provide paren-pairs)
(define paren-pairs
  (make-parameter #f (lambda (pairs) (arg-dispatcher pairs) pairs)))

;; A list of an open regexp for any openning, and then a list of thunks, each
;; one for retreiving a piece of text by some paren pair.
(define arg-dispatcher
  (make-parameter
   #f (lambda (pairs)
        (make-dispatcher
         "^[ \t\r\n\f]*"
         (map (lambda (p) (list (car p) (apply make-arg-getter p))) pairs)))))

(define (make-arg-getter open close)
  (let ([re (regexp (if (equal? open close)
                      (begin (set! open close) (regexp-quote close))
                      (format "(~a)|(~a)"
                              (regexp-quote close) (regexp-quote open))))])
    (lambda (match cont)
      (let loop ([level 0] [pos 0])
        (let ([m (regexp-match-peek-positions re (stdin) pos)])
          (unless m (error 'get-arg "missing ~s" close))
          ;; (cadr m) => close, (caddr m) => open
          (cond [(or (eq? open close) (and (zero? level) (cadr m)))
                 (begin0 (read-string (caar m))
                   (regexp-match-positions re (stdin)))]
                [(caddr m) (loop (add1 level) (cdar m))]
                [(cadr m)  (loop (sub1 level) (cdar m))]
                [else (error 'get-arg "internal error")]))))))

(provide get-arg)
(define (get-arg)
  (dispatch
   (arg-dispatcher)
   #f
   (lambda ()
     (cond [(regexp-match/fail-without-reading #rx"[^ \t\r\n]" (stdin)) => car]
           [else (error 'get-arg "got no argument")]))))

(provide get-arg*)
(define (get-arg*)
  (let ([buf (open-output-string)])
    (parameterize ([stdout buf] [stdin (open-input-string (get-arg))])
      (run) (flush-output buf))
    (get-output-string buf)))

;;=============================================================================
;; User functionality

(provide swallow-newline)
(define (swallow-newline)
  ;; careful: if there's no match, we don't want to consume the input
  (regexp-match/fail-without-reading #rx"^[ \t]*\r?\n" (stdin))
  #f)

(define (string->substlist args str)
  (if (null? args)
    str
    (let* ([re (map (lambda (x) (regexp-quote (symbol->string x))) args)]
           [re (regexp (string-append
                        "(" (car re)
                        (apply string-append
                               (map (lambda (x) (string-append "|" x))
                                    (cdr re)))
                        ")"))]
           [posns (regexp-match-positions* re str)])
      (define (sub n . m) (apply substring str n m))
      (let loop ([pos 0] [posns posns] [r '()])
        (cond [(null? posns)
               (cons 'list (reverse! (if (= pos (string-length str))
                                       r (cons (sub pos) r))))]
              [(= pos (caar posns))
               (loop (cdar posns) (cdr posns)
                     (cons (string->symbol (sub (caar posns) (cdar posns)))
                           r))]
              [else (loop (caar posns) posns
                          (cons (sub pos (caar posns)) r))])))))

(provide defcommand)
(define (defcommand)
  (let ([name (string->symbol (get-arg))]
        [args (read-from-string-all (get-arg))]
        [body (get-arg)])
    (unless (and (list? args) (andmap symbol? args))
      (error 'defcommand "bad arguments for ~s: ~e" name args))
    (eval `(define (,name)
             (let ,(map (lambda (a) `[,a (,get-arg)]) args)
               ,(string->substlist args body))))))

;;=============================================================================
;; Invocation

(define (initialize)
  (read-case-sensitive #t)
  (unless (command-marker) (command-marker "@"))
  (unless (paren-pairs)
    (paren-pairs '(("(" ")") ("[" "]") ("{" "}") ("<" ">"))))
  (namespace-require '(lib "mztext.ss" "preprocessor"))
  (do-evals))

(define (run)
  (dispatch (dispatcher-table) run void #t))

(provide include)
(define (include . files)
  (define inputs (if (null? files) (list (get-arg)) files))
  (define curdir (cd))
  (define (cd+file f)
    (let*-values ([(dir name dir?)
                   (if (input-port? f) (values #f #f #f) (split-path f))]
                  [(dir) (if (string? dir) dir (cd))])
      ;; could `add-to-input' and then `run' if we wrap this by a (cd dir), but
      ;; instead, plant cd-thunks in the input stream.
      (add-to-input
       (lambda () (cd dir) (current-file (and (string? name) name)))
       (if (input-port? f) f (open-input-file f))
       (lambda () (cd curdir) (current-file #f)))))
  (swallow-newline) ; swallow *before* more stuff is added
  (for-each cd+file (reverse inputs))
  (run))

(provide preprocess)
(define (preprocess . files)
  (initialize)
  (unless (null? files)
    (parameterize ([stdin '()])
      (apply include files))))

)
