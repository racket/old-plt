(module hashcons mzscheme
  (require (prefix list: (lib "list.ss"))
           (lib "match.ss")
           (lib "pretty.ss")
           (lib "contract.ss")
           (lib "etc.ss")
           (prefix string: (lib "string.ss"))
           
           (prefix cst: "constants.ss")
           "labels.ss"
           "types.ss"
           "set-hash.ss")
  
  (provide
   ;; Create a new hashcons table
   make-hashcons-table
   ;; Convert a type to a handle
   hashcons-type
   ;; Get the type of a handle
   get-type
   ;; Get a pretty string from a handle
   handle->string
   
   ;; contract functions
   hashcons-table?
   handle?
   hashcons-type?
   
   ;; functions used in testing
   hashcons-table-size
   
   ;; used in cgp but not really necessary
   for-each-vov!
   for-each-vector!)
  
  (define-syntax ea-define/contract
    (syntax-rules ()
      ((_ id contract body)
       ;       (define/contract id contract body))))
       (define id body))))
  
  ;; Some useful predicates for contracts
  
  (define nonempty-list-of?
    (lambda (p) (lambda (xs) (and (pair? xs) (andmap p xs)))))
  
  (define natural? (lambda (n) (and (integer? n) (>= n 0))))
  (define handle? natural?)
  
  (define label-type? (union type-case-lambda? type-cons? type-promise?
                             type-struct-value? type-union? type-values? type-vector?))
  (define base-type? (union type-empty? type-cst? type-struct-type?))
  (define hashcons-type? (union label-type? base-type? type-rec?))
  
  ;;
  ;; Type environments
  ;;
  (define create-tenv (lambda () '()))
  
  (define tenv? (listof (cons/p (listof symbol?) (vectorof any?))))
  
  (ea-define/contract extend-tenv
                      (tenv? (listof symbol?) (listof handle?) . ->d .
                             (lambda (env vars handles)
                               (unless (= (length vars) (length handles))
                                 (error 'extend-tenv "Must have one handle for each var~n~a~n~a" vars handles))
                               tenv?))
                      (lambda (env vars handles)
                        (cons (cons vars (list->vector handles)) env)))
  
  (ea-define/contract generic-lookup-symbol
                      ((any? . -> . any?) . -> . (tenv? any? . -> . any?))
                      (lambda (not-found-function)
                        (lambda (tenv var)
                          (let loop-env ([env tenv])
                            (if (null? env)
                                (not-found-function var)
                                (let* ([rib (car env)]
                                       [syms (car rib)]
                                       [types (cdr rib)])
                                  (let loop-rib ([syms syms] [i 0])
                                    (cond
                                      [(null? syms) (loop-env (cdr env))]
                                      [(equal? (car syms) var) (vector-ref types i)]
                                      [else
                                       (loop-rib (cdr syms) (+ i 1))]))))))))
  
  (ea-define/contract lookup-symbol (tenv? symbol? . -> . any?)
                      (generic-lookup-symbol
                       (lambda (var)
                         (error 'get-state "Unknown type variable in environment: ~a " var))))
  
  (ea-define/contract maybe-lookup-symbol (tenv? symbol? . -> . (union false? handle?))
                      (generic-lookup-symbol (lambda (_) #f)))
  
  ;;
  ;; DFAs
  ;;
  (define-struct dfa-state (state) (make-inspector))
  
  ;; All base types i.e. type-cst, type-empty, struct-type-states are always handle states
  (define-struct (handle-state dfa-state) (handle) (make-inspector))
  (define-struct (cons-state dfa-state) (car cdr) (make-inspector))
  
  ; case-lambda-states has vectors for all fields, in contrast union-states and 
  ;   struct-value-states both use lists.
  (define-struct (case-lambda-state dfa-state)
                 (rest-arg?s   ; (vectorof boolean)
                  req-args     ; (vectorof natural)
                  argss        ; (vectorof (vectorof dfa-states))
                  exps)        ; (vectorof dfa-states)
                 (make-inspector))
  (define-struct (promise-state dfa-state) (value) (make-inspector))
  (define-struct (struct-value-state dfa-state) (label types) (make-inspector))
  (define-struct (union-state dfa-state) (elements) (make-inspector))
  (define-struct (values-state dfa-state) (type) (make-inspector))
  (define-struct (vector-state dfa-state) (element) (make-inspector))
  
  ;; -> (hashtable dfa-state-number dfa-state)
  (define make-dfa (lambda () (make-hash-table 'equal)))
  (define dfa? hash-table?)
  (define dfa-state-number? natural?)
  
  (ea-define/contract add-dfa-state! (dfa? dfa-state? . -> . any)
                      (lambda (dfa state)
                        (unless (hash-table-get dfa (dfa-state-state state) cst:thunk-false)
                          (hash-table-put! dfa (dfa-state-state state) state))))
  
  (ea-define/contract has-state-number? (dfa? dfa-state-number? . -> . boolean?)
                      (lambda (dfa state-number)
                        (if (hash-table-get dfa state-number cst:thunk-false) #t #f)))
  
  (ea-define/contract get-dfa-state-value (dfa? dfa-state-number? . -> . dfa-state?)
                      (lambda (dfa state-number)
                        (hash-table-get dfa state-number
                                        (lambda () (error 'get-dfa-state-value
                                                          "Expected state number ~a in DFA" state-number)))))
  
  (define get-number-dfa-states
    (lambda (dfa)
      (length (hash-table-map dfa (lambda (k v) k)))))
  
  ;;
  ;; DFA Tries - Allows for testing of a DFA being previously hashconsed
  ;;             in Theta(|DFA|) time. 
  ;;
  
  ; DFA states are analogous to letters and at a node we have map of handles
  ; indexed by the DFA representative (the handle of the last DFA state in a DFA
  ; canonically ordered by minimization).
  ;
  ; Two equivalent (minimal, strongly connected) DFAs will yield the same
  ; canonically ordered DFAs regardless of the start state picked.
  ;
  ; An association list and hash-table are used to store the maps, but perhaps
  ; there is a better choice of data structures.
  (define-struct trie (dfa-representative->handle dfa-state->trie))
  (set! make-trie
	(let ([old-make-trie make-trie])
	  (lambda ()
	    (old-make-trie '() (make-hash-table 'equal)))))
  
  ; Get the trie on the edge labeled by the DFA state
  (ea-define/contract get-trie-child (trie? dfa-state? . -> . (union trie? false?))
                      (lambda (trie letter)
                        (hash-table-get (trie-dfa-state->trie trie) letter cst:thunk-false)))
  
  ; Each DFA state added to the trie must map to a unique handle. 
  (ea-define/contract add-trie-state-handle!
                      (trie? handle? handle? . ->d .
                             (lambda (trie representative-handle state-handle)
                               (let ([dfa->handle (trie-dfa-representative->handle trie)])
                                 (when (assq representative-handle dfa->handle)
                                   (error 'add-trie-state-handle!
                                          "Mapping ~a to ~a, but trie already has mapping from DFA representative ~a to handle ~a"
                                          representative-handle state-handle
                                          representative-handle (cdr (assq representative-handle dfa->handle))))
                                 trie?)))
                      (lambda (trie representative-handle state-handle)
                        (let ([dfa->handle (trie-dfa-representative->handle trie)])
                          (set-trie-dfa-representative->handle! trie (cons (cons representative-handle state-handle) dfa->handle))
                          trie)))
  
  (ea-define/contract get-state-handle (trie? handle? . -> . handle?)
                      (lambda (trie representative-handle)
                        (let ([dfa-representative->handle (trie-dfa-representative->handle trie)])
                          (cdr (assq representative-handle dfa-representative->handle)))))
  
  (ea-define/contract get-handle-from-representative
                      (trie? . ->d .
                             (lambda (trie)
                               (let ([dfa->handle (trie-dfa-representative->handle trie)])
                                 (unless (length-one? dfa->handle)
                                   (error 'get-handle-from-representative
                                          "~a (!= 1) representatives present: ~a" (length dfa->handle) dfa->handle))
                                 (unless (= (caar dfa->handle) (cdar dfa->handle))
                                   (error 'get-handle-from-representative "Representative handle ~a not equal to representative handle ~a"
                                          (caar dfa->handle) (cdar dfa->handle)))
                                 handle?)))
                      (lambda (trie)
                        (caar (trie-dfa-representative->handle trie))))
  
  ; Return a handle of the DFAs start state if the DFA has already
  ; been hasconsed.  For each of the ordered DFA states we descend one
  ; level in the trie until we reach the last state (the
  ; representative).  As we are descending we note which of the tries
  ; contains the start state.  Getting the representative handle, we
  ; can lookup the handle of the start state in this noted trie.
  (ea-define/contract dfa-present?
                      (trie? (nonempty-list-of? dfa-state?) ; natural?
                             . -> . (union false? (listof handle?)))
                      (lambda (trie nstates) ;start-state-number)
                        (let/ec return-with
                          (let* ([rev-tries (list:foldl (lambda (state tries)
                                                          (let ([trie (get-trie-child (car tries) state)])
                                                            (if trie
                                                                (cons trie tries)
                                                                (return-with #f))))
                                                        (list trie)
                                                        nstates)]
                                 [rep-handle (get-handle-from-representative (car rev-tries))])
                            ;; get the handles for each state, in reverse order from the (reversed) list of tries
                            (list:foldr (lambda (trie states) (cons (get-state-handle trie rep-handle) states))
                                        '() (cdr (reverse rev-tries)))))))
  
  ; Add a list of DFA states and their corresponding handles to the trie
  (ea-define/contract add-dfa-states
                      (trie? (nonempty-list-of? dfa-state?) (listof handle?) . ->d .
                             (lambda (trie states handles)
                               (unless (= (length states) (length handles))
                                 (error 'add "length of list of types ~a != length of DFA handle list ~a"
                                        (length states) (length handles)))
                               (lambda (_)
                                 (let loop ([trie trie] [states states])
                                   (if (null? states)
                                       (begin
                                         (unless (empty-hash? (trie-dfa-state->trie trie))
                                           (error 'add-dfa-states "Representative node has a child node"))
                                         (unless (length-one? (trie-dfa-representative->handle trie))
                                           (error 'add-dfa-states "Representative node has more than one representative handle")))
                                       (loop (get-trie-child trie (car states)) (cdr states)))))))
                      (lambda (trie states handles)
                        (let ([add-child (lambda (trie letter representative-handle dfa-handle)
                                           (add-trie-state-handle!
                                            (if (get-trie-child trie letter) (get-trie-child trie letter)
                                                (let ([child-trie (make-trie)])
                                                  (hash-table-put! (trie-dfa-state->trie trie) letter child-trie)
                                                  child-trie))
                                            representative-handle dfa-handle))]
                              [representative-handle (list-ref handles (sub1 (length handles)))])
                          (let loop ([trie trie] [states states] [handles handles])
                            (unless (null? states)
                              (loop (add-child trie (car states) representative-handle (car handles))
                                    (cdr states)
                                    (cdr handles)))))))
  
  
  ;;
  ;; Hashcons tables
  ;;
  (define-struct hashcons-table
                 (from-handle     ;; handle -> (union dfa label base-type)
                  from-dfa        ;; dfa -> handle
                  from-label      ;; label -> handle
                  from-base-type  ;; base-type -> handle
		  dfa-trie        ;; type -> trie, handle -> handle
                  number-handles)
                 (make-inspector))
  
  (set! make-hashcons-table
	(let ([old-make-hashcons-table make-hashcons-table])
	  (lambda ()
	    (old-make-hashcons-table
	     (make-hash-table)
	     (make-hash-table 'equal) 
	     (make-hash-table 'equal) 
	     (make-hash-table 'equal)
	     (make-trie)
	     0))))
  
  (define list-of-handles? (lambda (xs) (and (list? xs) (andmap handle? xs))))
  
  (define get-next-handle
    (lambda (tbl)
      (let ([x (hashcons-table-number-handles tbl)])
        (set-hashcons-table-number-handles! tbl (+ x 1))
        x)))
  
  (define hashcons-table-size
    (lambda (tbl)
      (hashcons-table-number-handles tbl)))
  
  (define get-type-handle
    (lambda (tbl type)
      (hash-table-get
       (hashcons-table-from-base-type tbl) type
       (lambda ()
         (hash-table-get
          (hashcons-table-from-label tbl) type
          (lambda () (hash-table-get (hashcons-table-from-dfa tbl) type
                                     (lambda ()
				       (print-hashcons-table tbl)
				       (error 'get-type-handle "Type ~a not in hashcons table" type)))))))))
  
  (ea-define/contract get-type (hashcons-table? handle? . -> . hashcons-type?)
                      (lambda (tbl handle)
                        (hash-table-get (hashcons-table-from-handle tbl) handle
                                        (lambda () (error 'get-type "Handle: ~a not in hashcons table" handle)))))
  
  (define has-handle?
    (lambda (tbl handle)
      (hash-table-get (hashcons-table-from-handle tbl) handle cst:thunk-false)))
  
  (define has-base-type?
    (lambda (tbl base-type)
      (hash-table-get (hashcons-table-from-base-type tbl) base-type cst:thunk-false)))
  
  (define has-label-type?
    (lambda (tbl label-type)
      (hash-table-get (hashcons-table-from-label tbl) label-type cst:thunk-false)))
  
  (define has-dfa-type?
    (lambda (tbl dfa-type)
      (hash-table-get (hashcons-table-from-dfa tbl) dfa-type cst:thunk-false)))
  
  (define has-type?
    (lambda (tbl type)
      (or (has-base-type? tbl type) (has-label-type? tbl type) (has-dfa-type? tbl type))))
  
  (ea-define/contract add-base-type
                      (hashcons-table? base-type? . ->d .
                                       (lambda (tbl base-type)
                                         (when (has-base-type? tbl base-type)
                                           (error 'add-base-type "Already have hashconsed ~a" base-type))
                                         handle?))
                      (lambda (tbl base-type)
                        (let ([h (get-next-handle tbl)])
                          (hash-table-put! (hashcons-table-from-handle tbl) h base-type)
                          (hash-table-put! (hashcons-table-from-base-type tbl) base-type h)
                          h)))
  
  (ea-define/contract add-label-type
                      (hashcons-table? label-type? . ->d . 
                                       (lambda (tbl label-type)
                                         (when (has-label-type? tbl label-type)
                                           (error 'add-label-type "Label Type ~a already present in hashcons table" label-type))
                                         (when (has-dfa-type? tbl label-type)
                                           (error 'add-label-type "Label Type ~a is equivalent to DFA type" label-type))
                                         handle?))
                      (lambda (tbl label-type)
                        (let ([h (get-next-handle tbl)])
                          (hash-table-put! (hashcons-table-from-handle tbl) h label-type)
                          (hash-table-put! (hashcons-table-from-label tbl) label-type h)
                          h)))
  
  ;; add-dfa-type is slightly different from add-label-type and
  ;; and-base-type in that it needs to take its handle as an argument.
  ;; This is because we need to substitute all state numbers for
  ;; handle numbers in all states of the DFA prior to adding it to
  ;; them hashcons table.
  (ea-define/contract add-dfa-type
                      (hashcons-table? label-type? handle? . ->d . 
                                       (lambda (tbl dfa-type handle)
                                         (when (has-dfa-type? tbl dfa-type)
                                           (begin
                                             ; (print-hashcons-table tbl)
                                             (error 'add-dfa-type "DFA Type ~a already present in hashcons table" dfa-type)))
                                         handle?))
                      (lambda (tbl dfa-type handle)
                        (hash-table-put! (hashcons-table-from-handle tbl) handle dfa-type)
                        (hash-table-put! (hashcons-table-from-dfa tbl) dfa-type handle)
                        handle))
  
  (ea-define/contract recall-base-type (hashcons-table? type? . -> . handle?)
                      (lambda (tbl base-type)
                        (if (has-base-type? tbl base-type)
                            (hash-table-get (hashcons-table-from-base-type tbl) base-type)
                            (add-base-type tbl base-type))))
  
  (ea-define/contract recall-label-type (hashcons-table? label-type? . -> . handle?)
                      (lambda (tbl label-type)
                        (cond
                          [(has-dfa-type? tbl label-type) (hash-table-get (hashcons-table-from-dfa tbl) label-type)]
                          [(has-label-type? tbl label-type) (hash-table-get (hashcons-table-from-label tbl) label-type)]
                          [else (add-label-type tbl label-type)])))
  
  
  ; Hashcons-type is the main function.
  ; 
  ; Hashconsing proceedings in two main stages. We first hashcons as
  ; much as possible in a straight forward, bottom up fashion.  If
  ; there is no recursive types, then we are done and just return the
  ; handle. If there is a recursive type, then it is necessary to
  ; hashcons the recursive type in a bottom up fashion whenever a
  ; type has no free variables.
  (define/contract hashcons-type (hashcons-table? hashcons-type? . -> . handle?)
                   (lambda (tbl type)
                     ;      (with-handlers ([(lambda (_) #t)
                     ;                       (lambda (exn)
                     ;                         (pretty-print type)
                     ;                         (raise exn))])
                     (let ([type (hashcons-acyclic-subtrees tbl type)])
                       (if (handle? type) type
                           (bottom-up-hashcons tbl type)))))
  ;  )
  
  ; Hashcons all subtrees in a type containing no variables. Returns
  ; a type where all subtrees w/o variables are replaced by the
  ; corresponding handle. All label types which remain, have at least
  ; one children which contain a variable.
  ;
  ; After hashconsing all of the children of a type, if there is a
  ; child which has not been replaced by a handle then we have a
  ; recursive type and we do not hashcons the label. If the children
  ; are all handles, we hashcons this label and return its handle in
  ; place of the label.
  (define hashcons-acyclic-subtrees
    (lambda (tbl type)
      ((fold-type
        (lambda (handle) handle)                   ;; handle       :: handle -> b
        (lambda (rest-args req-args argss exps)    ;; case-lambda  :: [bool] [nat] [[b]] [b] -> b
          (let ([new-case-lambda (make-type-case-lambda
                                  (if (list? rest-args) (list->vector rest-args) rest-args)
                                  (if (list? req-args) (list->vector req-args) req-args)
                                  argss exps)])
            (if (and (vector-of-vector-of? handle? argss) (vector-of? handle? exps))
                (recall-label-type tbl new-case-lambda)
                new-case-lambda)))
        (lambda (hd tl)                            ;; cons         :: b b -> b
          (let ([new-type-cons (make-type-cons hd tl)])
            (if (and (handle? hd) (handle? tl))
                (recall-label-type tbl new-type-cons)
                new-type-cons)))
        (lambda (ty)                               ;; cst          :: any? -> b
          (recall-base-type tbl (make-type-cst ty)))
        (let ((empty (make-type-empty)))           ;; empty        :: -> b
          (lambda ()
            (recall-base-type tbl empty)))
        (lambda (type)                             ;; promise      :: b -> b
          (let ([new-type-promise (make-type-promise type)])
            (if (handle? type)
                (recall-label-type tbl new-type-promise)
                new-type-promise)))
        (lambda (vars types body)                  ;; rec          :: [b] [b] b -> b
          (let* ([new-type-rec (make-type-rec vars types body)])
            (if (and (list-of-handles? types) (handle? body))
                body
                new-type-rec)))
        (lambda (label)                            ;; struct-type  :: label -> b
          (recall-base-type tbl (make-type-struct-type label)))
        (lambda (label types)                      ;; label        :: [b] -> b
          (let ([new-type (make-type-struct-value label types)])
            (if (list-of-handles? types)
                (recall-label-type tbl new-type)
                new-type)))
        (lambda (elements)                         ;; union        :: [b] -> b
	  (cond [(null? elements) (recall-base-type tbl (make-type-empty))]
		[(length-one? elements) (car elements)]
		[(list-of-handles? elements)
		 (let* ([elements (min-list-numbers elements)])
		   (cond
                     [(length-one? elements) (car elements)]
                     [else
                      (recall-label-type tbl (make-type-union elements))]))]
		[else (make-type-union elements)]))
        (lambda (type)                             ;; values       :: b -> b
          (let ([new-type-values (make-type-values type)])
            (if (handle? type)
                (recall-label-type tbl new-type-values)
                new-type-values)))
        (lambda (name recur)                       ;; var          :: name boolean -> b
          (make-type-var name recur))
        (lambda (type)                             ;; vector       :: b -> b
          (let ([new-type-vector (make-type-vector type)])
            (if (handle? type)
                (recall-label-type tbl new-type-vector)
                new-type-vector))))
       type)))
  
  ;; After we've hashconsed a recursive type this does the final job
  ;; of adding it to the hashcons table.
  (define hashcons-rec-type-body
    (lambda (tbl type)
      (let ([recall-type (lambda (type) (if ; (or (has-base-type? tbl type) (has-label-type? tbl type))
                                         (has-type? tbl type)
                                         (get-type-handle tbl type)
                                         (recall-label-type tbl type)))])
        ((fold-type
          (lambda (handle) handle)                   ;; handle       :: handle -> b
          (lambda (rest-args req-args argss exps)    ;; case-lambda  :: [bool] [bool] [[b]] [b] -> b
            (recall-type (make-type-case-lambda rest-args req-args argss exps)))
          (lambda (hd tl)                            ;; cons         :: b b -> b
            (recall-type (make-type-cons hd tl)))
          (lambda (ty)                               ;; cst          :: any? -> b
            (error 'hashcons-rec-type-body "type-cst should have been previously hashconsed"))
          (lambda ()                                 ;; empty        :: -> b
            (error 'hashcons-rec-type-body "type-empty should have been previously hashconsed"))
          (lambda (type)                             ;; promise      :: b -> b
            (recall-type (make-type-promise type)))
          (lambda (vars types body)                  ;; rec          :: [b] [b] b -> b
            (error 'hashcons-rec-type-body "Should not have a type-rec DFA at this point"))
          (lambda (label)                            ;; struct-type
            (error 'hashcons-rec-type-body "struct-type should have been hashcons already"))
          (lambda (label types)                      ;; struct-value
            (recall-type (make-type-struct-value label types)))
          (lambda (elements)                         ;; type-union [b] -> b
	    (let ([elements (min-list-numbers elements)])
	      (cond [(length-one? elements) (car elements)]
		    [else
		     (recall-type (make-type-union elements))])))
          (lambda (type)                             ;; type-values       ;; b -> b
            (recall-type (make-type-values type)))
          (lambda (name recur)                       ;; type-var          ;; b -> b
            (error 'hashcons-rec-type-body "Should not have type-var at this point"))
          (lambda (type)                             ;; type-vector       ;; b -> b
            (recall-type (make-type-vector type))))
         type))))
  
  ;; Almost the same as acyclic hashcons except when we reach a
  ;; rec-type with no free variables we hashcons it.
  ;; 
  ;; Perhaps this could be merged w/ acyclic hashcons. The code is
  ;; almost identical
  (ea-define/contract bottom-up-hashcons
                      (hashcons-table? hashcons-type? . ->d . (lambda (tbl type) handle?))
                      (lambda (tbl type)
                        (let
                            ([hashcons
                              (fold-type
                               (lambda (handle) handle)                 ;; handle       :: handle -> b
                               (lambda (rest-args req-args argss exps)  ;; case-lambda  :: [bool] [bool] [[b]] [b] -> b
                                 (let ([new-case-lambda (make-type-case-lambda rest-args req-args argss exps)])
                                   (if (and (vector-of-vector-of? handle? argss)
                                            (vector-of? handle? exps))
                                       (recall-label-type tbl new-case-lambda)
                                       new-case-lambda)))
                               (lambda (hd tl)                          ;; cons         :: b b -> b
                                 (let ([new-type-cons (make-type-cons hd tl)])
                                   (if (and (handle? hd) (handle? tl))
                                       (recall-label-type tbl new-type-cons)
                                       new-type-cons)))
                               (lambda (type)                           ;; cst          :: any? -> b
                                 (recall-base-type tbl type))
                               (lambda ()                               ;; empty        :: -> b
                                 (recall-base-type tbl (make-type-empty)))
                               (lambda (value)                          ;; promise      :: b -> b
                                 (let ((new-type-promise (make-type-promise value)))
                                   (if (handle? value)
                                       (recall-label-type tbl new-type-promise)
                                       new-type-promise)))
                               (lambda (vars types body)                ;; rec          :: [b] [b] b -> b
                                 (cond
                                   [(not (has-free-vars? (make-type-rec vars types body)))
                                    (let* ([vars (if (type-var? body) vars (cons (make-type-var (gensym) #f) vars))]
                                           [types (if (type-var? body) types (cons body types))]
                                           [body (if (type-var? body) body (car vars))]
                                           [graph (let ([g (make-hash-table)])
                                                    (for-each (lambda (var type)
                                                                (hash-table-put! g (type-var-name var) (get-referenced-vars type)))
                                                              vars types)
                                                    g)]
                                           [bindings (let ([h (make-hash-table)])
                                                       (for-each (lambda (var type)
                                                                   (hash-table-put! h (type-var-name var) type)) vars types)
                                                       h)]
                                           [sccs (strongly-connected-components graph)]
                                           [_ (printf "sccs=~a~n" sccs)]
                                           [env
                                            (list:foldl (lambda (scc env)
                                                          (printf "scc=~a~n" scc)
                                                          (cond
                                                            ;; The SCC is actually a recursive type
                                                            [(and (length-one? scc) (memq (car scc) (hash-table-get graph (car scc))))
                                                             (let*-values ([(ty) (make-type-rec (list (make-type-var (car scc) #f))
                                                                                                (list (hash-table-get bindings (car scc)))
                                                                                                (make-type-var (car scc) #f))]
                                                                           [(ty) (subst-handles/vars-if-possible ty env)]
                                                                           [(ty) (hashcons-acyclic-subtrees tbl ty)]
                                                                           [(dfa binder-states)
                                                                            (create-dfa-from-type ty env)]
                                                                           [(_) (printf "rec-type original-dfa=")]
                                                                           [(_) (print-dfa dfa)]
                                                                           [(min-dfa min-binder-states)
                                                                            (minimize-dfa dfa binder-states)]
                                                                           [(_) (printf "rec-type min-dfa=~a binder-states=~a~n" min-dfa binder-states)]
                                                                           [(all-handles)
                                                                            (recall-entire-dfa tbl min-dfa)]
                                                                           [(_) (printf "all-handles=~a~n" all-handles)]
                                                                           [(binder-handles)
                                                                            (let/ec return-with
                                                                              (let loop ([all-states min-dfa]
                                                                                         [all-handles all-handles])
                                                                                (when (or (null? min-dfa) (null? all-handles))
                                                                                  (error 'one-rec-no-binder-found))
                                                                                (unless (= 1 (length min-binder-states))
                                                                                  (error 'more-than-one-binder-in-one))
                                                                                (if (= (dfa-state-state (car all-states)) (car min-binder-states))
                                                                                    (list (car all-handles))
                                                                                    (loop (cdr all-states) (cdr all-handles)))))])
                                                               (extend-tenv env scc binder-handles))]
                                                            [(length-one? scc)
                                                             (printf "going to hashcons-rec-type-body~n")
                                                             (extend-tenv env scc
                                                                          (list (hashcons-rec-type-body tbl
                                                                                                        (subst-handles/vars (hash-table-get bindings (car scc)) env))))]
                                                            [else
                                                             (printf "else branch~n")
                                                             (let*-values ([(ty) (make-type-rec (map (lambda (v) (make-type-var v #f)) scc)
                                                                                                (map (lambda (v) (hash-table-get bindings v)) scc)
                                                                                                (make-type-var (car scc) #f))]
                                                                           [(ty) (hashcons-acyclic-subtrees tbl ty)]
                                                                           [(dfa binder-states)
                                                                            (create-dfa-from-type ty env)]
                                                                           [(_) (printf "original-dfa=")]
                                                                           [(_) (print-dfa dfa)]
                                                                           [(min-dfa min-binder-states)
                                                                            (minimize-dfa dfa binder-states)]
                                                                           [(_) (printf "min-dfa=~a~n" min-dfa)]
                                                                           [(handles) (recall-entire-dfa tbl min-dfa)]
                                                                           [(_) (printf "all-handles=~a~n" handles)]
                                                                           [(binder-handles)
                                                                            ;; O(n^2) ;; <-- stupid
                                                                            (letrec ([position
                                                                                      (lambda (state-num xs counter)
                                                                                        (cond [(null? xs) (error 'not-found)]
                                                                                              [(= (dfa-state-state (car xs)) state-num) counter]
                                                                                              [else (position state-num (cdr xs) (add1 counter))]))])
                                                                              (map (lambda (pos) (list-ref handles pos))
                                                                                   (map (lambda (state) (position state min-dfa 0)) min-binder-states)))])
                                                               (extend-tenv env scc binder-handles))]))
                                                        (create-tenv)
                                                        sccs)])
                                      (printf "looking up ~a in " (type-var-name body)) (pretty-print env)
                                      (lookup-symbol env (type-var-name body)))]))
                               (lambda (label)                            ;; type-struct-type 
                                 (recall-base-type tbl (make-type-struct-type label)))
                               (lambda (label types)                      ;; type-struct-value
                                 (let ([new-type (make-type-struct-value label types)])
                                   (if (list-of-handles? types)
                                       (recall-label-type tbl new-type)
                                       new-type)))
                               (lambda (elements)           ;; type-union [b] -> b
                                 (if (list-of-handles? elements)
                                     (let* ([elements (min-list-numbers elements)])
                                       (cond
                                         [(null? elements) (recall-base-type tbl (make-type-empty))]
                                         [(length-one? elements) (car elements)]
                                         [else
                                          (recall-label-type tbl (make-type-union elements))]))
                                     (make-type-union elements)))
                               (lambda (type)               ;; type-values       ;; b -> b
                                 (let ((new-type-values (make-type-values type)))
                                   (if (handle? type)
                                       (recall-label-type tbl new-type-values)
                                       new-type-values)))
                               (lambda (name recur)
                                 (make-type-var name recur))
                               (lambda (element)            ;; type-vector      ;; b -> b
                                 (let ((new-type-vector (make-type-vector element)))
                                   (if (handle? element)
                                       (recall-label-type tbl new-type-vector)
                                       new-type-vector))))])
                          (hashcons type))))
  
  ;; When this function is called all of the label types in present
  ;; belong to a strongly connected graph. 1) The all label types are
  ;; annotated with a state number. Variables are not given a state
  ;; number. 2) The graph is traversed again w/ a type
  ;; environment. When a rec-types is encountered the variable/state
  ;; bindings are added to the type environment. When a variable is
  ;; encountered its state is returned. Labeled states are created w/
  ;; the states of their children and added to the DFA.
  ;;
  ;; type -> (values dfa start-state)
  (ea-define/contract create-dfa-from-type
                      ((type-rec? tenv?) . ->d* .
                                         (lambda (type tenv)
                                           (unless (type-var? (type-rec-body type))
                                             (error 'create-dfa-from-type
                                                    "type-rec should have type-var for body"))
                                           (for-each (lambda (type)
                                                       (when (type-var? type)
                                                         (error 'create-dfa-from-type "DFA has variable on right side of binder")))
                                                     (type-rec-types type))
                                           (values dfa? (listof dfa-state-number?))))
                      (lambda (type tenv)
                        (let* ([dfa (make-dfa)]
                               [annotations (make-hash-table 'equal)]
                               [next-dfa-state
                                (let ([*i* 0])
                                  (lambda () (let ([x *i*]) (set! *i* (+ 1 *i*)) x)))])
                          (letrec ([annotate
                                    (lambda (type)
                                      (unless (hash-table-get annotations type cst:thunk-false)
                                        (match type
                                          [(? handle? h)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [($ type-cons hd tl)
                                           (annotate hd)
                                           (annotate tl)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [($ type-case-lambda rest-arg?s req-args argss exps)
                                           (for-each-vov (lambda (ty) (annotate ty)) argss)
                                           (for-each-vector (lambda (ty) (annotate ty)) exps)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [($ type-promise value)
                                           (annotate value)
                                           (hash-table-put! annotations type (next-dfa-state))]    
                                          [($ type-rec vars types body)
                                           (for-each (lambda (var ty)
                                                       (cond [(type-var? ty)
                                                              (error 'create-dfa-from-type
                                                                     (string-append "Types bound by type-rec must have bindings"
                                                                                    " wrapped by a type constructor: ~a" ty))]
                                                             [(maybe-lookup-symbol tenv (type-var-name var))]
                                                             [else (annotate ty)]))
                                                     vars types)
                                           (annotate body)]
                                          [($ type-struct-value type-label types)
                                           (for-each annotate types)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [($ type-union elements)
                                           (for-each annotate elements)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [($ type-var name reach recur)
                                           (let ([handle (maybe-lookup-symbol tenv name)])
                                             (if handle
                                                 (hash-table-put! annotations type (next-dfa-state))
                                                 cst:void))]
                                          [($ type-values ty)
                                           (annotate ty)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [($ type-vector element)
                                           (annotate element)
                                           (hash-table-put! annotations type (next-dfa-state))]
                                          [else
                                           (error 'create-dfa-from-type "Type ~a should already have been hashconsed" type)])))]
                                   [create-dfa
                                    (lambda (type env)
                                      (match type
                                        [(? handle? h)
                                         (let ([state-number
                                                (hash-table-get annotations type
                                                                (lambda ()
                                                                  (error "Couldn't find handle ~a in annotation table" h)))])
                                           (add-dfa-state! dfa (make-handle-state state-number h))
                                           state-number)]
                                        [($ type-cons hd tl)
                                         (let* ([hd (create-dfa hd env)]
                                                [tl (create-dfa tl env)]
                                                [state-number (hash-table-get annotations type assert-never)]
                                                [state (make-cons-state state-number hd tl)])
                                           (add-dfa-state! dfa state)
                                           state-number)]
                                        [($ type-case-lambda rest-arg?s req-args argss exps)
                                         (let* ([argss (map-vector-of-vector (lambda (type) (create-dfa type env)) argss)]
                                                [exps (map-vector (lambda (type) (create-dfa type env)) exps)]
                                                [state-number (hash-table-get annotations type assert-never)]
                                                [state (make-case-lambda-state state-number
                                                                               rest-arg?s
                                                                               req-args
                                                                               argss exps)])
                                           (add-dfa-state! dfa state)
                                           state-number)]
                                        [($ type-promise value)
                                         (let* ([value (create-dfa value env)]
                                                [state-number (hash-table-get annotations type)])
                                           (add-dfa-state! dfa (make-promise-state state-number value))
                                           state-number)]
                                        [($ type-rec vars types body)
                                         (let* ([binder-states (map (lambda (v ty)
                                                                      (let ([ann (hash-table-get annotations v cst:thunk-false)])
                                                                        (if ann ann
                                                                            (hash-table-get annotations ty))))
                                                                    vars types)]
                                                [new-env (extend-tenv env (map type-var-name vars) binder-states)])
                                           (for-each (lambda (var type)
                                                       (if (hash-table-get annotations var cst:thunk-false)
                                                           (create-dfa var new-env)
                                                           (create-dfa type new-env)))
                                                     vars types)
                                           binder-states)]
                                        [($ type-struct-value type-label types)
                                         (let ([types (map (lambda (ty) (create-dfa ty env)) types)]
                                               [state-number (hash-table-get annotations type assert-never)])
                                           (add-dfa-state! dfa (make-struct-value-state state-number type-label types))
                                           state-number)]
                                        [($ type-union elements)
                                         (let* ([elements (map (lambda (ty) (create-dfa ty env)) elements)]
                                                [state-number (hash-table-get annotations type assert-never)])
                                           (add-dfa-state! dfa (make-union-state state-number
                                                                                 (min-list-numbers elements)))
                                           state-number)]
                                        [($ type-vector element)
                                         (let ([element (create-dfa element env)]
                                               [state-number (hash-table-get annotations type assert-never)])
                                           (add-dfa-state! dfa (make-vector-state state-number element))
                                           state-number)]
                                        [($ type-var name reach recur)
                                         (let ([state-number (hash-table-get annotations type cst:thunk-false)])
                                           (if state-number
                                               (begin
                                                 (add-dfa-state! dfa (make-handle-state state-number (lookup-symbol tenv name)))
                                                 state-number)
                                               (lookup-symbol env name)))]
                                        [($ type-values ty)
                                         (let ([ty (create-dfa ty env)]
                                               [state-number
                                                (hash-table-get annotations type
                                                                (lambda () (error 'create-dfa-from-type "Type ~a not annotated" type)))])
                                           (add-dfa-state! dfa (make-values-state state-number ty))
                                           state-number)]
                                        [else
                                         (error 'create-dfa-from-type "Type should already have been hashconsed")]
                                        ))])
                            (annotate type)
                            (let ([binder-state-numbers (create-dfa type (create-tenv))])
                              (values dfa binder-state-numbers))))))
  
  
  ;;
  ;; DFA Minimization
  ;;
  
  ;; A non-empty list of dfa-states, representing an equivalence class
  (define block? (listof dfa-state?))
  
  ;; A list of disjoint blocks
  (define partition? (listof block?))
  
  ;; A function extracting some value from a dfa-state. Discriminators are used
  ;; when comparing two DFAs states
  (define discriminator? (dfa-state? . -> . (union integer? boolean?)))
  
  (define block->partition list)
  
  ;; To split a block of dfa-states, use the value projected from a
  ;; dfa-state as a hashtable-key which is associated with the list of
  ;; dfa-states with identical values
  (ea-define/contract split-set (discriminator? block? . -> . partition?)
                      (lambda (f xs)
                        (if (length-one? xs) (block->partition xs)
                            (let ([accs (make-hash-table)])
                              (for-each (lambda (x) (hash-table-prepend! accs (f x) x)) xs)
                              (let ([keys (hash-table-map accs (lambda (k v) k))])
                                (if (null? keys) '()
                                    (let* ([gt (cond [(boolean? (car keys))
                                                      (lambda (a b) (cond [(eq? a b) #f] [a #f] [b #t]))]
                                                     [(integer? (car keys)) >]
                                                     [else (error 'split-set "Unknown type ~a" (car keys))])]
                                           [keys (list:mergesort keys gt)])
                                      (map (lambda (k) (hash-table-get accs k)) keys))))))))
  
  ;; list list list -> list list
  (define unnest
    (lambda (xsss)   ;; there is probably a better way of doing this, but its not a big time hit
      (let loop ([xsss xsss] [acc '()])
        (if (null? xsss) acc (loop (cdr xsss) (append acc (car xsss)))))))
  
  ; Split each a partition by a block splitter
  (ea-define/contract split-partition-by
                      ((block? . -> . partition?) partition? . -> . partition?)
                      (lambda (partition-block partition)
                        (unnest (map (lambda (block) (partition-block block)) partition))))
  
  ; Split each a partition by a discriminator
  (ea-define/contract split-partition (discriminator? partition? . -> . partition?)
                      (lambda (f xss)
                        (unnest (map (lambda (xs) (split-set f xs)) xss))))
  
  ; Split a block by the values  
  (ea-define/contract split-by-vector-values
                      ((dfa-state? . -> . vector?) (integer? . -> . discriminator?) . -> . (block? . -> . partition?))
                      (lambda (list-accessor discriminator)
                        (lambda (block)
                          (list:foldr (lambda (i xss) (split-partition (discriminator i) xss))
                                      (block->partition block)                          ; initial partition
                                      (iota (vector-length (list-accessor (car block))))))))   ; split for each member in the list
  
  (ea-define/contract split-by-vector-vector-values
                      ((dfa-state? . -> . (vectorof vector?))
                       (dfa-state? . -> . vector?)
                       (integer? integer? . -> . discriminator?)
                       . -> . (block? . -> . partition?))
                      (lambda (vector-accessor vector-vector-accessor discriminator)
                        (lambda (type-list)
                          (list:foldr (lambda (row acc)
                                        (list:foldr (lambda (col acc2)
                                                      (split-partition (discriminator row col) acc2))
                                                    acc
                                                    (iota (vector-length (vector-vector-accessor (car type-list))))))
                                      (list type-list)
                                      (iota (vector-length (vector-accessor (car type-list))))))))
  
  ;; Split a block of unions into a paritions with each union in a
  ;; block has the same number of elements.
  (ea-define/contract split-union-states ((listof union-state?) . -> . (listof (listof union-state?)))
                      (lambda (unions)
                        (let* ([number-elements-discriminator
                                (lambda (union) (length (union-state-elements union)))])
                          (split-set number-elements-discriminator unions))))
  
  (ea-define/contract split-struct-value-states ((listof struct-value-state?) . -> . (listof (listof struct-value-state?)))
                      (lambda (structs)
                        (let ([number-elements-discriminator
                               (lambda (struct) (length (struct-value-state-types struct)))])
                          ; a total ordering must be imposed on labels
                          (split-partition (lambda (sv) (eq-hash-code (struct-value-state-label sv)))
                                           (split-set number-elements-discriminator structs)))))
  
  ;; Split a block of case-lambda-states into a partition with each
  ;; block having the same number of expressions, each parameter list
  ;; having the same length, each rest and req arg lists have the same
  ;; length and values.
  (ea-define/contract split-case-lambda-states
                      ((listof case-lambda-state?) . -> . (listof (listof case-lambda-state?)))
                      (lambda (cls)
                        (letrec
                            ([get-number-args
                              (lambda (i) (lambda (cl) (vector-length (vector-ref (case-lambda-state-argss cl) i))))]
                             [get-number-exps
                              (lambda (cl) (vector-length (case-lambda-state-exps cl)))]
                             [get-rest-arg
                              (lambda (i) (lambda (cl) (vector-ref (case-lambda-state-rest-arg?s cl) i)))]
                             [get-req-arg
                              (lambda (i) (lambda (cl) (vector-ref (case-lambda-state-req-args cl) i)))]
                             [req-arg-gt (lambda (xs ys)
                                           (cond [(and (null? xs) (null? ys)) #f]
                                                 [(= (car xs) (car ys)) (req-arg-gt (cdr xs) (cdr ys))]
                                                 [(> (car xs) (car ys)) #t]
                                                 [(< (car xs) (car ys)) #f]
                                                 [else (error 'lex "Differing lengths")]))]
                             [rest-arg-gt (lambda (xs ys)
                                            (cond [(and (null? xs) (null? ys)) #f]
                                                  [(= (car xs) (car ys)) (rest-arg-gt (cdr xs) (cdr ys))]
                                                  [(car xs) #t]
                                                  [else #f]))])
                          (split-partition-by
                           (split-by-vector-values case-lambda-state-req-args get-req-arg)
                           (split-partition-by
                            (split-by-vector-values case-lambda-state-rest-arg?s get-rest-arg)
                            (split-partition-by
                             (split-by-vector-values         ; block -> partition
                              case-lambda-state-argss        ; (any? . -> . list?)  ;  case-lambda -> args
                              get-number-args)               ; (integer? . -> . (any? . -> . any?))
                             (split-set get-number-exps cls)))) )))
  
  ;; If this DFA has been hashconsed return its handle, otherwise add it to the
  ;; hashcons table and the trie.
  (ea-define/contract recall-entire-dfa
                      (hashcons-table? (listof dfa-state?) . -> . (listof handle?))
                      (lambda (hashcons-table dfa)
                        (let* ([trie (hashcons-table-dfa-trie hashcons-table)]
                               [maybe-handles (dfa-present? trie dfa)]) ; start-state-index)])
                          (if maybe-handles maybe-handles 
                              (let* ([new-handles (map (lambda (state) (if (handle-state? state)
                                                                           (handle-state-handle state)
                                                                           (get-next-handle hashcons-table)))
                                                       dfa)]
                                     [state->handle (make-immutable-hash-table (map cons (map dfa-state-state dfa) new-handles))]
                                     [lookup (lambda (dfa-state)
                                               (hash-table-get state->handle dfa-state
                                                               (lambda () (error 'lookup
                                                                                 "Expected handle for DFA state number ~a" dfa-state))))]
                                     [subst-handle/state
                                      (match-lambda
                                          [($ cons-state state hd tl)
                                           (make-type-cons (lookup hd) (lookup tl))]
                                        [($ case-lambda-state state rest-arg?s req-args argss exps)
                                         (make-type-case-lambda rest-arg?s
                                                                req-args
                                                                (map-vector-of-vector lookup argss)
                                                                (map-vector lookup exps))]
                                        [($ union-state state elements)
                                         (make-type-union (min-list-numbers (map lookup elements)))]  ;; double check this 
                                        [($ promise-state state value)
                                         (make-type-promise (lookup value))]
                                        [($ struct-value-state state label types)
                                         (make-type-struct-value label (map lookup types))]
                                        [($ values-state state types)
                                         (make-type-values (lookup types))]
                                        [($ vector-state state element)
                                         (make-type-vector (lookup element))]
                                        [x (error 'recall-entire-dfa "Unmatched type ~a" x)])])
                                (add-dfa-states trie dfa new-handles) ; trie w/ handle states
                                (for-each (lambda (dfa-state handle)  ; hashcons-table w/o handle states
                                            (unless (handle-state? dfa-state)
                                              (add-dfa-type hashcons-table (subst-handle/state dfa-state) handle)))
                                          dfa new-handles)
                                new-handles)))))
  
  ;; Almost a fold, with the exception of the type-rec-type binding variables which we do not
  ;; recurse on. 
  (define fold-type
    (lambda (handlef       ;; handle -> b
             case-lambdaf  ;; [bool] [int] [[b]] [b] -> b
             consf         ;; b b -> b
             cstf          ;; any? -> b
             emptyf        ;; -> b
             promisef      ;; b -> b
             recf          ;; [b] [b] b -> b
             struct-typef  ;; label -> b
             struct-valuef ;; label [b] -> b
             unionf        ;; [b] -> b
             valuesf       ;; b -> b
             varf          ;; name bool -> b
             vectorf)      ;; b -> b
      (lambda (type)
        (letrec ([foldt (fold-type handlef case-lambdaf consf cstf emptyf
                                   promisef recf struct-typef struct-valuef
                                   unionf valuesf varf vectorf)])
          (match type
            [(? handle? type)
             (handlef type)]
            [($ type-case-lambda rest-arg?s req-args argss exps)
             ;; When we first get a case-lambda its arguments may be lists,
             ;; so convert them to vectors once and for all here.  This is
             ;; a hack until case-lambda uses vectors in all cases.
             (let* ([argss (if (list? argss) (lol->vov argss) argss)]
                    [exps (if (list? exps) (list->vector exps) exps)]
                    [argss (for-each-vov! foldt argss)]
                    [exps (for-each-vector! foldt exps)])
               (case-lambdaf  rest-arg?s req-args argss exps))]
            [($ type-cons hd tl)
             (consf (foldt hd) (foldt tl))]
            [($ type-cst ty)
             (cstf ty)]
            [($ type-empty)
             (emptyf)]
            [($ type-promise value)
             (promisef (foldt value))]
            [($ type-rec vars types body)
             (let (;(vars (map foldt vars))  ; <-- Do not recur on variables being bound
                   (types (map foldt types)))
               (recf vars types (foldt body)))]
            [($ type-struct-type label)
             (struct-typef label)]
            [($ type-struct-value label types)
             (struct-valuef label (map foldt types))]
            [($ type-union elements)
             (unionf (map foldt elements))]
            [($ type-values type)
             (valuesf (foldt type))]
            [($ type-var name reach recur)
             (varf name recur)]
            [($ type-vector element)
             (vectorf (foldt element))]
            [_ (error 'fold-type "Unmatched type ~a" type)])))))
  
  ;; Return a type with handles replacing variables
  (ea-define/contract subst-handles/vars ((union label-type? handle? type-var?) tenv? . -> . (union type? handle?))
                      (lambda (type tenv)
                        (let subst ([type type])
                          (match type
                            [(? handle? type) type]
                            [($ type-case-lambda rest-arg?s req-args argss exps)
                             (let* ([argss (for-each-vov! subst argss)]
                                    [exps (for-each-vector! subst exps)])
                               (make-type-case-lambda rest-arg?s req-args argss exps))]
                            [($ type-cons hd tl)
                             (make-type-cons (subst hd) (subst tl))]
                            [($ type-promise value)
                             (make-type-promise (subst value))]
                            [($ type-rec vars handle-list body)
                             (for-each (lambda (handle) (unless (handle? handle)
                                                          (pretty-print (make-type-rec vars handle-list body))
                                                          (error 'type-rec-var-no-handle)))
                                       handle-list)
                             (subst-handles/vars body
                                                 (extend-tenv tenv (map type-var-name vars) handle-list))]
                            [($ type-struct-value label types)
                             (make-type-struct-value label (map subst types))]
                            [($ type-union elements)
                             (make-type-union (map subst elements))]
                            [($ type-values type)
                             (make-type-values (subst type))]
                            [($ type-var name reach recur)
                             (lookup-symbol tenv name)]
                            [($ type-vector element)
                             (make-type-vector (subst element))]
                            [_ (error 'subst-handles/vars "Unmatched type ~a" type)]))))
  
  (ea-define/contract subst-handles/vars-if-possible
                      ((union hashcons-type? handle? type-var?) tenv? . -> . (union type? handle?))
                      (lambda (type tenv)
                        (let subst ([type type])
                          (match type
                            [(? handle? type) type]
                            [($ type-case-lambda rest-arg?s req-args argss exps)
                             (let* ([argss (for-each-vov! subst argss)]
                                    [exps (for-each-vector! subst exps)])
                               (make-type-case-lambda rest-arg?s req-args argss exps))]
                            [($ type-cons hd tl)
                             (make-type-cons (subst hd) (subst tl))]
                            [($ type-promise value)
                             (make-type-promise (subst value))]
                            [($ type-rec vars types body)
                             ;; maybe we should add the vars/types to the scope iff the type is a handle
                             (make-type-rec vars (map subst types) (subst body))]
                            [($ type-struct-value label types)
                             (make-type-struct-value label (map subst types))]
                            [($ type-union elements)
                             (make-type-union (map subst elements))]
                            [($ type-values type)
                             (make-type-values (subst type))]
                            [($ type-var name reach recur)
                             (let ([h (maybe-lookup-symbol tenv name)])
                               (if h h type))]
                            [($ type-vector element)
                             (make-type-vector (subst element))]))))
  
  (ea-define/contract has-free-vars? ((union type? handle?) . -> . boolean?)
                      (lambda (type)
                        (let* ([bound-vars (make-hash-table)]
                               [bind (lambda (var)
                                       (let ([cv (hash-table-get bound-vars var cst:thunk-false)])
                                         (hash-table-put! bound-vars var (if cv (add1 cv) 1))))]
                               [unbind (lambda (var)
                                         (let ([cv (hash-table-get bound-vars var
                                                                   (lambda () (error 'unbind "Cannot unbind unbound variable ~a" var)))])
                                           (when (= cv 0)
                                             (error 'unbind "Cannot unbind variable ~a more times than its bound" var))
                                           (hash-table-put! bound-vars var (sub1 cv))))]
                               [bound? (lambda (var)
                                         (let ([cv (hash-table-get bound-vars var cst:thunk-false)])
                                           (and cv (> cv 0))))])
                          (let/ec k
                            (letrec
                                ([list-has-free-vars? (lambda (args) (ormap has-free-vars? args))]
                                 [has-free-vars?
                                  (match-lambda
                                      [(? handle? type) #f]
                                    [($ type-case-lambda rest-arg?s req-args argss exps)
                                     (or (vector-of-vector-has? has-free-vars? argss)
                                         (vector-has? has-free-vars? exps))]
                                    [($ type-cons hd tl)
                                     (or (has-free-vars? hd) (has-free-vars? tl))]
                                    [($ type-promise value)
                                     (has-free-vars? value)]
                                    [($ type-rec vars types body)
                                     (let* ([vnames (map type-var-name vars)]
                                            [_ (for-each bind vnames)]
                                            [fv (or (list-has-free-vars? types) (has-free-vars? body))])
                                       (for-each unbind vnames)
                                       fv)]
                                    [($ type-struct-value label types)
                                     (list-has-free-vars? types)]
                                    [($ type-union elements)
                                     (list-has-free-vars? elements)]
                                    [($ type-values type)
                                     (has-free-vars? type)]
                                    [($ type-var reach name recur)
                                     (if (bound? name) #f (k #t))]
                                    [($ type-vector element)
                                     (has-free-vars? element)]
                                    [_ (error 'has-free-vars? "Unmatched type ~a" type)])])
                              (has-free-vars? type))))))
  
  (ea-define/contract get-referenced-vars ((union type? handle?) . -> . (listof symbol?))
                      (lambda (type)
                        (let* ([refed (make-hash-table)]
                               [nop (lambda x cst:void)])
                          (let loop ([type type])
                            (match type
                              [(? handle? type) cst:void]
                              [($ type-case-lambda rest-arg?s req-args argss exps)
                               (let* ([argss (for-each-vov loop argss)]
                                      [exps (for-each-vector loop exps)])
                                 cst:void)]
                              [($ type-cons hd tl)
                               (loop hd) (loop tl)]
                              [($ type-promise value)
                               (loop value)]
                              [($ type-rec vars handle-list body)
                               (error 'get-referenced-vars "Nested type-rec found")]
                              [($ type-struct-value label types)
                               (map loop types)]
                              [($ type-union elements)
                               (map loop elements)]
                              [($ type-values type)
                               (loop  type)]
                              [($ type-var name reach recur)
                               (hash-table-put! refed name #t)]
                              [($ type-vector element)
                               (loop element)])
                            (hash-table-map refed (lambda (v _) v))))))
  
  
  ;;
  ;; Utility functions
  ;;
  
  (ea-define/contract lol->vov ((listof (listof any?)) . -> . vector?)
                      (lambda (xss) (list->vector (map list->vector xss))))
  
  (ea-define/contract for-each-vector ((any? . -> . any) vector? . -> . void?)
                      (lambda (f v)
                        (let ([len (vector-length v)])
                          (let loop ([i 0])
                            (when (< i len)
                              (f (vector-ref v i))
                              (loop (add1 i)))))
                        cst:void))
  
  (ea-define/contract map-vector ((any? . -> . any) vector? . -> . vector?)
                      (lambda (f v)
                        (let* ([len (vector-length v)]
                               [new-v (make-vector len #f)])
                          (let loop ([i 0])
                            (when (< i len)
                              (vector-set! new-v i (f (vector-ref v i)))
                              (loop (add1 i))))
                          new-v)))
  
  (ea-define/contract map-vector-of-vector ((any? . -> . any) (vectorof vector?) . -> . (vectorof vector?))
                      (lambda (f vov)
                        (map-vector (lambda (v) (map-vector f v)) vov)))
  
  ; Replace each element e in a vector with (f e)
  (ea-define/contract for-each-vector! ((any? . -> . any) vector? . -> . vector?)
                      (lambda (f v)
                        (let ([len (vector-length v)])
                          (let loop ([i 0])
                            (when (< i len)
                              (vector-set! v i (f (vector-ref v i)))            
                              (loop (add1 i)))))
                        v))
  
  (ea-define/contract for-each-vov ((any? . -> . any) (vectorof (vectorof any?)) . -> . void?)
                      (lambda (f vov)
                        (for-each-vector (lambda (v) (for-each-vector f v) v) vov)))
  
  ; Replace each element in a vector of vectors with (f e)
  (ea-define/contract for-each-vov! ((any? . -> . any) (vectorof (vectorof any?)) . -> . any)
                      (lambda (f vov)
                        (for-each-vector! (lambda (v) (for-each-vector! f v) v) vov)
                        vov))
  
  (define foldr-case-lambda-vector
    (lambda (f init rest-arg?s req-args argss exps)
      (let ([len (vector-length rest-arg?s)])
        (let loop ([i 0])
          (if (= i len) init
              (f (vector-ref rest-arg?s i)
                 (vector-ref req-args i)
                 (vector-ref argss i)
                 (vector-ref exps i)
                 (loop (add1 i))))))))
  
  (define (foldr-vector f init v)
    (let loop ([i 0])
      (if (= i (vector-length v)) init
          (f (vector-ref v i) (loop (add1 i))))))
  
  (define vector-of?
    (lambda (pred v)
      (let/ec escape
        (let loop ([i 0])
          (if (= i (vector-length v)) #t
              (if (pred (vector-ref v i))
                  (loop (add1 i))
                  (escape #f)))))))
  
  (define vector-of-vector-of?
    (lambda (pred vov)
      (vector-of? (lambda (v) (vector-of? pred v)) vov)))
  
  (define vector-has?
    (lambda (pred v)
      (let/ec escape
        (let loop ([i 0])
          (if (= i (vector-length v)) #f
              (if (pred (vector-ref v i))
                  (escape #t)
                  (loop (add1 i))))))))
  
  (define vector-of-vector-has?
    (lambda (pred vov)
      (vector-has? (lambda (v) (vector-has? pred v)) vov)))
  
  (define vector-foldr
    (lambda (f init xs)
      (let loop ([i 0])
        (if (= i (vector-length xs)) init
            (f (vector-ref xs i) (loop (add1 i)))))))
  
  (define unfold
    (lambda (p f g seed)
      (if (p seed) '()
          (cons (f seed) (unfold p f g (g seed))))))
  
  ;; int -> list int
  (define iota
    (lambda (n)
      (unfold (lambda (x) (= x n)) (lambda (x) x) add1 0)))
  
  (define iota-positive
    (lambda (n)
      (unfold (lambda (x) (= x n)) add1 add1 0)))
  
  (define min-list-numbers
    (let ([gt (lambda (x y) (> x y))]
          [remove-duplicates              ;; remove duplicate numbers from a sorted list
           (lambda (xs)                   ;; of numbers, returned list is reversed
             (if (null? xs) '()
                 (let loop ((xs (cdr xs)) (acc (list (car xs))))
                   (if (null? xs) acc
                       (if (< (car xs) (car acc))
                           (loop (cdr xs) (cons (car xs) acc))
                           (loop (cdr xs) acc))))))])
      (lambda (nums)
        (remove-duplicates (list:mergesort nums gt)))))
  
  (define assert-never (lambda () (error 'assert-never "Should not have reached this point")))
  
  (ea-define/contract hash-table-has-key? (hash-table? any? . -> . boolean?)
                      (lambda (hash-table key)
                        (if (hash-table-get hash-table key cst:thunk-false) #t #f)))
  
  ;; (hash-table key (list value)) key value -> (hash-table key (list value))
  (ea-define/contract hash-table-prepend! (hash-table? any? any? . -> . any)
                      (lambda (hash-table key value)
                        (hash-table-put! hash-table key
                                         (if (hash-table-has-key? hash-table key)
                                             (cons value (hash-table-get hash-table key assert-never))
                                             (list value)))))
  
  ;;
  ;; Printing Functions
  ;;
  
  (ea-define/contract dfa-state->string (dfa-state? . -> . string?)
                      (opt-lambda (dfa-state (tbl 'not-present))
                        (match dfa-state
                          [($ handle-state state handle)
                           (if (eq? tbl 'not-present)
                               (string-append "(handle " (number->string state) " " (number->string handle) ") ")
                               (string-append "(handle " (number->string state) " " (number->string handle) " -> ["
                                              (pretty-print (get-type tbl handle)) "])"))]
                          [($ cons-state state car cdr) (string-append "(cons " (number->string state) " "
                                                                       (number->string car)
                                                                       " " (number->string cdr) ")")]
                          [($ case-lambda-state state rest-arg?s req-args argss exps)
                           (string-append "(case-lambda " (number->string state)
                                          (foldr-case-lambda-vector
                                           (lambda (rest-arg? req-arg args exp acc)
                                             (string-append "[" ; rest-arg "," req-arg ", "
                                                            (vector-foldr (lambda (arg arg-acc)
                                                                            (string-append " " (number->string arg) arg-acc))
                                                                          ""
                                                                          args)
                                                            (if rest-arg? "*-> " "-> ")
                                                            (number->string exp)
                                                            "]"
                                                            acc))
                                           ")" rest-arg?s req-args argss exps))]
                          [($ promise-state state value) 
                           (string-append "(promise " (number->string state) " " (number->string value) ")")]
                          [($ struct-value-state state label types)
                           (string-append
                            "#(struct:"
                            " " (number->string state) " " 
                            (list:foldr
                             (lambda (elt-type str) (string-append (number->string elt-type)
                                                                   (if (string=? str ")")  ""    " ")
                                                                   str))
                             ")"
                             types))]
                          [($ union-state state elements)
                           (string-append "(union " (number->string state) " "
                                          (list:foldr (lambda (elem acc)
                                                        (string-append " " (number->string elem) acc)) ")" elements))]
                          [($ values-state state type)
                           (string-append "(values " (number->string state) " " (number->string type) ")")]
                          [($ vector-state state type)
                           (string-append "(vector " (number->string state) " " (number->string type) ")")]
                          [x (error 'dfa-state->string "Unmatched type ~a\n" x)]
                          )))
  
  (define print-dfa
    (lambda (dfa)
      (printf "(")
      (hash-table-for-each dfa (lambda (k v) (printf "~a " (dfa-state->string v))))
      (printf ")")
      (newline)))
  
  (define print-hashcons-table
    (lambda (tbl)
      (printf "Hashcons table has ~a elements~%" (hashcons-table-number-handles tbl))
      (hash-table-for-each (hashcons-table-from-handle tbl)
                           (lambda (h type)
			     (printf "Handle: ~a --> " h) (pretty-print type)))
      (printf "------------------------------\n")
      (hash-table-for-each (hashcons-table-from-label tbl)
                           (lambda (label h)
                             (printf "Label: ") (pretty-print label) (printf " --> Handle: ~a~%"  h)))
      (printf "------------------------------\n")
      (hash-table-for-each (hashcons-table-from-base-type tbl)
                           (lambda (base-type h)
                             (printf "Base Type: ") (pretty-print base-type) (printf " --> Handle: ~a~%" h)))
      (printf "------------------------------\n")
      (hash-table-for-each (hashcons-table-from-dfa tbl)
                           (lambda (dfa h)
                             (printf "DFA: ") (pretty-print dfa) (printf " --> Handle: ~a~%" h)))
      (printf "------------------------------\n")))
  
  (ea-define/contract handle->string (hashcons-table? handle? . -> . string?)
                      (lambda (tbl handle)
                        (letrec
                            ([handle->var (make-hash-table)]
                             [handle->binding (make-hash-table)]
                             [get-next-var! (let ([*i* 0]) (lambda (handle)
                                                             (let ([new-i (+ *i* 1)]
                                                                   [str (string-append "a" (number->string *i*))])
                                                               (set! *i* (+ *i* 1))
                                                               (hash-table-put! handle->var handle str)
                                                               str)))]
                             [base-type->string
                              (lambda (type)
                                (match type
                                  [($ type-empty) "_"]
                                  [($ type-cst type) (string:expr->string type)]
                                  [($ type-struct-type label)
                                   (string-append "#<struct-type:" (symbol->string (label-struct-type-name label)) ">")]
                                  [_ (error 'base-type->string "No such base type ~a" type)]))]
                             [label-type->string
                              (lambda (type ancest)
                                (match type
                                  [($ type-cons hd tl)
                                   (string-append "(cons " (loop hd ancest) " " (loop tl ancest) ")")]
                                  [($ type-case-lambda rest-arg?s req-args argss exps)
                                   (string-append "(case-lambda "
                                                  (foldr-case-lambda-vector (lambda (rest-arg? req-arg args exp acc)
                                                                              (string-append
                                                                               "["
                                                                               (foldr-vector (lambda (arg acc)
                                                                                               (string-append (loop arg ancest) " " acc))
                                                                                             (if rest-arg? "*-> " "-> ")
                                                                                             args)
                                                                               (loop exp ancest) "]" acc))
                                                                            ")"
                                                                            rest-arg?s req-args argss exps))]
                                  [($ type-promise value) (string-append "(promise " (loop value ancest) ")")]
                                  [($ type-struct-value label types)
                                   (string-append "#(struct:"
                                                  (symbol->string (if (label-struct-type? label)  (label-struct-type-name label) label))
                                                  " "
                                                  (list:foldr
                                                   (lambda (elt-type str)
                                                     (string-append
                                                      (loop elt-type ancest)
                                                      (if (string=? str ")") ""  " ")
                                                      str))
                                                   ")"
                                                   types))]
                                  [($ type-values values-type)
                                   (cond
                                     [(type-empty? values-type) (loop values-type ancest)]
                                     [(and (type-cst? values-type) (eq? (type-cst-type values-type) 'top))
                                      (loop values-type ancest)]
                                     [else
                                      (string-append "(values " (loop values-type ancest) ")")])]
                                  [($ type-vector element) (string-append "(vector " (loop element ancest) ")")]
                                  [($ type-union elements)
                                   (string-append "(union"
                                                  (list:foldr (lambda (element acc)
                                                                (string-append " " (loop element ancest) acc)) ")" elements))]
                                  [else (error 'hashcons-type-string "~a Not implemented yet" type)]))]
                             [loop (lambda (handle ancest)
                                     (if (set-in? ancest handle) ;; if we've already come across this node
                                         ;; Add a back reference
                                         (if (hash-table-get handle->var handle cst:thunk-false)
                                             (hash-table-get handle->var handle)
                                             (get-next-var! handle))
                                         (let* ([type (get-type tbl handle)]
                                                [str (cond
                                                       [(has-base-type? tbl type) (set-set ancest handle) (base-type->string type)]
                                                       [(has-label-type? tbl type) (label-type->string type (set-set ancest handle))]
                                                       [(has-dfa-type? tbl type) (label-type->string type (set-set ancest handle))])])
                                           (set-remove ancest handle) ;; imperative sets
                                           (if (hash-table-has-key? handle->var handle)
                                               (begin
                                                 (hash-table-put! handle->binding handle str)
                                                 (hash-table-get handle->var handle))
                                               str))))])
                          (let* ([rec-body (loop handle (set-make))]
                                 [var-bindings
                                  (list:foldr
                                   (lambda (cur acc) (string-append cur acc)) ""
                                   (hash-table-map handle->var
                                                   (lambda (handle var)
                                                     (string-append "[" var " "
                                                                    (hash-table-get handle->binding handle
                                                                                    (lambda () (error 'handle->string
                                                                                                      "No binding for var handle ~a" handle)))
                                                                    "]"))))])
                            (if (string=? "" var-bindings) rec-body
                                (string-append "(rec-type (" var-bindings ") " rec-body ")"))))))
  
  (define (length-one? x) (and (pair? x) (null? (cdr x))))
  
  (define (empty-hash? h)
    (let/ec escape
      (hash-table-for-each h (lambda (k v) (escape #f)))
      #t))
  
  (define (hash-table-size h)
    (let ([size 0])
      (hash-table-for-each h (lambda (_ _2) (set! size (add1 size))))
      size))
  
  (define (select pred xs ys)
    (list:foldr (lambda (x y acc) (if (pred x) (cons y acc) acc)) '() xs ys))
  
  ;; get a list of strongly connected components in reverse
  ;; topological order, taken from CLR
  (ea-define/contract strongly-connected-components
                      (hash-table? . ->d .
                                   (lambda (h)
                                     (lambda (list-of-sccs)
                                       (= (hash-table-size h) (apply + (map length list-of-sccs))))))
                      (lambda (graph)
                        (letrec
                            ;; finished nodes from most recently finished to first finished
                            ([finished-nodes (box ())]  
                             [color (make-hash-table)]
                             
                             [transpose-graph
                              (lambda (graph)
                                (let ([new-graph (make-hash-table)])
                                  (hash-table-for-each graph (lambda (node adj-list)
                                                               (hash-table-put! new-graph node null)))
                                  (hash-table-for-each graph (lambda (node adj-list)
                                                               (for-each (lambda (adj-node)
                                                                           (hash-table-put! new-graph adj-node
                                                                                            (cons node (hash-table-get new-graph adj-node))))
                                                                         adj-list)))
                                  new-graph))]
                             [dfs-visit (lambda (graph u visited-nodes)
                                          (hash-table-put! color u 'gray)
                                          (let* ([adj (hash-table-get graph u)]
                                                 [new-nodes
                                                  (list:foldl (lambda (v visited)
                                                                (if (eq? (hash-table-get color v) 'white)
                                                                    (dfs-visit graph v visited)
                                                                    visited))
                                                              visited-nodes
                                                              adj)])
                                            (hash-table-put! color u 'black)
                                            (set-box! finished-nodes (cons u (unbox finished-nodes)))
                                            (cons u new-nodes)))]
                             [dfs (lambda (graph nodes-to-visit)
                                    (hash-table-for-each graph
                                                         (lambda (u _) (hash-table-put! color u 'white)))
                                    (let ([sccs (list:foldl (lambda (u sccs)
                                                              (if (eq? (hash-table-get color u) 'white)
                                                                  (cons (dfs-visit graph u null) sccs)
                                                                  sccs))
                                                            '()
                                                            nodes-to-visit)])
                                      sccs))])
                          (dfs graph (hash-table-map graph (lambda (k adj) k)))
                          (dfs (transpose-graph graph) (unbox finished-nodes)))))
  
  
  ;;
  ;; DFA minimization
  ;;
  
  (define cross2
    (lambda (xs ys)
      (list:foldl (lambda (x xacc)
                    (list:foldl (lambda (y yacc) (cons (cons x y) yacc)) xacc ys))
                  '() xs)))
  
  (define coalesce-lists
    (lambda xs
      (letrec ([reverse-onto
                (lambda (xs ys)
                  (if (null? xs) ys
                      (reverse-onto (cdr xs) (cons (car xs) ys))))])
        (if (null? xs) '()
            (list:foldl reverse-onto (car xs) (cdr xs))))))
  
  (define-struct equiv-class (type number length classes) (make-inspector))
  
  
  ;; Each element of the partitions table contains either an equivalence class,
  ;; or false if it hasn't been used or the equivalence class it contains has
  ;; already been split. 
  (define (make-partitions k)
    (make-vector k #f))
  
  (define size length)
  
  (define mark-equiv-class-split
    (lambda (partitions k)
      (vector-set! partitions k #f)))
  
  (ea-define/contract place-new-equiv-class
                      ((vectorof (union equiv-class? false?)) equiv-class? . -> . any)
                      (lambda (partitions eq-class)
                        (vector-set! partitions (equiv-class-number eq-class) eq-class)))
  
  (ea-define/contract get-equiv-class
                      ((vectorof (union equiv-class? false?)) natural? . -> . equiv-class?)
                      (lambda (partitions k)
                        (if (vector-ref partitions k)
                            (vector-ref partitions k)
                            (error 'get-equiv-class "Equiv class ~a #f. Already split?" k))))
  
  (define make-state->equiv-class
    (lambda (num-states)
      (make-vector num-states #f)))
  
  (define get-equiv-class-of-state
    (lambda (classes state-num)
      (vector-ref classes state-num)))
  
  (define set-equiv-class-of-state!
    (lambda (classes equiv-class state)
      (vector-set! classes (dfa-state-state state) (equiv-class-number equiv-class))))
  
  (ea-define/contract set-equiv-class-of-states!
                      (vector? equiv-class? (listof dfa-state?) . -> . any)
                      (lambda (classes equiv-class states)
                        (for-each (lambda (state) (set-equiv-class-of-state! classes equiv-class state)) states)))
  
  ; split q0 into 2 equivalence classes, those which transitions to q1 from
  ; letter b and those which don't transition to q1
  ;
  ; The 'letter' b depends on the type of partiton we're splitting. A letter
  ; consists of a place within the type E.g. A type-cons letter has a 'position'
  ; indicator of 'car or 'cdr to distinguish which position in a partition of
  ; type-cons we're going to split by.
  (ea-define/contract split
                      (natural? natural? list? vector? vector? any? . -> . any)
                      (lambda (q0-num q1-num b partitions state->equiv-class get-next-equiv-class)
                        (let* ([q0 (get-equiv-class partitions q0-num)]
                               [type1 (equiv-class-type q0)]
                               [transitions-to-q1?
                                (lambda (q0 b)
                                  (match b
                                    [('handle) #f]
                                    [('case-lambda 'exps row)
                                     (and (case-lambda-state? q0)
                                          (< row (vector-length (case-lambda-state-exps q0)))
                                          (get-equiv-class-of-state
                                           state->equiv-class
                                           (vector-ref (case-lambda-state-exps q0) row)))]
                                    [('case-lambda 'argss row col)
                                     (and (case-lambda-state? q0)
                                          (let ([argss (case-lambda-state-argss q0)])
                                            (and (< row (vector-length argss))
                                                 (< col (vector-length (vector-ref argss row)))
                                                 (get-equiv-class-of-state
                                                  state->equiv-class
                                                  (vector-ref (vector-ref argss row) col)))))]
                                    [('cons pos)
                                     (and (cons-state? q0)
                                          (get-equiv-class-of-state
                                           state->equiv-class
                                           ((if (eq? pos 'car) cons-state-car cons-state-cdr) q0)))]
                                    [('promise)
                                     (and (promise-state? q0)
                                          (get-equiv-class-of-state state->equiv-class (promise-state-value q0)))]
                                    [('struct-value pos)
                                     (and (struct-value-state? q0)
                                          (< (length (struct-value-state-types q0)) pos)
                                          (get-equiv-class-of-state state->equiv-class
                                                                    (list-ref (struct-value-state-types q0) pos)))]
                                    [('union pos)
                                     (and (union-state? q0)
                                          (< pos (length (union-state-elements q0)))
                                          (get-equiv-class-of-state state->equiv-class
                                                                    (list-ref (union-state-elements q0) pos)))]
                                    [('values)
                                     (and (values-state? q0)
                                          (get-equiv-class-of-state state->equiv-class (values-state-type q0)))]
                                    [('vector)
                                     (and (vector-state? q0)
                                          (get-equiv-class-of-state state->equiv-class (vector-state-element q0)))]))])
                          ; this always makes a new list, even if not splittable. probably
                          ; faster to switch to new list midway through
                          (let loop ([q0 (equiv-class-classes q0)] [to-q1 '()] [not-to-q1 '()])
                            (cond [(null? q0)
                                   (if (and (not (null? to-q1)) (not (null? not-to-q1)))
                                       (let* ([to-q1-num (get-next-equiv-class)]
                                              [to-q1 (make-equiv-class type1 to-q1-num (length to-q1) to-q1)]
                                              [not-to-q1-num (get-next-equiv-class)]
                                              [not-to-q1 (make-equiv-class type1 not-to-q1-num
                                                                           (length not-to-q1) not-to-q1)])
                                         (place-new-equiv-class partitions to-q1)
                                         (place-new-equiv-class partitions not-to-q1)
                                         (set-equiv-class-of-states! state->equiv-class to-q1 (equiv-class-classes to-q1))
                                         (set-equiv-class-of-states! state->equiv-class not-to-q1 (equiv-class-classes not-to-q1))
                                         (mark-equiv-class-split partitions q0-num)
                                         (values to-q1-num not-to-q1-num))
                                       (values #f #f))]
                                  [(eq? (transitions-to-q1? (car q0) b) q1-num)
                                   (loop (cdr q0) (cons (car q0) to-q1) not-to-q1)]
                                  ;; q0 does not transition on b
                                  [else
                                   (loop (cdr q0) to-q1 (cons (car q0) not-to-q1))])) )))
  
  ; Hopcrofts DFA minimization algorithm. First generate letters for each
  ; partition individually as the have different types and shapes. Next while
  ; there are still letters which may split an equivalence class, try to split
  ; each equivalence class by the letter. Most times the split will fail, but if
  ; it succeeds then replace the old equivalence class with the new split
  ; equivalence classes.  The letter/equiv class pairs will need to be changed
  ; to point to the new equivalence classes
  ; 
  ; partition-nums : (listof natural) list of partition indexes with identical type/shapes
  (ea-define/contract hopcroft
                      ((listof natural?) (listof natural?) (vectorof (union false? equiv-class?)) any? any? . -> . any)
                      (lambda (states partition-nums partitions state->equiv-class get-next-equiv-class)
                        (if (null? partition-nums) (void)
                            (let* ([l (set-make 'equal)]
                                   [largest-number-cl-exps -1]
                                   [largest-number-cl-args -1]
                                   [largest-number-union-elements -1]
                                   [largest-number-struct-value-types -1]
                                   [_ (for-each-vector
                                       (lambda (ec)
                                         (when ec
                                           (cond [(eq? 'case-lambda (equiv-class-type ec))
                                                  (let* ([cl (car (equiv-class-classes ec))]
                                                         [argss (case-lambda-state-argss cl)]
                                                         [exps (case-lambda-state-exps cl)])
                                                    (when (> (vector-length exps) largest-number-cl-exps)
                                                      (set! largest-number-cl-exps (vector-length exps)))
                                                    (when (> (vector-foldr (lambda (c acc) (max (vector-length c) acc)) -1 argss)
                                                             largest-number-cl-args)
                                                      (set! largest-number-cl-args (vector-length exps))))]
                                                 [(eq? 'union (equiv-class-type ec))
                                                  (let* ([union (car (equiv-class-classes ec))]
                                                         [len (length (union-state-elements union))])
                                                    (when (> len largest-number-union-elements)
                                                      (set! largest-number-union-elements len)))]
                                                 [(eq? 'struct-value (equiv-class-type ec))
                                                  (let* ([struct-value (car (equiv-class-classes ec))]
                                                         [len (length (struct-value-state-types struct-value))])
                                                    (when (> len largest-number-struct-value-types)
                                                      (set! largest-number-struct-value-types len)))])))
                                       partitions)]
                                   [letters
                                    (let* ([letters
                                            (list:foldr
                                             (lambda (equiv-class-num letters)
                                               (let* ([equiv-class (get-equiv-class partitions equiv-class-num)]
                                                      [state (car (equiv-class-classes equiv-class))])
                                                 (cond [(handle-state? state) letters]
                                                       [(cons-state? state)
                                                        (cons '(cons car) (cons '(cons cdr) letters))]
                                                       [(promise-state? state)
                                                        (cons '(promise) letters)]
                                                       [(values-state? state)
                                                        (cons '(values) letters)]
                                                       [(vector-state? state)
                                                        (cons '(vector) letters)]
                                                       [else
                                                        letters])))
                                             '() partition-nums)]
                                           [letters   ;; add letters for case-lambda
                                            (if (= largest-number-cl-args -1) letters
                                                (let ([w-argss
                                                       (list:foldr (lambda (row acc)
                                                                     (coalesce-lists (list:foldr (lambda (col acc)
                                                                                                   (cons (list 'case-lambda 'argss row col) acc))
                                                                                                 '()
                                                                                                 (iota largest-number-cl-args))
                                                                                     acc))
                                                                   letters
                                                                   (iota largest-number-cl-exps))])
                                                  (coalesce-lists
                                                   (map (lambda (row) (list 'case-lambda 'exps row)) (iota largest-number-cl-exps))
                                                   w-argss)))]
                                           [letters ;; add letters for unions
                                            (if (= largest-number-union-elements -1) letters
                                                (coalesce-lists
                                                 (map (lambda (i) (list 'union i)) (iota largest-number-union-elements))
                                                 letters))])
                                      (if (= largest-number-struct-value-types -1) letters
                                          (coalesce-lists 
                                           (map (lambda (i) (list 'struct-value i)) (iota largest-number-struct-value-types))
                                           letters)))]
                                   ; This is a cheesy way to remove a random element from the set.  XXX ?
                                   [get-next! (lambda ()
                                                (let ([eq&letter (let/ec return (set-for-each l (lambda (elem) (return elem))) #f)])
                                                  (when eq&letter
                                                    (set-remove l eq&letter))
                                                  eq&letter))]
                                   [add-to-L!
                                    (lambda (eq letter)
                                      (set-set l (cons eq letter)))]
                                   [print-L
                                    (lambda ()
                                      (printf "(L=") (set-for-each l display)(printf ")"))]
                                   [remove! (lambda (eq-class-num letter)
                                              (set-remove l (cons eq-class-num letter)))]
                                   [eq&letter-present? (lambda (eq-class-num letter)
                                                         (set-in? l (cons eq-class-num letter)))])
                              (for-each (lambda (eq&letter)
                                          ; (printf " ~a" eq&letter)
                                          (set-set l eq&letter))
                                        (cross2 partition-nums letters))
                              (let while-letters ([eq&letter (get-next!)] [partition-nums partition-nums])
                                (when eq&letter
                                  (let ([q1 (car eq&letter)]
                                        [a (cdr eq&letter)]
                                        [new-partition-nums '()])
                                    (for-each
                                     (lambda (q0)
                                       (let-values ([(equiv-class-a equiv-class-b)
                                                     (split q0 q1 a partitions state->equiv-class get-next-equiv-class)])
                                         (if equiv-class-a ;; when the split is successful
                                             (begin
                                               ;			    (printf "(split q0=~a q1=~a a=~a)~n" q0 q1 a)
                                               (set! new-partition-nums
                                                     (cons equiv-class-a (cons equiv-class-b new-partition-nums)))
                                               (for-each (lambda (b)
                                                           (if (eq&letter-present? q0 b)
                                                               (begin
                                                                 (remove! q0 b)
                                                                 (add-to-L! equiv-class-a b)
                                                                 (add-to-L! equiv-class-b b))
                                                               (begin
                                                                 (add-to-L! (if (< (equiv-class-length (get-equiv-class partitions equiv-class-a))
                                                                                   (equiv-class-length (get-equiv-class partitions equiv-class-b)))
                                                                                equiv-class-a
                                                                                equiv-class-b)
                                                                            b)))
                                                           )
                                                         letters))
                                             (begin
                                               ; (printf "(did not split q0=~a q1=~a a=~a)~n" q0 q1 a)
                                               (set! new-partition-nums (cons q0 new-partition-nums))))))
                                     partition-nums)
                                    (while-letters (get-next!) new-partition-nums))))))))
  
  ; Minimize DFA sets up the partition table and the gross equivalence classes
  ; for hopcrofts algorithm, as well as replacing the states numbers with their
  ; equivalence classes after minimization.
  (define minimize-dfa
    (lambda (dfa original-states)
      (let* ([highest-equiv-class -1]
             [get-next-equiv-class (lambda ()
                                     (set! highest-equiv-class (add1 highest-equiv-class))
                                     highest-equiv-class)]
             [get-highest-equiv-class (lambda () highest-equiv-class)]
             [get-matching-states
              (let ([state-values (hash-table-map dfa (lambda (k v) (list k v)))])
                (lambda (pred)
                  (map cadr (list:filter (lambda (sv) (pred (cadr sv))) state-values))))]
             [state-numbers (hash-table-map dfa (lambda (k v) k))]
             [num-states (length state-numbers)]
             [_ (when (= num-states 0)
                  (error 'minimize-dfa "No states!"))]
             [highest-state-number
              (list:foldr (lambda (x cur-max) (if (> x cur-max) x cur-max)) -1 state-numbers)]
             [state->equiv-class
              (make-state->equiv-class (add1 highest-state-number))]
             
             ;; We split at most num-states - 1 times, but we never reuse the
             ;; states in an old partition so allocate twice the number of states.
             [partitions (make-partitions (* 2 num-states))] 
             [min '()]
             [add-minimum-dfa-state!
              (lambda (state)
                (when (member state min)
                  (error 'add-minimum-dfa-state! "Should never add the same state ~a to minimal DFA"
                         (dfa-state-state state)))
                (set! min (cons state min)))]
             [make-minimized-state
              (match-lambda
                  [($ handle-state state handle)
                   (make-handle-state (get-equiv-class-of-state state->equiv-class state) handle)]
                [($ case-lambda-state state  rest-arg?s req-args argss exps)
                 (make-case-lambda-state (get-equiv-class-of-state state->equiv-class state)
                                         rest-arg?s req-args
                                         (for-each-vov! (lambda (state)
                                                          (get-equiv-class-of-state state->equiv-class state)) argss)
                                         (for-each-vector! (lambda (state)
                                                             (get-equiv-class-of-state state->equiv-class state))
                                                           exps))]
                [($ cons-state state car cdr)
                 (make-cons-state (get-equiv-class-of-state state->equiv-class state)
                                  (get-equiv-class-of-state state->equiv-class car)
                                  (get-equiv-class-of-state state->equiv-class cdr))]
                [($ promise-state state value)
                 (make-promise-state (get-equiv-class-of-state state->equiv-class state)
                                     (get-equiv-class-of-state state->equiv-class value))]
                [($ struct-value-state state label types)
                 (make-struct-value-state (get-equiv-class-of-state state->equiv-class state)
                                          label
                                          (map (lambda (type)
                                                 (get-equiv-class-of-state state->equiv-class type))
                                               types))]
                [($ union-state state elements)
                 (make-union-state (get-equiv-class-of-state state->equiv-class state)
                                   (min-list-numbers (map (lambda (type)
                                                            (get-equiv-class-of-state state->equiv-class type))
                                                          elements)))]
                [($ values-state state type)
                 (make-values-state (get-equiv-class-of-state state->equiv-class state)
                                    (get-equiv-class-of-state state->equiv-class type))]
                [($ vector-state state element)
                 (make-vector-state (get-equiv-class-of-state state->equiv-class state)
                                    (get-equiv-class-of-state state->equiv-class element))]
                [x (error 'make-minimized-state "Unmatched type ~a" x)])]
             [case-lambda-partition (split-case-lambda-states (get-matching-states case-lambda-state?))]
             [struct-value-partition  (split-struct-value-states (get-matching-states struct-value-state?))]
             [union-partition (split-union-states (get-matching-states union-state?))]
             [handle-partition
	      (list:mergesort (map list (get-matching-states handle-state?))
                              (lambda (x y) (< (handle-state-handle (car x)) (handle-state-handle (car y)))))]
             [setup-equiv-class
	      (lambda (states type)
		(if (null? states) #f
		    (let* ([equiv-class-number (get-next-equiv-class)]
			   [equiv-class (make-equiv-class type equiv-class-number (length states) states)])
		      (place-new-equiv-class partitions equiv-class)
		      (set-equiv-class-of-states! state->equiv-class equiv-class states)
		      equiv-class-number)))]
             [handle-partition-numbers
	      (map (lambda (states) (setup-equiv-class states 'handle)) handle-partition)]
             [cl-partition-numbers
              (map (lambda (states) (setup-equiv-class states 'case-lambda)) case-lambda-partition)]
             [struct-value-numbers
              (map (lambda (states) (setup-equiv-class states 'struct-value)) struct-value-partition)]
             [union-numbers
              (map (lambda (states) (setup-equiv-class states 'union)) union-partition)]
	     [cons-number (setup-equiv-class (get-matching-states cons-state?) 'cons)]
             [promise-number (setup-equiv-class (get-matching-states promise-state?) 'promise)]
             [values-number (setup-equiv-class (get-matching-states values-state?) 'values)]
             [vector-number (setup-equiv-class (get-matching-states vector-state?) 'vector)])
        ;; There is no position ordering on the elements of a union so we
        ;; impose one on the equivalence classes of the elements
        (for-each (lambda (states)
                    (for-each (lambda (state)
                                (set-union-state-elements! state
                                                           (list:mergesort (union-state-elements state)
                                                                           (lambda (a b)
                                                                             (> (get-equiv-class-of-state state->equiv-class a)
                                                                                (get-equiv-class-of-state state->equiv-class b))))))
                              states))
                  union-partition)
        ;            (printf "~a~n" case-lambda-partition)
        (hopcroft state-numbers
                  (list:filter cst:id
                               (append handle-partition-numbers
				       cl-partition-numbers struct-value-numbers union-numbers
                                       (list cons-number promise-number values-number vector-number)))
                  partitions state->equiv-class get-next-equiv-class)
	(let* ([minimized-dfa (vector-foldr
			       (lambda (p acc)
				 (if p (cons (make-minimized-state (car (equiv-class-classes p))) acc) acc))
			       '() partitions)]
               [original->new-states
                (map (lambda (state) (get-equiv-class-of-state state->equiv-class state)) original-states)])
          ;	  (pretty-print partitions)
	  (values minimized-dfa original->new-states))
        )))
  
  ) ;; end (module hashcons

;; EOF