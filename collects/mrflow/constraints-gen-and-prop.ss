; In the first clause, the first label is the origin label (the origin of the arrow), the
; second label in the one flowing along the arrow. The destination of the arrow is embedded
; in the edge, and can be obtained by applying the second clause.
; (define-type edge (case-lambda [label label -> void] [-> label]))

(module
 constraints-gen-and-prop mzscheme
 (require (prefix kern: (lib "kerncase.ss" "syntax"))
          (prefix list: (lib "list.ss"))
          (prefix string: (lib "string.ss")) ; XXX show-expanded debug
          )
 (provide ;sba-driver
  create-label-from-term
  initialize-primitive-type-schemes
  check-primitive-types
  reset-all
  get-prims
  get-loc
  get-var
  get-type
  pp-type
  parents
  children
  has-member?
  ast-nodes graph-nodes graph-edges ; XXX perf
  show-expanded ; XXX debug
  )
 ; debug XXX
 ;read-and-analyze label-case-lambda-exps label-set label-term)
 
; (define-syntax mycar
;   (let ([counter 1])
;     (lambda (stx)
;       (syntax-case stx ()
;         [(_ x)
;          (begin
;            (printf "counter: ~a~n" counter)
;            (let ([stx-out (datum->syntax-object (syntax x)
;                                                 `(#%app mycar2 ,counter ,(syntax x)))])
;              (set! counter (add1 counter))
;              stx-out))]))))
; 
; (define (mycar2 n x)
;   (printf "car: ~a~n" n)
;   (label-term x))

 ; XXX perf analysis
 (define ast-nodes 0)
 (define graph-nodes 0)
 (define graph-edges 0)
 
 (define (reset-perf)
   (set! ast-nodes 0)
   (set! graph-nodes 0)
   (set! graph-edges 0)
   )
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FLOW GRAPH LABELS & PSEUDO_LABELS
 
 ; type-var = (union type-var #f), trace = boolean, tunnel? (union #f label), prim? = boolean
 ; term = syntax-object, set = (hash-table-of label (cons (listof label) (listof label)),
 ; edges = (listof edge))
 ; a flow graph label/node type-var is a type variable name used when reconstructing recursive
 ; types.
 ; trace is used to detect cycles in the graph during type reconstruction.
 ; tunnel? tells whether a label is currently tunneling through labels created during graph
 ; reconstruction from a primitive type. If the label is tunneling, then tunnel? indicates
 ; the last non-tunnel label (i.e. the entrance of the tunnel). E.g. in
 ; (let ([y (car (cons x 2))]) 3), the entrance of the tunnel is the label for x, then the
 ; value tunnels through the label for the first argument of cons, the label for the result
 ; of car, then exits the tunnel to flow into the label for y.
 ; prim? tells whether the label was created during graph reconstruction from a primitive
 ; type. We need this to detect tunneling.
 ; Note that the only reason we need to have this tunneling stuff is to keep the GUI arrows right.
 ; term: the source program term (or a fake version of it, in the case of "null" when we have
 ; a rest argument)
 ; set: contains label structures (see below) for all the values that flow into this term.
 ; Each label in the set has two lists of in and out edges pointing back and forth to the nodes
 ; from which this label has flowed in (or '() if the label is the source of the label) and
 ; flowed out to. The in edges (which need to be checked each time a propagation is done, to 
 ; revent cycles) are in a list, and not in an hash-table, because we assume that the same
 ; label is not going to flow very often into this term through several paths. The out-edge
 ; list is only used to draw arrows, so it doesn't have to be implemented very efficiently.
 ; Note that, since constants are represented by label structs, the same constant can appear
 ; several times in the set, even symbols.
 ; edges: functions that take two labels as argument and either propagate the second one to
 ; another label, using the first label as the source, or transform the graph accordingly (if
 ; the inflowing label is a function pseudo-label and the label into which it flows corresponds
 ; to the operator in an application, for example).
 (define-struct label (type-var trace tunnel? prim? term set edges) (make-inspector))
 
 ; a constant...
 (define-struct (label-cst label) (value) (make-inspector))
 
 ; car = label, cdr = label
 (define-struct (label-cons label) (car cdr) (make-inspector))
 
 ; rest-arg?s = (listof boolean), req-args = (listof number), argss = (listof (listof label)),
 ; exps = (listof label), top-free-varss = (listof (listof labels)),
 ; app-thunks = (listof (-> void))
 ; Each "level" of the six lists represents the args and body labels of a given clause in the
 ; case-lambda. At a given level, rest-arg? tells whether this clause has a rest argument,
 ; and req-args gives the number of required arguments, so it only has to be computed once.
 ; top-free-varss are the labels of the top level free variables in the corresponding clause.
 ; This field is updated as a side effect when analyzing top level variable references inside
 ; the body of a lambda. Edges flowing into these free variables must be created when the
 ; clause is applied. app-thunk is a thunk that is used to delay the transformation of the
 ; graph when a function flows into an application, until the clause around the application
 ; is itself applied.
 (define-struct (label-case-lambda label)
                (rest-arg?s req-args argss exps top-free-varss app-thunks) (make-inspector))
 
 ; labels = label
 ; used to simulate multiple values. So this label is going to flow around and work pretty
 ; much like a cons label. the problem is that multiple values are not first-class in Scheme,
 ; so we have to be careful to only propagate them through edges that correspond to the result
 ; of applications, never through edges that correspond to arguments of applications. Hence
 ; the reason for the complication in create-simple-edge. Note that define-struct expands
 ; into a define-values, so we need all that stuff.
 (define-struct (label-values label) (labels) (make-inspector))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MISC
 
 (define *dummy* (void))
 (define *dummy-thunk* (lambda () *dummy*))
 (define *hash-table-fail-empty* (lambda () '()))
 (define *hash-table-fail-false* (lambda () #f))
 
 ; (listof (list (listof term) label symbol string))
 ; term is where the error occured
 ; symbol is a color
 ; string is the actual error message
 (define *errors* 'uninitialized)
 
 (define (reset-derivation)
   (set! *errors* '())
   (set! *top-level-name->label* (make-hash-table))
   )
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LOCAL ENVIRONMENT
 
 ; (listof (cons symbol label)) (listof syntax-objects) (listof label)
 ; -> (listof (cons symbol label))
 ; the syntax objects in args are all atomic syntax objects for argument names
 ; the labels in args-labels are all simple labels (not pseudo-labels)
 (define (extend-env env args args-labels)
   ; doesn't matter whether we foldl or foldr
   (list:foldl 
    (lambda (arg arg-label env)
      (cons (cons (syntax-e arg) arg-label)
            env))
    env args args-labels))
 
 ; syntax-object (listof (cons symbol label)) -> (union label #f)
 (define (lookup-env var env)
   (let ([name-label-pair (assq (syntax-e var) env)])
     (if name-label-pair
         (cdr name-label-pair)
         #f)))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TOP LEVEL ENVIRONMENT
 
 ; (hash-table-of symbol label)
 (define *top-level-name->label* 'uninitialized)
 
 ; syntax-object -> label
 (define (add-top-level-name term)
   (let ([label (create-simple-pos-associated-label term)])
     (hash-table-put! *top-level-name->label* (syntax-object->datum term) label)
     label))
 
 ; symbol -> (union label #f)
 ; finds the label for a top level var.
 (define (lookup-top-level-name name)
   (hash-table-get *top-level-name->label* name *hash-table-fail-false*))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROPAGATION
 
 ; label boolean -> edge
 ; creates simple edge function that just propagates labels into in-label's set
 ; and propagates down the flow, taking cycles into account.
 ; note that the out-label is not a parameter of create-simple-edge, but a parameter of
 ; the resulting edge. This is both because it makes for nicer edges and edge creation code
 ; (since the edges are origin independant), and for historical reasons (because we used to
 ; create fake top level variables for lambdas and the edges that were added to these fake
 ; labels were moved to the label for the actual lambda when the enclosing lambda was applied
 ; - having to move edges meant they had to be origin independant). This also means that we
 ; can nicely re-use the same edge over and over when dealing with multiple values (see
 ; extend-edge-for-values below).
 (define (create-simple-edge in-label)
   (let ([in-set (label-set in-label)])
     (if (label-prim? in-label)
         (case-lambda
          [(out-label inflowing-label)
           ; initialize tunnel entrance when entering tunnel...
           (unless (label-tunnel? inflowing-label)
             (set-label-tunnel?! inflowing-label out-label))
           ; Note: we assume that primitives don't have internal cycles, so we
           ; don't have to keep track of in/out edges. We still have to put the
           ; inflowing-label in the set, because otherwise nothing is going to be
           ; propagated when we add a new edge to the in-label.
           (hash-table-put! in-set inflowing-label (cons '() '()))
           (for-each (lambda (edge)
                       (edge in-label inflowing-label))
                     (label-edges in-label))]
          [() in-label])
         (case-lambda
          [(out-label inflowing-label)
           (when (label-tunnel? inflowing-label)
             ; coming out of tunnel, so set the out-label to the entrance of tunnel,
             ; and reset tunneling.
             (set! out-label (label-tunnel? inflowing-label))
             (set-label-tunnel?! inflowing-label #f))
           (let* ([out-set (label-set out-label)]
                  [in/out-edges-pair-in-set
                   (hash-table-get in-set inflowing-label *hash-table-fail-false*)]
                  [in/out-edges-pair-out-set
                   (hash-table-get out-set inflowing-label *hash-table-fail-false*)])
             (if in/out-edges-pair-in-set
                 ; the value has already flown before into this set, which means it has
                 ; already been propagated further down. So we just need to update the
                 ; in/out edges. Note that a side effect of this is that we never loop
                 ; indefinitely inside a cycle.
                 (let ([in-edges-in-set (car in/out-edges-pair-in-set)]
                       [out-edges-in-set (cdr in/out-edges-pair-in-set)]
                       [in-edges-out-set (car in/out-edges-pair-out-set)]
                       [out-edges-out-set (cdr in/out-edges-pair-out-set)])
                   (hash-table-put! in-set inflowing-label
                                    (cons (cons out-label in-edges-in-set)
                                          out-edges-in-set))
                   (hash-table-put! out-set inflowing-label
                                    (cons in-edges-out-set
                                          (cons in-label out-edges-out-set))))
                 ; first time this inflowing label is propagated to in-label, so update the
                 ; in/out edges and propagate further down.
                 (let ([in-edges-out-set (car in/out-edges-pair-out-set)]
                       [out-edges-out-set (cdr in/out-edges-pair-out-set)])
                   (hash-table-put! in-set inflowing-label
                                    (cons (list out-label) '()))
                   (hash-table-put! out-set inflowing-label
                                    (cons in-edges-out-set
                                          (cons in-label out-edges-out-set)))
                   (for-each (lambda (edge)
                               (edge in-label inflowing-label))
                             (label-edges in-label)))))]
          [() in-label]))))
 
 ; label (label (listof label) -> void) -> void
 ; creates an edge from out-label to in-label and start the propagation for all the labels
 ; in out-label's set.
 ; Note: an edge is a function that updates the set of the in-label (and propagates further down
 ; the flow), so there's no need to have the in-label appear here explicitely as an argument.
 ; Note: if a function refers to a top level variable, and the function is applied twice and
 ; the top level variable refers both times to the same binding, we dont' want to end up with
 ; two parallel edges, so we have to test that.
 (define (add-edge-and-propagate-set-through-edge out-label new-edge)
   (let ([existing-edges (label-edges out-label)]
         [in-label (new-edge)])
     (unless (ormap (lambda (existing-edge)
                      (eq? in-label (existing-edge)))
                    existing-edges)
       (set! graph-edges (add1 graph-edges))
       (set-label-edges! out-label (cons new-edge existing-edges))
       (hash-table-for-each (label-set out-label)
                            (lambda (label in/out-edges)
                              (new-edge out-label label))))))
 
 ; edge label -> void
 ; We must be able to take care of all the following different cases:
 ; (define-values (x) a)
 ; (define-values (x) (values a))
 ; (define-values (x) (values (values a)))
 ; (define-values (x) (values (values (values a))))
 ; ...
 ; with all the call to "values" being possibly inside functions...
 ; So we define extend-edge-for-values that recursively unpacks nested "values" by adding new
 ; unpacking edges on the fly when a label-values flows into a label that has an unpacking edge.
 ; The unpacking edge is created as a wrapper around a simple label-to-label edge simple-edge that
 ; we use for direct propagation of non-values labels.
 ; This is used in processing all values related forms (define-values, let-values, etc...)
 ; Note that for values, we only ever wrap the in-edges, not the out-edges (i.e. the edges
 ; that point towards a subexpression, not towards a context).
 (define (extend-edge-for-values simple-edge)
   (case-lambda
    [(out-label inflowing-label)
     (if (label-values? inflowing-label)
         (let ([values-labels (label-values-labels inflowing-label)])
           (if (= (length values-labels) 1)
               ; we have something like (define-values (x) (... (values a) ...)), so we add a
               ; new direct edge from a to x. Of course this new edge has to be itself a recursive
               ; unpacking edge, since some (values b) could later flow into a. Note that, since
               ; our edges are independant of their origin, we can re-use the same simple edge.
               ; Watch then the nice infinitely-looking recursion. We are just creating a
               ; potentialy infinite number of unpacking edges, lazily. Also, since our edges
               ; are closures already containing the target label (the one for x),
               ; extend-edge-for-values doesn't need the target label as an explicit parameter.
               ; Only the origin label (the one corresponding to some use of "values") ever
               ; changes. This is just plain beautiful.
               (let ([new-origin-label (car values-labels)])
                 (add-edge-and-propagate-set-through-edge
                  new-origin-label
                  (extend-edge-for-values simple-edge)))
               ; (define-values (x) (... (values a b ...) ...))
               (set! *errors*
                     (cons (list (list (label-term inflowing-label))
                                 'red
                                 (format "context expected 1 value, received ~a values"
                                         (length values-labels)))
                           *errors*))))
         ; (define-values (x) a) or equivalent (e.g. the result of analysing something like
         ; (define-values (x) (values (values (values a)))), after three levels of recursion).
         (simple-edge out-label inflowing-label))]
    [() (simple-edge)]))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DERIVATION
 
 ; syntax-object -> label
 ; create simple, basic label. Used directly during graph reconstruction for primitives, since
 ; only the outer label will be associated with the term position, not all the internal labels.
 (define (create-simple-label term)
   (set! graph-nodes (add1 graph-nodes))
   (make-label #f #f #f #f term (make-hash-table) '()))

 ; create-simple-label is seldom used in the graph reconstruction from primitive type part
 ; of the code but used a lot in the graph derivation code, so rather than add a second
 ; argument to create-simple-label everywhere, it's easier to have this little specialized
 ; function for primitives...
 ; Note that such a label has prim? set to #t and that the associated term will, in practice,
 ; be the term into which the primitive label will initially flow.
 (define (create-simple-prim-label term)
   (make-label #f #f #f #t term (make-hash-table) '()))
 
 ; syntax-object -> label
 ; create simple, basic label, with term position associated with the label.
 ; Used for program variables (bindings and uses), expression result edges, things like that.
 (define (create-simple-pos-associated-label term)
   (let ([label (create-simple-label term)])
     (associate-label-with-term-position label term)
     label))
 
 ; label -> void
 ; put a label in it's own set, for terms that are value sources
 (define (initialize-label-set-for-value-source label)
   (hash-table-put! (label-set label) label (cons '() '())))
 
 ; syntax-object (listof (cons symbol label)) label -> label
 ; gamma is the binding-variable-name-to-label environment
 ; enclosing-lambda-label is the label for the enclosing lambda, if any. We
 ; need it to update its list of free variables if we find any. This means
 ; we have to create the label for a lambda before analyzing its body...
 (define (create-label-from-term term gamma enclosing-lambda-label)
   ; XXX perf analysis
   (set! ast-nodes (add1 ast-nodes))
   (kern:kernel-syntax-case
    term #f
    ; lambda and case-lambda are currently both core forms. This might change (dixit Matthew)
    [(lambda . clause)
     (create-label-from-term (datum->syntax-object
                              term
                              `(case-lambda . (,(syntax clause)))
                              term term)
                             gamma enclosing-lambda-label)]
    [(case-lambda . ((args exps ...) ...))
     (let* (; scheme lists of syntax object lists of syntax objects
            [argss (syntax-e (syntax (args ...)))]
            [expss (syntax-e (syntax ((exps ...) ...)))]
            [label (make-label-case-lambda
                    #f #f #f #f
                    term
                    (make-hash-table)
                    '()
                    *dummy*
                    *dummy*
                    *dummy*
                    *dummy*
                    '()
                    '())]
            [all-labels
             (list:foldr
              (lambda (args exps other-clauses-labels)
                (let ([rest-arg?s (vector-ref other-clauses-labels 0)]
                      [req-args (vector-ref other-clauses-labels 1)]
                      [argss-labels (vector-ref other-clauses-labels 2)]
                      [exps-labels (vector-ref other-clauses-labels 3)]
                      ; scheme list of syntax objects for body exps
                      [exps (syntax-e exps)])
                  ; we add one new element to each list each time we process a new clause,
                  ; so that the element for the current clause is always at the start of the
                  ; list, so we know where to find this element when we need it (we need to
                  ; update the top free vars for the current clause in the #%top case, and
                  ; the application thunk for the current clause in the #%app case).
                  (set-label-case-lambda-top-free-varss!
                   label
                   (cons '() (label-case-lambda-top-free-varss label)))
                  (set-label-case-lambda-app-thunks!
                   label
                   (cons *dummy-thunk* (label-case-lambda-app-thunks label)))
                  (kern:kernel-syntax-case
                   args #f
                   [(args ...)
                    (let* (; proper scheme list of syntax objects for arguments
                           [args (syntax-e (syntax (args ...)))]
                           [args-labels (map create-simple-pos-associated-label args)]
                           [gamma-extended (extend-env gamma args args-labels)])
                      (vector (cons #f rest-arg?s)
                              (cons (length args) req-args)
                              (cons args-labels argss-labels)
                              (cons (list:foldl
                                     (lambda (exp _)
                                       (create-label-from-term exp gamma-extended label))
                                     *dummy*
                                     exps)
                                    exps-labels)))]
                   [(first-arg . other-args-including-rest-arg)
                    (let* (; (syntax other-args-including-rest-arg) is either a (syntax
                           ; version of a) list of syntax objects (if there's strictly more
                           ; than one required argument), or a single syntax object (if
                           ; there's only one required argument). In both cases we want to
                           ; construct an improper list of syntax objects. syntax-e takes
                           ; care of that in the list case, cons takes care of that in the
                           ; other case.
                           [args (cons (syntax first-arg)
                                       (let* ([syntax-obj (syntax other-args-including-rest-arg)]
                                              [symbol-or-list-of-syntax-obj (syntax-e syntax-obj)])
                                         (if (symbol? symbol-or-list-of-syntax-obj)
                                             syntax-obj
                                             symbol-or-list-of-syntax-obj)))]
                           ; convert the improper list into a proper one.
                           [args (let loop ([args args])
                                   (if (pair? args)
                                       (cons (car args)
                                             (loop (cdr args)))
                                       (list args)))]
                           [args-labels (map create-simple-pos-associated-label args)]
                           [gamma-extended (extend-env gamma args args-labels)])
                      (vector (cons #t rest-arg?s)
                              (cons (sub1 (length args)) req-args)
                              (cons args-labels argss-labels)
                              (cons (list:foldl
                                     (lambda (exp _)
                                       (create-label-from-term exp gamma-extended label))
                                     *dummy*
                                     exps)
                                    exps-labels)))]
                   [rest-arg
                    (let* (; one syntax object for rest-arg
                           [rest-arg (syntax rest-arg)]
                           [rest-arg-label-list (list (create-simple-pos-associated-label rest-arg))]
                           [gamma-extended (extend-env gamma (list rest-arg) rest-arg-label-list)])
                      (vector (cons #t rest-arg?s)
                              (cons 0 req-args)
                              (cons rest-arg-label-list argss-labels)
                              (cons (list:foldl
                                     (lambda (exp _)
                                       (create-label-from-term exp gamma-extended label))
                                     *dummy*
                                     exps)
                                    exps-labels)))]
                   )))
              (vector '()'()'()'())
              argss
              expss
              )])
       (set! graph-nodes (add1 graph-nodes))
       (set-label-case-lambda-rest-arg?s! label (vector-ref all-labels 0))
       (set-label-case-lambda-req-args! label (vector-ref all-labels 1))
       (set-label-case-lambda-argss! label (vector-ref all-labels 2))
       (set-label-case-lambda-exps! label (vector-ref all-labels 3))
       (initialize-label-set-for-value-source label)
       (associate-label-with-term-position label term)
       label)]
    [(#%app op actual-args ...)
     (let*
         ([app-label (create-simple-pos-associated-label term)]
          [op-term (syntax op)]
          [op-label (create-label-from-term op-term gamma enclosing-lambda-label)]
          [actual-args-labels
           (map (lambda (actual-arg)
                  (create-label-from-term actual-arg gamma enclosing-lambda-label))
                (syntax-e (syntax (actual-args ...))))]
          [actual-args-length (length actual-args-labels)]
          [edge
           (let ([edge-fake-destination (gensym)])
             (case-lambda
              [(out-label inflowing-label)
               ; inflowing-label doesn't go anywhere, it's components are just connected to
               ; the rest of the graph, so out-label (which will be the op-label from which
               ; the case-lambda label is flowing out) is not used. I.e. op-label (out-label)
               ; is a sink for functions.
               (if (label-case-lambda? inflowing-label)
                   ; loop on clauses, looking for arity match
                   (let clauses-loop ([rest-arg?s
                                       (label-case-lambda-rest-arg?s inflowing-label)]
                                      [req-args
                                       (label-case-lambda-req-args inflowing-label)]
                                      [formal-argss-labels
                                       (label-case-lambda-argss inflowing-label)]
                                      [body-exps-label
                                       (label-case-lambda-exps inflowing-label)]
                                      [top-free-varss-labels
                                       (label-case-lambda-top-free-varss inflowing-label)]
                                      [app-thunks
                                       (label-case-lambda-app-thunks inflowing-label)]) 
                     (if (null? rest-arg?s)
                         ; Note: nothing was done, so there's nothing to undo
                         (set! *errors*
                               (cons (list
                                      (list op-term)
                                      'red
                                      (format "procedure application: arity mismatch, given: ~a; arguments were: ~a"
                                              (if (label-prim? inflowing-label)
                                                  ; this won't work if we use a primitive
                                                  ; in a higer-order way, but they can always
                                                  ; trace the case-lambda back, so that should
                                                  ; be good enough.
                                                  (syntax-object->datum op-term)
                                                  (syntax-object->datum
                                                   (label-term inflowing-label)))
                                              (syntax-object->datum (syntax (actual-args ...)))))
                                     *errors*))
                         (let ([rest-arg? (car rest-arg?s)]
                               [req-arg (car req-args)])
                           (if (or (and (not rest-arg?) (= actual-args-length req-arg))
                                   (and rest-arg? (>= actual-args-length req-arg)))
                               ; arity match
                               ; first, take care of all the top level free variables in the
                               ; clause.
                               ; Note that we make sure that all free variables are bound before
                               ; creating the edges, and we check all free variables even if we
                               ; already know some of them are unbound.
                               ; Note also that a free variable can not be captured by a lexical
                               ; binding, it has to be a top level binding.
                               (let* ([free-vars-labels (car top-free-varss-labels)]
                                      [binding-labels
                                       (map
                                        (lambda (free-var-label)
                                          (let* ([free-var-name
                                                  (syntax-e (label-term free-var-label))]
                                                 [binding-label
                                                  (lookup-top-level-name free-var-name)])
                                            (unless binding-label
                                              (set! *errors*
                                                    (cons
                                                     (list
                                                      (list (label-term free-var-label) op-term)
                                                      'red
                                                      (format "reference to undefined identifier: ~a in function ~a"
                                                              free-var-name
                                                              (syntax-object->datum op-term)))
                                                    *errors*)))
                                            binding-label))
                                        free-vars-labels)])
                                 ; we don't expect to have many free variables, so processing
                                 ; free-vars-labels once and binding-labels twice should be ok.
                                 (when (andmap (lambda (x) x) binding-labels)
                                   (for-each
                                    (lambda (binding-label free-var-label)
                                      (add-edge-and-propagate-set-through-edge
                                       binding-label
                                       (extend-edge-for-values
                                        (create-simple-edge free-var-label))))
                                    binding-labels
                                    free-vars-labels)
                                   ; then, make the applications inside this clause of
                                   ; inflowing-label flow (only once).
                                   ((car app-thunks))
                                   (set-car! app-thunks *dummy-thunk*)
                                   ; args edges (including special processing for rest arg, if any)
                                   ; don't do all that stuff if there's no arg at all, because of
                                   ; the cdr just below (which in turn is needed because we want
                                   ; to treat the last arg differently when it's a rest arg)
                                   (when (or rest-arg? (> req-arg 0))
                                     (let args-loop ([formal-args-labels (car formal-argss-labels)]
                                                     [actual-args-labels actual-args-labels])
                                       (if (null? (cdr formal-args-labels))
                                           (if rest-arg?
                                               ; create list for all the remaining actual args,
                                               ; and make the list flow into the rest arg
                                               (let*
                                                   ([rest-arg-label (car formal-args-labels)]
                                                    [rest-arg-term (label-term rest-arg-label)]
                                                    [actual-args-label-labellist
                                                     (let rest-loop ([actual-args-labels
                                                                      actual-args-labels])
                                                       (if (null? actual-args-labels)
                                                           (let ([null-label
                                                                  (make-label-cst
                                                                   #f #f #f #f
                                                                   rest-arg-term
                                                                   (make-hash-table)
                                                                   '()
                                                                   '())])
                                                             (set! graph-nodes (add1 graph-nodes))
                                                             (initialize-label-set-for-value-source
                                                              null-label)
                                                             ;(associate-label-with-term-position
                                                             ; null-label rest-arg-term)
                                                             null-label)
                                                           (let ([cons-label
                                                                  (make-label-cons
                                                                   #f #f #f #f
                                                                   rest-arg-term
                                                                   (make-hash-table)
                                                                   '()
                                                                   (car actual-args-labels)
                                                                   (rest-loop
                                                                    (cdr actual-args-labels)))])
                                                             (set! graph-nodes (add1 graph-nodes))
                                                             (initialize-label-set-for-value-source
                                                              cons-label)
                                                             ;(associate-label-with-term-position
                                                             ; cons-label rest-arg-term)
                                                             cons-label)))])
                                                 (add-edge-and-propagate-set-through-edge
                                                  actual-args-label-labellist
                                                  (extend-edge-for-values
                                                   (create-simple-edge rest-arg-label))))
                                               ; normal last arg
                                               (let ([formal-arg-label (car formal-args-labels)]
                                                     [actual-arg-label (car actual-args-labels)])
                                                 (add-edge-and-propagate-set-through-edge
                                                  actual-arg-label
                                                  (extend-edge-for-values
                                                   (create-simple-edge formal-arg-label)))))
                                           ; all formal args except last one
                                           (begin
                                             (let ([formal-arg-label (car formal-args-labels)]
                                                   [actual-arg-label (car actual-args-labels)])
                                               (add-edge-and-propagate-set-through-edge
                                                actual-arg-label
                                                (extend-edge-for-values
                                                 (create-simple-edge formal-arg-label))))
                                             (args-loop (cdr formal-args-labels)
                                                        (cdr actual-args-labels)))))
                                     )
                                   ; edge from body of clause to app term itself
                                   ; note that we do not detect multiple values here
                                   (let ([body-exp-label (car body-exps-label)])
                                     (add-edge-and-propagate-set-through-edge
                                      body-exp-label
                                      (create-simple-edge app-label)))
                                   ; flow sensitivity here ?
                                   ))
                               ; no arity match for this clause, keep looking
                               (clauses-loop (cdr rest-arg?s) (cdr req-args)
                                             (cdr formal-argss-labels) (cdr body-exps-label)
                                             (cdr top-free-varss-labels) (cdr app-thunks))))))
                   ; trying to apply something not a function
                   ; Note: nothing was done, so there's nothing to undo
                   (set! *errors*
                         (cons (list (list op-term)
                                     'red
                                     (format "procedure application: expected procedure, given: ~a; arguments were: ~a"
                                             (syntax-object->datum (label-term inflowing-label))
                                             (syntax-object->datum (syntax (actual-args ...)))))
                               *errors*)))]
              ; function value sink => unique, fake destination
              [() edge-fake-destination]))])
       ; If the app is inside a lambda, we delay the addition of the edge until the enclosing
       ; lambda is itself applied.
       (if enclosing-lambda-label
           (let* ([enclosing-lambda-app-thunks
                   (label-case-lambda-app-thunks enclosing-lambda-label)]
                  ; has to be evaluated now, not inside the thunk, otherwise we might have an
                  ; infinite loop (if there's only one clause in the lambda) or complete
                  ; non-sense (if there's several clauses).
                  [current-thunk (car enclosing-lambda-app-thunks)])
             (set-car! enclosing-lambda-app-thunks
                       (lambda ()
                         (add-edge-and-propagate-set-through-edge op-label edge)
                         (current-thunk))))
           (add-edge-and-propagate-set-through-edge op-label edge))
       app-label)]
    [(#%datum . datum)
     (let ([label (make-label-cst
                   #f #f #f #f
                   term
                   (make-hash-table)
                   '()
                   (syntax-object->datum (syntax datum)))])
       (set! graph-nodes (add1 graph-nodes))
       (initialize-label-set-for-value-source label)
       (associate-label-with-term-position label term)
       label)]
    [(quote sexp)
     (let ([label (make-label-cst
                   #f #f #f #f
                   term
                   (make-hash-table)
                   '()
                   ; syntax-e is not enough because of quasiquote
                   (syntax-object->datum (syntax sexp)))])
       (set! graph-nodes (add1 graph-nodes))
       (initialize-label-set-for-value-source label)
       (associate-label-with-term-position label term)
       label)]
    [(define-values vars exp)
     (let* (; scheme list of syntax objects
            [vars (syntax-e (syntax vars))]
            [vars-length (length vars)]
            [vars-labels (map add-top-level-name vars)]
            [exp-label (create-label-from-term (syntax exp) gamma enclosing-lambda-label)]
            [define-label (make-label-cst
                           #f #f #f #f
                           term
                           (make-hash-table)
                           '()
                           (void))])
       (set! graph-nodes (add1 graph-nodes))
       ; We must be able to take care of all the following different cases:
       ; (define-values (x) a)
       ; (define-values (x) (values a))
       ; (define-values (x) (values (values a)))
       ; (define-values (x) (values (values (values a))))
       ; ...
       ; (define-values (x y) (values a b))
       ; (define-values (x y) (values (values a) (values b)))
       ; (define-values (x y) (values (values (values a)) (values (values b))))
       ; ...
       ; with all the call to "values" being possibly inside functions...
       ; So we use extend-edge-for-values that recursively unpacks nested "values" by adding
       ; new unpacking edges on the fly when a label-values flows into a label that has an
       ; unpacking edge.
       ; Note that when define-values defines more than one variable, we must first unpack
       ; the top level of "values", then start the recursion for each variable separately.
       (if (= vars-length 1)
           ; we have something like (define-values (x) (values (values (values a)))) so we
           ; can directly start the recursion.
           (let ([var-label (car vars-labels)])
             (add-edge-and-propagate-set-through-edge
              exp-label
              (extend-edge-for-values (create-simple-edge var-label))))
           ; we have something like (define-values (x y) (values (values (values a))
           ; (values (values b)))) so we first have to manually unpack the top-most "values",
           ; then start a recursion for each of the defined variables. So in effect we end
           ; up doing something equivalent to analysing
           ; (define-values (x) (values (values a)))
           ; (define-values (y) (values (values b)))
           ; in parallel.
           (let ([distributive-unpacking-edge
                  (let ([edge-fake-destination (gensym)])
                    (case-lambda
                     [(out-label inflowing-label)
                      ; inflowing-label (the label corresponding to the top "values") doesn't
                      ; flow anywhere, it's just taken apart and its elements are connected to
                      ; the different variables. I.e. it's a sink for multiple values. So we
                      ; have no need for out-label here.
                      (if (label-values? inflowing-label)
                          (let ([values-labels (label-values-labels inflowing-label)])
                            (if (= (length values-labels) vars-length)
                                ; we have something like
                                ; (define-values (x y) (... (values a b) ...)), so we add a
                                ; new direct edge from a to x and b to y. Of course these new
                                ; edges have to be themselves recursive unpacking edges, since
                                ; some (values c) could later flow into either a or b.
                                (for-each
                                 (lambda (new-origin-label var-label)
                                   (add-edge-and-propagate-set-through-edge
                                    new-origin-label
                                    (extend-edge-for-values (create-simple-edge var-label))))
                                 values-labels vars-labels)
                                ; (define-values (x y) (... (values a b c) ...))
                                (set! *errors*
                                      (cons
                                       (list (list (label-term inflowing-label))
                                             'red
                                             (format "context expected ~a value, received ~a values"
                                                     vars-length (length values-labels)))
                                       *errors*))))
                          ; (define-values (x y) (... 1 ...))
                          (set! *errors*
                                (cons
                                 (list
                                  (list term)
                                  'red
                                  (format "define-values: context expected ~a values, received 1 non-multiple-values value"
                                          vars-length))
                                 *errors*)))]
                     ; multiple values sink => unique, fake destination
                     [() edge-fake-destination]))])
             (add-edge-and-propagate-set-through-edge
              exp-label
              distributive-unpacking-edge)))
       (initialize-label-set-for-value-source define-label)
       (associate-label-with-term-position define-label term)
       define-label)]
    [(let-values ((vars exp) ...) body-exps ...)
     (let* ([let-values-label (create-simple-pos-associated-label term)]
            [gamma-extended
             (list:foldl
              ; syntax-obj syntax-obj -> (listof (cons symbol label))
              ; loop on each binding clause of the let-values, returning the corresponding
              ; extended environment
              (lambda (vars exp new-gamma)
                (let* (; scheme list of syntax objects
                       [vars (syntax-e vars)]
                       [vars-length (length vars)]
                       [vars-labels (map create-simple-pos-associated-label vars)]
                       [exp-label (create-label-from-term exp gamma enclosing-lambda-label)])
                  ; We must be able to take care of all the following different cases:
                  ; (let-values ([(x) a] ...) ...)
                  ; (let-values ([(x) (values a)] ...) ...)
                  ; (let-values ([(x) (values (values a))] ...) ...)
                  ; (let-values ([(x) (values (values (values a)))] ...) ...)
                  ; ...
                  ; (let-values ([(x y) (values a b)] ...) ...)
                  ; (let-values ([(x y) (values (values a) (values b))] ...) ...)
                  ; (let-values ([(x y) (values (values (values a)) (values (values b)))] ...) ...)
                  ; ...
                  ; with all the call to "values" being possibly inside functions...
                  ; So we use extend-edge-for-values that recursively unpacks nested "values" by
                  ; adding new unpacking edges on the fly when a label-values flows into a label
                  ; that has an unpacking edge.
                  ; Note that when let-values defines more than one variable, we must first
                  ; unpack the top level of "values", then start the recursion for each
                  ; variable separately.
                  (if (= vars-length 1)
                      ; we have something like
                      ; (let-values ([(x) (values (values (values a)))]) ...) so we can
                      ; directly start the recursion.
                      (let ([var-label (car vars-labels)])
                        (add-edge-and-propagate-set-through-edge
                         exp-label
                         (extend-edge-for-values (create-simple-edge var-label))))
                      ; we have something like
                      ; (let-values ([(x y) (values (values (values a)) (values (values b)))] ...) ...)
                      ; so we first have to manually unpack the top-most "values", then start a
                      ; recursion for each of the defined variables. So in effect we end up
                      ; doing something equivalent to analysing
                      ; (let-values ([(x) (values (values a))]
                      ;               (y) (values (values b))] ...) ...)
                      ; in parallel.
                      (let ([distributive-unpacking-edge
                             (let ([edge-fake-destination (gensym)])
                               (case-lambda
                                [(out-label inflowing-label)
                                 ; inflowing-label (the label corresponding to the top "values")
                                 ; doesn't flow anywhere, it's just taken apart and its elements
                                 ; are connected to the different variables. I.e. it's a sink for
                                 ; multiple values. So we have no need for out-label here.
                                 (if (label-values? inflowing-label)
                                     (let ([values-labels (label-values-labels inflowing-label)])
                                       (if (= (length values-labels) vars-length)
                                           ; we have something like
                                           ; (let-values ([(x y) (... (values a b) ...)]...) ...),
                                           ; so we add a new direct edge from a to x and b to y.
                                           ; Of course these new edges have to be themselves
                                           ; recursive unpacking edges, since some (values c)
                                           ; could later flow into either a or b.
                                           (for-each
                                            (lambda (new-origin-label var-label)
                                              (add-edge-and-propagate-set-through-edge
                                               new-origin-label
                                               (extend-edge-for-values
                                                (create-simple-edge var-label))))
                                            values-labels vars-labels)
                                           ; (let-values ([(x y) (... (values a b c ...) ...)]
                                           ;             ...) ...)
                                           (set! *errors*
                                                 (cons
                                                  (list
                                                   (list (label-term inflowing-label))
                                                   'red
                                                   (format "context expected ~a value, received ~a values"
                                                           vars-length (length values-labels)))
                                                  *errors*))))
                                     ; (let-values ([(x y) (... 1 ...)] ...) ...)
                                     (set! *errors*
                                           (cons
                                            (list
                                             (list term)
                                             'red
                                             (format "let-values: context expected ~a values, received 1 non-multiple-values value"
                                                     vars-length))
                                            *errors*)))]
                                ; multiple values sink
                                [() edge-fake-destination]))])
                        (add-edge-and-propagate-set-through-edge
                         exp-label
                         distributive-unpacking-edge)))
                  (extend-env new-gamma vars vars-labels)))
              gamma
              ; Scheme lists of syntax objects, one for each list of vars and one for each exp
              (syntax-e (syntax (vars ...)))
              (syntax-e (syntax (exp ...))))]
            [last-body-exp-label
             (list:foldl
              (lambda (exp _)
                (create-label-from-term exp gamma-extended enclosing-lambda-label))
              *dummy*
              (syntax-e (syntax (body-exps ...))))])
       (add-edge-and-propagate-set-through-edge
        last-body-exp-label
        (create-simple-edge let-values-label))
       let-values-label)]
    [(if test then else)
     (let* ([test-label (create-label-from-term (syntax test) gamma enclosing-lambda-label)]
            [then-label (create-label-from-term (syntax then) gamma enclosing-lambda-label)]
            [else-label (create-label-from-term (syntax else) gamma enclosing-lambda-label)]
            ; because of the (if test then) case below, else-label might be associated with
            ; the same position as the whole term, so we have to create the if-label after
            ; the else-label, so that the wrong label/position association created by the
            ; else-label is overwritten.
            [if-label (create-simple-pos-associated-label term)]
            ; our edges are origin-independant, so we can use the same one for both then and else.
            [if-edge (create-simple-edge if-label)]
            [test-edge
             (letrec ([edge-fake-destination (gensym)]
                      [dummy-edge (case-lambda
                                   [(out-label inflowing-label)
                                    ; sink edge, so no need for out-label
                                    *dummy*]
                                   ; test value sink
                                   [() edge-fake-destination])]
                      [self-modifying-edge
                       (case-lambda
                        [(out-label inflowing-label)
                         ; sink edge, so no need for out-label
                         (if (and (label-cst? inflowing-label)
                                  (not (label-cst-value inflowing-label)))
                             (begin
                               (add-edge-and-propagate-set-through-edge
                                else-label if-edge)
                               (set! self-modifying-edge
                                     (case-lambda
                                      [(out-label inflowing-label)
                                       ; sink edge, so no need for out-label
                                       (when (or (not (label-cst? inflowing-label))
                                                 (label-cst-value inflowing-label))
                                         (add-edge-and-propagate-set-through-edge
                                          then-label if-edge)
                                         ; it would be more efficient to directly remove the edge.
                                         (set! self-modifying-edge dummy-edge))]
                                      ; test value sink
                                      [() edge-fake-destination])))
                             (begin
                               (add-edge-and-propagate-set-through-edge
                                then-label if-edge)
                               (set! self-modifying-edge
                                     (case-lambda
                                      [(out-label inflowing-label)
                                       ; sink edge, so no need for out-label
                                       (when (and (label-cst? inflowing-label)
                                                  (not (label-cst-value inflowing-label)))
                                         (add-edge-and-propagate-set-through-edge
                                          else-label if-edge)
                                         ; it would be more efficient to directly remove the edge.
                                         (set! self-modifying-edge dummy-edge))]
                                      ; test value sink
                                      [() edge-fake-destination]))))]
                        ; test value sink
                        [() edge-fake-destination])])
               self-modifying-edge)])
       (add-edge-and-propagate-set-through-edge test-label test-edge)
       if-label)]
    [(if test then)
     (create-label-from-term (datum->syntax-object
                              term
                              (list 'if (syntax test) (syntax then) '(#%app (#%top . void)))
                              term term)
                             gamma enclosing-lambda-label)]
    [(begin exps ...)
     (let ([begin-label (create-simple-pos-associated-label term)]
           [last-body-exp-label (list:foldl
                                 (lambda (exp _)
                                   (create-label-from-term exp gamma enclosing-lambda-label))
                                 *dummy*
                                 (syntax-e (syntax (exps ...))))])
       (add-edge-and-propagate-set-through-edge
        last-body-exp-label
        (create-simple-edge begin-label))
       begin-label)]
    [(#%top . identifier)
     (let* ([identifier (syntax identifier)]
            [identifier-name (syntax-e identifier)]
            ; note that bound-label doesn't contain the #%top, but they have the same
            ; syntax source/line/column/position, so arrows and underlining will work
            ; the same, but it will make things a little bit simpler when doing a
            ; lookup-top-level-name in the #%app case (if we have to).
            [bound-label (create-simple-pos-associated-label identifier)])
       (if enclosing-lambda-label
           ; free var inside a lambda, so add it to the list of free variables, don't do
           ; any lookup now (will be done when the enclosing lambda is applied)
           (let ([enclosing-lambda-top-free-varss
                  (label-case-lambda-top-free-varss enclosing-lambda-label)])
             (set-car! enclosing-lambda-top-free-varss
                       (cons bound-label (car enclosing-lambda-top-free-varss))))
           ; top level
           ; we do the top level lookup first, so we allow primitives to be redefined.
           (let ([binding-label (lookup-top-level-name identifier-name)])
             (if binding-label
                 (add-edge-and-propagate-set-through-edge
                  binding-label
                  (extend-edge-for-values (create-simple-edge bound-label)))
                 ; free var at top level XXX must be a primitive (explicit call to
                 ; primitive at top level)
                 (let ([primitive-type-scheme (lookup-primitive-type-scheme identifier-name)])
                   (if primitive-type-scheme
                       ; we allow a primitive like pi to evaluate to multiple values...
                       (add-edge-and-propagate-set-through-edge
                        (reconstruct-graph-from-type-scheme
                         primitive-type-scheme (make-hash-table) term)
                        (create-simple-edge bound-label))
                       (set! *errors*
                             (cons (list (list term)
                                         'red
                                         (format "reference to undefined identifier (at top level): ~a"
                                                 identifier-name))
                                   *errors*)))))))
       bound-label)]
    [var
     ; we cannot directly return the binding label, because, even though it makes for a
     ; simpler graph and simpler types, it screws up the arrows
     (let* ([bound-label (create-simple-pos-associated-label term)]
            [binding-label (lookup-env (syntax var) gamma)])
       (if binding-label
           (add-edge-and-propagate-set-through-edge
            binding-label
            (extend-edge-for-values (create-simple-edge bound-label)))
           ; no binding label at all. Either a construct we don't know how to analyze yet,
           ; or an implicit call to a primitive (call to cons as the result of using
           ; quasiquotes, stuff like that - don't ask me why  such primitive calls are
           ; not wrapped inside a #%top...)
           (set! *errors*
                 (cons (list (list term)
                             'red
                             (format "unknown construct: ~a" (syntax-object->datum (syntax var))))
                       *errors*)))
       bound-label)]
    ))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TYPES
 
 (define-struct type ())
 (define-struct (type-empty type) ())
 (define-struct (type-cst type) (type))
 (define-struct (type-cons type) (car cdr))
 (define-struct (type-case-lambda type) (rest-arg?s req-args argss exps))
 (define-struct (type-var type) (name recur))
 (define-struct (type-union type) (elements))
 (define-struct (type-rec type) (vars types body))
 (define-struct (type-values type) (types))
 (define-struct (type-flow-var type) (name))
 (define-struct (type-scheme type) (flow-vars type^Cs type))
 
 ; XXX
 (define *basic-types* '(top bottom number symbol string char void))
 (define *type-constructors* '(forall cons case-lambda))
 (define *all-type-keywords* (append *basic-types* *type-constructors*))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVE TYPE PARSER AND LOOKUP
 
 ; (listof (cons symbol type-scheme))
 (define *primitive-types-table* 'uninitialized)
 
 ; symbol -> (union type-scheme #f)
 (define (lookup-primitive-type-scheme name)
   (hash-table-get *primitive-types-table* name *hash-table-fail-false*))
     
 ; -> void
 (define (initialize-primitive-type-schemes)
   (let* ([filename (build-path (collection-path "mrflow") "primitives.ss")]
          [sexp (call-with-input-file filename read 'text)])
     (unless (list? sexp)
       (raise-syntax-error
        'initialize-primitive-type-schemes
        (format "expected list at top level in file ~a, got: ~a"
                filename sexp)))
     (for-each (lambda (prim-entry) 
                 (unless (and (list? prim-entry)
                              (= 2 (length prim-entry))
                              (symbol? (car prim-entry)))
                   (raise-syntax-error
                    'initialize-primitive-type-schemes
                    (format "expected `(,symbol type-scheme) entry in file ~a, got: ~a"
                            filename prim-entry))))
               sexp)
     (set! *primitive-types-table* (make-hash-table))
     (for-each (lambda (prim-entry)
                 (let ([primitive-name (car prim-entry)]
                       [primitive-type (cadr prim-entry)])
                   (when (hash-table-get *primitive-types-table*
                                         primitive-name *hash-table-fail-false*)
                     (raise-syntax-error
                      'initialize-primitive-type-schemes
                      (format "found duplicate for primitive ~a in file ~a"
                              primitive-name filename)))
                   (hash-table-put! *primitive-types-table* primitive-name
                                    (parse&check-type-scheme
                                     primitive-type primitive-name filename))))
               sexp)))
 
 ; sexp symbol tring -> type
 (define (parse&check-type-scheme sexp primitive-name filename)
   (if (and (list? sexp)
            (eq? (car sexp) 'forall))
       (if (= (length sexp) 3)
           (let ([delta (make-hash-table)]
                 [flow-vars&type^Cs (cadr sexp)]
                 [type (caddr sexp)])
             (for-each
              (lambda (flow-var&type^C)
                (if (and (list? flow-var&type^C)
                         (= (length flow-var&type^C) 2)
                         (symbol? (car flow-var&type^C)))
                    (let ([flow-var (car flow-var&type^C)]
                          [type^C (cadr flow-var&type^C)])
                      (when (memq flow-var *all-type-keywords*)
                        (raise-syntax-error
                         'parse&check-type-scheme
                         (format "flow variable ~a is already the name of a basic type or type constructor, in type scheme for primitive ~a in file ~a"
                                 flow-var primitive-name filename)))
                      (when (hash-table-get delta flow-var *hash-table-fail-false*)
                        (raise-syntax-error
                         'parse&check-type-scheme
                         (format "duplicate flow variable ~a in type scheme for primitive ~a in file ~a"
                                 flow-var primitive-name filename)))
                      (hash-table-put! delta
                                       flow-var
                                       (cons (cons #t #t)
                                             ; type^Cs do not contain flow vars, so we give an
                                             ; empty delta. If this function returns, we know
                                             ; the result is a constant type.
                                             (parse&check-type type^C
                                                               (make-hash-table)
                                                               #t
                                                               primitive-name filename))))
                    (raise-syntax-error
                     'parse&check-type-scheme
                     (format "malformed type scheme clause for primitive ~a in file ~a: expected (symbol type), got ~a"
                             primitive-name filename flow-var&type^C))))
              flow-vars&type^Cs)
             (let ([type (parse&check-type type delta #t primitive-name filename)])
               (hash-table-for-each
                delta
                (lambda (flow-var type-info)
                  (let ([no-contra-use (caar type-info)]
                        [no-co-use (cdar type-info)])
                    (cond
                      [(and no-contra-use no-co-use)
                       (raise-syntax-error
                        'parse&check-type-scheme
                        (format "unused flow variable ~a in type scheme for primitive ~a in file ~a"
                                flow-var primitive-name filename))]
                      [no-contra-use
                       (raise-syntax-error
                        'parse&check-type-scheme
                        (format "no contravariant in-flow for flow variable ~a in type scheme for primitive ~a in file ~a"
                                flow-var primitive-name filename))]
                      [no-co-use
                       (raise-syntax-error
                        'parse&check-type-scheme
                        (format "no covariant out-flow for flow variable ~a in type scheme for primitive ~a in file ~a"
                                flow-var primitive-name filename))]
                      [else #t]))))
               (if (null? flow-vars&type^Cs)
                   type
                   (make-type-scheme
                    (hash-table-map delta (lambda (flow-var type-info) flow-var))
                    (hash-table-map delta (lambda (flow-var type-info) (cdr type-info)))
                    type))))
           (raise-syntax-error 'parse&check-type-scheme
                               (format "malformed type scheme for primitive ~a in file ~a: expected (forall (flow-var-clause ...) type), got ~a"
                                       primitive-name filename sexp)))
       (parse&check-type sexp (make-hash-table) #t primitive-name filename)))

 ; sexp (hash-table-of symbol (cons (cons boolean boolean) type)) boolean symbol string -> type
 (define (parse&check-type sexp delta covariant? primitive-name filename)
   (if (list? sexp)
       (let ([type-kw (car sexp)])
         (cond
           [(eq? type-kw 'forall)
            (raise-syntax-error
             'parse&check-type
             (format "type scheme inside type or other type scheme primitive ~a in file ~a: ~a"
                     primitive-name filename sexp))]
           [(eq? type-kw 'case-lambda)
            (let ([all-types            
                   (list:foldr
                    (lambda (clause other-clauses-types)
                      (if (and (list? clause)
                               (= (length clause) 2))
                          (let* ([args (car clause)]
                                 [exp (cadr clause)]
                                 [exp-type (parse&check-type exp delta
                                                             covariant?
                                                             primitive-name filename)]
                                 [rest-arg?s (vector-ref other-clauses-types 0)]
                                 [req-args (vector-ref other-clauses-types 1)]
                                 [argss-typess (vector-ref other-clauses-types 2)]
                                 [exps-types (vector-ref other-clauses-types 3)])
                            (cond
                              [(list? args)
                               (if (and (pair? args) ; could be empty
                                        (memq (car args) *type-constructors*))
                                   ; we have a single rest arg that's a complex type.
                                   (vector (cons #t rest-arg?s)
                                           (cons 0 req-args)
                                           (cons (list (parse&check-type args delta
                                                                         (not covariant?)
                                                                         primitive-name filename))
                                                 argss-typess)
                                           (cons exp-type exps-types))
                                   ; normal (possibly empty) list of (possibly complex) args
                                   (vector (cons #f rest-arg?s)
                                           (cons (length args) req-args)
                                           (cons (map (lambda (arg)
                                                        (parse&check-type arg delta
                                                                          (not covariant?)
                                                                          primitive-name filename))
                                                      args)
                                                 argss-typess)
                                           (cons exp-type exps-types)))]
                              [(symbol? args)
                               ; single simple rest argument
                               (vector (cons #t rest-arg?s)
                                       (cons 0 req-args)
                                       (cons (parse&check-type args delta
                                                               (not covariant?)
                                                               primitive-name filename))
                                       (cons exp-type exps-types))]
                              [else
                               ; improper list with a rest argument, so convert to proper list
                               (let ([args (let loop ([args args])
                                             (if (pair? args)
                                                 (cons (car args)
                                                       (loop (cdr args)))
                                                 (list args)))])
                                 (vector (cons #t rest-arg?s)
                                         (cons (sub1 (length args)) req-args)
                                         (cons (map (lambda (arg)
                                                      (parse&check-type arg delta
                                                                        (not covariant?)
                                                                        primitive-name filename))
                                                    args)
                                               argss-typess)
                                         (cons exp-type exps-types)))]))
                          (raise-syntax-error
                           'parse&check-type
                           (format "malformed clause in case-lambda type in type scheme for primitive ~a in file ~a: expected (args-types exp-type), got ~a"
                                   primitive-name filename clause))))
                    (vector '()'()'()'())
                    (cdr sexp))])
              (make-type-case-lambda (vector-ref all-types 0)
                                     (vector-ref all-types 1)
                                     (vector-ref all-types 2)
                                     (vector-ref all-types 3)))]
           [(eq? type-kw 'cons)
            (if (= (length sexp) 3)
                (make-type-cons (parse&check-type (cadr sexp) delta covariant?
                                                  primitive-name filename)
                                (parse&check-type (caddr sexp) delta covariant?
                                                  primitive-name filename))
                (raise-syntax-error
                 'parse&check-type
                 (format "malformed cons type in type scheme primitive ~a in file ~a: ~a"
                         primitive-name filename sexp)))]
           [(eq? type-kw 'void)
            (if (= (length sexp) 1)
                (make-type-cst (void))
                (raise-syntax-error
                 'parse&check-type
                 (format "malformed void type in type scheme primitive ~a in file ~a: ~a"
                         primitive-name filename sexp)))]
           ))
       (if (pair? sexp)
           ; improper list
           (raise-syntax-error
            'parse&check-type
            (format "improper list found in type scheme primitive ~a in file ~a: ~a"
                    primitive-name filename sexp))
           (cond
             [(hash-table-get delta sexp *hash-table-fail-false*)
              =>
              (lambda (type-info)
                (if covariant?
                    (set-cdr! (car type-info) #f)
                    (if (caar type-info)
                        (set-car! (car type-info) #f)
                        ; already used this flow variable in contravariant position
                        (raise-syntax-error
                         'parse&check-type
                         (format "flow variable ~a used several times in contravariant position in type scheme for primitive ~a in file ~a"
                                 sexp primitive-name filename))))
                (make-type-flow-var sexp))]
             ; [(memq sexp *basic-types*)
             ;  (make-type-cst sexp)]
             ; the following works for both basic types and any atomic value (which is
             ; then considered a basic type too). We know that flow var names and basic
             ; type names are disjoint, so there's no confusion between this case and
             ; the previous one.
             [else (make-type-cst sexp)]))))
   
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POST ANALYSIS TYPE CHECKING FOR PRIMITIVES
 
 ; (make-hash-table-of label type)
 ; used for post-derivation/propagation type checking of primitives
 (define *label-types* 'uninitialized)
 
 ; label type -> void
 (define (associate-label-with-type label type)
   (hash-table-put! *label-types* label type))

 (define (subtype t1 t2)
   #t) ; XXX
 
 ; -> void
 ; post analysis checking of primitives inputs and outputs
 (define (check-primitive-types)
   (hash-table-for-each
    *label-types*
    (lambda (label expected-type)
      (let ([computed-type (get-type label)])
        (unless (subtype computed-type expected-type)
          (set! *errors*
                (cons (list (list (label-term label))
                            'red
                            (format "computed type ~a not a subtype of expected type ~a"
                                    (pp-type computed-type)
                                    (pp-type expected-type)))
                      *errors*)))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TYPE ENVIRONMENT & MISC
 
 (define (reset-type)
   (set! *label-types* (make-hash-table))
   )

 ; (hash-table-of symbol label) symbol label -> label
 (define (add-flow-var-to-env env name label)
   (hash-table-put! env name label)
   env)

 ; (hash-table-of symbol label) symbol -> label
 ; the type parser guarantees that the lookup will be succesfull
 (define (lookup-flow-var-in-env env name)
   (hash-table-get env name))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAPH RECONSTRUCTION FROM TYPE

 ; type (hash-table-of symbol label) term -> label
 ; analyse type scheme and creates flow var environment
 ; term is the term into which the final result will flow into. We need that
 ; mainly to report errors correctly.
 (define (reconstruct-graph-from-type-scheme type delta term)
   (if (type-scheme? type)
       (begin
         (for-each (lambda (flow-var type^C)
                     (let ([label (create-simple-prim-label term)])
                       (associate-label-with-type label type^C)
                       (add-flow-var-to-env delta flow-var label)))
                   (type-scheme-flow-vars type)
                   (type-scheme-type^Cs type))
         (reconstruct-graph-from-type (type-scheme-type type) delta term #t))
       (reconstruct-graph-from-type type delta term #t)))
 
 ; type (hash-table-of symbol label) term boolean -> label
 ; reconstructs a graph from type representing the primitive represented by term,
 ; using environment delta.
 ; Note how we use associate-label-with-type to memorize type checking only in the contravariant
 ; case. The type to check in the covariant case is always top, since we assume internal
 ; correctness of the graph generation from a primitive type.
 (define (reconstruct-graph-from-type type delta term covariant?)
   (if covariant?
       ; covariant cases
       (cond
         [(type-case-lambda? type)
          (let* ([all-labels
                  (list:foldr
                   (lambda (args-types exp-type other-clauses-labels)
                     (let ([argss-labelss (car other-clauses-labels)]
                           [exps-labels (cdr other-clauses-labels)])
                       (cons (cons (map (lambda (arg-type)
                                          (reconstruct-graph-from-type arg-type delta term #f))
                                        args-types)
                                   argss-labelss)
                             (cons (reconstruct-graph-from-type exp-type delta term #t)
                                   exps-labels))))
                   (cons '()'())
                   (type-case-lambda-argss type)
                   (type-case-lambda-exps type))]
                 [label (make-label-case-lambda
                         #f #f #f #t
                         term
                         (make-hash-table)
                         '()
                         (type-case-lambda-rest-arg?s type)
                         (type-case-lambda-req-args type)
                         (car all-labels)
                         (cdr all-labels)
                         (list null)
                         (list *dummy-thunk*))])
            (initialize-label-set-for-value-source label)
            label)]
         [(type-cons? type)
          (let ([label (make-label-cons
                        #f #f #f #t
                        term
                        (make-hash-table)
                        '()
                        (reconstruct-graph-from-type (type-cons-car type) delta term #t)
                        (reconstruct-graph-from-type (type-cons-cdr type) delta term #t))])
            (initialize-label-set-for-value-source label)
            label)]
         [(type-flow-var? type)
          (lookup-flow-var-in-env delta (type-flow-var-name type))]
         [(type-cst? type)
          (let ([label (make-label-cst
                        #f #f #f #t
                        term
                        (make-hash-table)
                        '()
                        ; the type parser ensures that type-cst is only created for
                        ; non-list (i.e. atomic) types => 3, 'foo, 'int
                        (type-cst-type type))])
            (initialize-label-set-for-value-source label)
            label)]
         [else (error 'reconstruct-graph-from-type "unknown covariant type: ~a" type)]
         )
       ; contravariant cases
       (cond
         [(type-cons? type)
          (let* ([car-label (reconstruct-graph-from-type (type-cons-car type) delta term #f)]
                 [car-edge (create-simple-edge car-label)]
                 [cdr-label (reconstruct-graph-from-type (type-cons-cdr type) delta term #f)]
                 [cdr-edge (create-simple-edge cdr-label)]
                 [cons-label (create-simple-prim-label term)]
                 [cons-edge
                  (let ([edge-fake-destination (gensym)])
                    (case-lambda
                     [(out-label inflowing-label)
                      ; cons sink => no use for out-label here
                      (if (label-cons? inflowing-label)
                          (begin
                            (add-edge-and-propagate-set-through-edge
                             (label-cons-car inflowing-label)
                             car-edge)
                            (add-edge-and-propagate-set-through-edge
                             (label-cons-cdr inflowing-label)
                             cdr-edge))
                          ; XXX should we do this here because we can, or in check-primitive-types
                          ; because that's where it should be done... ? We don't have access to
                          ; term anymore in check-primitive-types... See the commented call to
                          ; associate-label-with-type below.
                          (set! *errors*
                                (cons (list (list term)
                                            'red
                                            (format "primitive expects argument of type <pair>; given ~a"
                                                    (syntax-object->datum
                                                     (label-term inflowing-label))))
                                      *errors*)))]
                     ; cons sink
                     [() edge-fake-destination]))])
            ;(associate-label-with-type cons-label (make-type-cons
            ;                                       (make-type-cst 'top)
            ;                                       (make-type-cst 'top)))
            (add-edge-and-propagate-set-through-edge cons-label cons-edge)
            cons-label)]
         [(type-flow-var? type)
          ; associate-label-with-type has already been done when analyzing the type scheme
          (lookup-flow-var-in-env delta (type-flow-var-name type))]         
         [(type-cst? type)
          (let ([label (make-label-cst
                        #f #f #f #t
                        term
                        (make-hash-table)
                        '()
                        ; the type parser ensures that type-cst is only created for
                        ; non-list (i.e. atomic) types => 3, 'foo, 'int
                        (type-cst-type type))])
            (associate-label-with-type label type)
            label)]
         [else (error 'reconstruct-graph-from-type "unknown contravariant type: ~a" type)]
         )))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI INTERFACE
 
 ; (hash-table-of number label)
 ; need this for DrScheme, to find the label that
 ; corresponds to the term starting at the given position
 (define *position->label* 'uninitialized)
 
 ; syntax-object label -> void
 (define (associate-label-with-term-position label term)
   (hash-table-put! *position->label* (syntax-position term) label))
 
 ; number -> (union label #f)
 (define (lookup-label-from-position position)
   (hash-table-get *position->label* position *hash-table-fail-false*))
 
 (define type-var-counter 'uninitialized)
 (define (create-type-var-name)
   (set! type-var-counter (add1 type-var-counter))
   (string->symbol (string-append "a" (number->string type-var-counter))))
 
 ; -> void
 (define (reset-gui)
   (set! type-var-counter 0)
   (set! *position->label* (make-hash-table))
   )
 
 ; -> (listof (list positive-int positive-int value symbol))
 ; extracts locations of erroneous terms, to be flaged by DrScheme
 (define (get-prims)
   (letrec ([get-prims-l
             (lambda (l)
               (if (null? l)
                   '()
                   (let* ([current-error (car l)]
                          [terms (car current-error)]
                          ; mzscheme uses offset 1, drscheme offset 0
                          [terms-pos (map (lambda (term)
                                            ; XXX primitives used in quasiquotes don't 
                                            ; have a position ?
                                            (sub1 (syntax-position term)))
                                          terms)])
                     ; XXX create GUI for this
                     ; XXX printed in reverse order
                     (printf (string-append (caddr current-error) "~n"))
                     (append (map (lambda (term-pos term)
                                    (list term-pos
                                          (+ term-pos (syntax-span term) -1)
                                          (syntax-source term)
                                          (cadr current-error)))
                                  terms-pos terms)
                             (get-prims-l (cdr l))))))])
     (get-prims-l *errors*)))
 
 ; label -> positive-int
 ; returns start location of term associated with label
 (define (get-loc label)
   (syntax-position (label-term label)))
 
 ; number -> (union label #f)
 ; given a DrScheme offset, returns the label associated with the terms that start at that offset.
 (define get-var lookup-label-from-position)
 
 ; (make-hash-tableof label (cons type-var type))
 (define *rec-types* 'uninitialized)
 
 ; label -> type
 ; computes type for label and wraps a rec-type around if necessary
 (define (get-type label)
   (set! *rec-types* (make-hash-table))
   ; let* to enforce sequentiality
   (let* ([non-rec-type (get-non-rec-type label)]
          [rec-type-clauses (hash-table-map *rec-types*
                                            (lambda (label rec-type-clause-value)
                                              rec-type-clause-value))])
     (if (> (length rec-type-clauses) 0)
         (let ([all-rec-type-clauses
                ; (list (cons x y)) -> (cons (list x) (list y)) ...
                (list:foldr
                 (lambda (rec-type-clause other-rec-type-clauses)
                   (let ([rec-type-var (car rec-type-clause)]
                         [rec-type-type (cdr rec-type-clause)]
                         [other-rec-type-vars (car other-rec-type-clauses)]
                         [other-rec-type-types (cdr other-rec-type-clauses)])
                     (cons (cons rec-type-var other-rec-type-vars)
                           (cons rec-type-type other-rec-type-types))))
                 (cons '() '())
                 rec-type-clauses)])
           (make-type-rec (car all-rec-type-clauses)
                          (cdr all-rec-type-clauses)
                          non-rec-type))
         non-rec-type)))
 
 ; label -> type
 ; reconstructs the type for a label
 ; a lot of things are done imperatively: the trace is used to detect cycles in the graph,
 ; and recur in the type var is used to know when to add a rec-type to the list of rec-types
 ; to wrap around the final type (the list actually being done imperatively using a hash
 ; table, because doing it functionaly using merge-lists and by returning multiple values
 ; (both a type and a list of rec-types) becomes an horrible mess when you reach the
 ; case-lambda case because you have to merge everything at once from all the different args
 ; and bodies types)
 (define (get-non-rec-type label)
   (if (label-trace label)
       ; we have a recursive type
       (let ([type-var (label-type-var label)])
         (if type-var
             ; found some join point that was used during a previous recursion or a previous
             ; part of the same recursion => mark the join point and re-use the same variable name
             (begin
               (set-type-var-recur! type-var #t)
               type-var)
             ; never saw this join point before, so create a type variable for it.
             ; We set recur to #t to mark that we must create a rec-type when
             ; seeing this label again on the way up.
             (let ([type-var (make-type-var (create-type-var-name) #t)])
               (set-label-type-var! label type-var)
               type-var)))
       ; non-recursive type (yet), update trace and recursively compute type for label
       (begin
         (set-label-trace! label #t)
         (let* ([type-union-elements
                 (hash-table-map (label-set label)
                                 (lambda (label in/out-edges)
                                   (cond
                                     [(label-cst? label)
                                      (make-type-cst (label-cst-value label))]
                                     [(label-cons? label)
                                      (make-type-cons (get-non-rec-type (label-cons-car label))
                                                      (get-non-rec-type (label-cons-cdr label)))]
                                     [(label-case-lambda? label)
                                      (let ([all-types
                                             (list:foldr
                                              (lambda (args-labels exp-label other-clauses-types)
                                                (let ([argss-typess (car other-clauses-types)]
                                                      [exps-types (cdr other-clauses-types)])
                                                  (cons (cons (map get-non-rec-type args-labels)
                                                              argss-typess)
                                                        (cons (get-non-rec-type exp-label)
                                                              exps-types))))
                                              (cons '() '())
                                              (label-case-lambda-argss label)
                                              (label-case-lambda-exps label))])
                                        (make-type-case-lambda (label-case-lambda-rest-arg?s label)
                                                               #f ; does not matter here
                                                               (car all-types)
                                                               (cdr all-types)))]
                                     [(label-values? label)
                                      (make-type-values
                                       (map get-non-rec-type (label-values-labels label)))]
                                     [else (error 'get-non-rec-type "unknown label: ~a" label)])))]
                [type-union-elements-flattened
                 (let loop ([cur-types type-union-elements])
                   (if (null? cur-types)
                       '()
                       (let ([cur-type (car cur-types)])
                         (if (type-union? cur-type)
                             (append (type-union-elements cur-type)
                                     (loop (cdr cur-types)))
                             (cons cur-type
                                   (loop (cdr cur-types)))))))]
                [type-union-length (length type-union-elements-flattened)]
                [final-type (cond
                              [(= type-union-length 0) (make-type-empty)]
                              [(= type-union-length 1) (car type-union-elements-flattened)]
                              [else (make-type-union type-union-elements-flattened)])]
                ; now compute this one *after* the recursion, to see if it was modified
                [type-var (label-type-var label)])
           ; recursion is done, so we can reset the trace for this label
           (set-label-trace! label #f)
           ; now check whether we have to create a new rec-type or not
           (if (and (type-var? type-var) (type-var-recur type-var))
               ; join point found on the way up in the recursion
               ; We don't touch the variable name in the label, so we'll re-use the same name
               ; in another future recursion, if any; we clear the mark, add a rec-type to our
               ; hash table of rec-types, and just return the type variable.
               (begin
                 (set-type-var-recur! type-var #f)
                 (unless (hash-table-get *rec-types* label *hash-table-fail-false*)
                   (hash-table-put! *rec-types* label (cons type-var final-type)))
                 type-var)
               ;                 (make-type-rec (list type-var)
               ;                                (list final-type)
               ;                                type-var))
               ; note: we can't memoize here, because this part of the graph might be part
               ; of a cycle that we are currently analyzing, and if we memoize the result and
               ; this part of the graph happens to be also part of another cycle, we'll have
               ; the type variable for the first cycle appear in the type for the second
               ; cycle, without any corresponding rec-type.
               final-type)))))
 
 ; (define get-type get-non-rec-type)
 
 ; type -> string
 ; type pretty printer
 (define (pp-type type)
   (cond
     [(type-empty? type) "_"]
     [(type-cst? type)
      ; can be a complex sexp if (quote sexp) is in the input
      (string:expr->string (type-cst-type type))]
     ;      (let ([val (type-cst-type type)])
     ;        (cond
     ;          [(number? val) (number->string val)]
     ;          [(symbol? val) (symbol->string val)]
     ;          [(string? val) (string-append "\"" val "\"")]
     ;          [(void? val) "void"]
     ;          [else (error 'pp-type "unknown datum: ~a" val)]))]
     [(type-cons? type)
      (string-append "(cons " (pp-type (type-cons-car type)) " "
                     (pp-type (type-cons-cdr type)) ")")]
     [(type-case-lambda? type)
      (string-append
       "(case-lambda "
       (list:foldr
        (lambda (rest-arg? formal-args-types body-exp-type str)
          (string-append
           "["
           (list:foldr
            (lambda (formal-arg-type str)
              (string-append
               (pp-type formal-arg-type)
               " "
               str))
            ""
            formal-args-types)
           (if rest-arg?
               "*-> "
               "-> ")
           (pp-type body-exp-type)
           "]"
           (if (string=? str "")
               ""
               " ")
           str))
        ""
        (type-case-lambda-rest-arg?s type)
        (type-case-lambda-argss type)
        (type-case-lambda-exps type))
       ")")]
     [(type-var? type)
      (symbol->string (type-var-name type))]
     [(type-union? type)
      (string-append
       "(union "
       (list:foldr
        (lambda (union-element str)
          (string-append
           (pp-type union-element)
           (if (string=? str ")")
               ""
               " ")
           str))
        ")"
        (type-union-elements type)))]
     [(type-values? type)
      (string-append
       "(values "
       (list:foldr
        (lambda (union-element str)
          (string-append
           (pp-type union-element)
           (if (string=? str ")")
               ""
               " ")
           str))
        ")"
        (type-values-types type)))]
     [(type-rec? type)
      (string-append
       "(rec-type ("
       (list:foldr
        (lambda (var type str)
          (string-append
           "["
           (symbol->string (type-var-name var))
           " "
           (pp-type type)
           (if (string=? str ") ")
               "]"
               "] ")
           str))
        ") "
        (type-rec-vars type)
        (type-rec-types type))
       (pp-type (type-rec-body type))
       ")")]
     [else (error 'pp-type "unknown type: ~a" type)]))
 
 ; (listof top) (listof top) -> (listof top)
 ; This is O(n^2) but we expect the lists to be small, otherwise use a hash table... It's only
 ; used in the GUI part anyway.
 ; Note that neither l1 nor l2 contains duplicates, because of the test in create-simple-edge
 (define (merge-lists l1 l2)
   (cond
     [(null? l1) l2]
     [else (let ([elt-l1 (car l1)])
             (if (memq elt-l1 l2)
                 (merge-lists (cdr l1) l2)
                 (cons elt-l1 (merge-lists (cdr l1) l2))))]))
 
 ; ((cons (listof label) (listof label)) -> (listof label)) -> (label -> (listof label))
 ; returns list of labels from which label went in or out, depending on selector
 (define (get-parents/children selector)
   (lambda (label)
     (list:foldr
      (lambda (edges edgess)
        (let ([edges (list:filter (lambda (label)
                                    (not (label-prim? label)))
                                  edges)])
          (merge-lists edges edgess)))
      '()
      (hash-table-map (label-set label)
                      (lambda (label in/out-edges)
                        (selector in/out-edges))))))
 
 ; label -> (listof label)
 (define parents (get-parents/children car))
 
 ; label -> (listof label)
 (define children (get-parents/children cdr))
 
 ; XXX fix this
 (define (has-member? type sym)
   #t)
 
 ; XXX debug
 (define (show-expanded label)
   (string:expr->string (syntax-object->datum (label-term label))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DRIVER
 
 ; ; port value -> void
 ; (define (sba-driver port source)
 ;   (reset-all)
 ;   (let ([start (current-milliseconds)])
 ;     (read-and-analyze port source)
 ;     (printf "time: ~a ms~n" (- (current-milliseconds) start)))
 ;   
 ;   ; XXX perf analysis
 ;   (printf "ast-nodes: ~a  graph-nodes: ~a  graph-edges: ~a~n" ast-nodes graph-nodes graph-edges)
 ;   )
 
 ; -> void
 (define (reset-all)
   (reset-derivation)
   (reset-type)
   (reset-gui)
   (reset-perf)
   )
 
 ; ; port value -> void
 ; ; read and analyze, one syntax object at a time
 ; (define (read-and-analyze port source)
 ;   (let ([stx-obj (read-syntax source port)])
 ;     (unless (eof-object? stx-obj)
 ;       (begin (printf "sba-driver in: ~a~n" (syntax-object->datum stx-obj))
 ;              (printf "sba-driver analyzed: ~a~n~n" (syntax-object->datum (expand stx-obj)))
 ;              (printf "sba-driver out: ~a~n~n" (create-label-from-term (expand stx-obj) '() #f)))
 ;       (read-and-analyze port source))))
 ;;     (if (eof-object? stx-obj)
 ;;         '()
 ;;         (cons (create-label-from-term (expand stx-obj) '() #f)
 ;;               (read-and-analyze port source)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERFORMANCE TEST
 
 ; ; (: test-i (nothing -> void))
 ; ; parse expression interactively
 ; (define (test-i)
 ;   (sba-driver (current-input-port) 'interactive))
 ;
 ; ; (: test-f (string -> (listof Ast)))
 ; (define (test-f filename)
 ;   (let ([port (open-input-file filename)])
 ;     (sba-driver port filename)
 ;     (close-input-port port)))
 ; 
 ; (let* ([path (build-path (collection-path "mrflow") "tests")]
 ;        [files (list:filter (lambda (file)
 ;                              (and (> (string-length file) 3)
 ;                                   (string=? "test-real"
 ;                                             (substring file 0 9))
 ;                                   (string=? "test-realbig"
 ;                                             (substring file 0 12))))
 ;                            (list:quicksort 
 ;                             (directory-list path)
 ;                             string<=?)
 ;                            )]
 ;	)
 ;   (for-each (lambda (file)
 ;               (printf "~a: " file)
 ;               (test-f (build-path path file))
 ;               ;		  (test-f file)
 ;               )
 ;             files))
 
 ) ; end module constraints-gen-and-prop
