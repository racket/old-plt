;; FFI Scheme interface

(module foreign mzscheme

(require #%foreign)
(require-for-syntax (lib "stx.ss" "syntax"))

(provide ctype-sizeof ctype-alignof malloc end-stubborn-change
         cpointer? ptr-ref ptr-set! ptr-equal?
         ctype? make-ctype make-cstruct-type register-finalizer
         make-sized-byte-string)
(provide _void _int8 _uint8 _int16 _uint16 _int32 _uint32 _int64 _uint64
         _byte _word _int _uint _fixint _ufixint _long _ulong _fixnum _ufixnum
         _float _double
         _bool _pointer _scheme)

(provide define*)
(define-syntax define*
  (syntax-rules ()
    [(_ (name . args) body ...)
     (begin (provide name) (define (name . args) body ...))]
    [(_ name expr)
     (begin (provide name) (define name expr))]))

;; ----------------------------------------------------------------------------
;; Getting and setting library objects

(define lib-suffix
  (case (system-type)
    [(unix)    "so"]
    [(macosx)  "dylib"]
    [(windows) "dll"]
    [else (error 'foreign "unknown system type: ~s" (system-type))]))

(provide (rename get-ffi-lib ffi-lib))
(define (get-ffi-lib name . version)
  (let ([version (if (pair? version) (string-append "." (car version)) "")])
    (let loop ([name name])
      (cond
       [(ffi-lib? name) name]
       [(path? name) (loop (path->string name))]
       [(not (string? name)) (raise-type-error 'ffi-lib "library-name" name)]
       [else (let ([name  (string-append name version)]
                   [name* (string-append name "." lib-suffix version)])
               (or (ffi-lib name #t)         ; try unmodified name first
                   (ffi-lib name* #t)        ; try with platform-suffix
                   (and (file-exists? name)  ; try a relative path
                        (ffi-lib (build-path 'same name) #t))
                   (and (file-exists? name*) ; relative with suffix
                        (ffi-lib (build-path 'same name*) #t))
                   ;; give up: call ffi-lib so it will raise an error
                   (ffi-lib name)))]))))

(define* (get-ffi-obj name lib type)
  (let ([lib  (get-ffi-lib lib)]
        [name (get-ffi-obj-name 'get-ffi-obj name)])
    (ptr-ref (ffi-obj name lib) type)))

;; It is important to use the set-ffi-obj! wrapper because it takes care of
;; keeping a handle on the object -- otherwise, setting a callback hook will
;; crash when the Scheme function is gone.
(define* (set-ffi-obj! name lib type new)
  (let* ([lib  (get-ffi-lib lib)]
         [name (get-ffi-obj-name 'set-ffi-obj! name)]
         [obj  (ffi-obj name lib)])
    (let-values ([(new type) (get-lowlevel-object new type)])
      (hash-table-put! ffi-objects-ref-table obj new)
      (ptr-set! obj type new))))

;; Used to convert strings and symbols to a byte-string that names an object
(define (get-ffi-obj-name name objname)
  (cond [(bytes? objname) objname]
        [(symbol? objname) (get-ffi-obj-name name (symbol->string objname))]
        [(string? objname) (string->bytes/utf-8 objname)]
        [else (raise-type-error name "object-name" objname)]))

;; This table keeps references to values that are set in foreign libraries, to
;; avoid them being GCed.  See set-ffi-obj! above.
(define ffi-objects-ref-table (make-hash-table))

;; ----------------------------------------------------------------------------
;; Function type

;; internal, used by _fun
(define (ffi-fun itypes otype . wrapper)
  (let ([wrapper (and (pair? wrapper) (car wrapper))])
    (if wrapper
      (make-ctype _fmark
        (lambda (x)
          ;; (ffi-callback (wrapper x) itypes otype)
          (error 'ffi-fun "cannot use wrappers for callback functions (yet)"))
        (lambda (x) (wrapper (ffi-call x itypes otype))))
      (make-ctype _fmark
        (lambda (x) (ffi-callback x itypes otype))
        (lambda (x) (ffi-call x itypes otype))))))

;; Syntax for the special _fun type:
;; (_fun [{(name ... [. name]) | name} [-> expr] ::]
;;       {type | (name : type [= expr]) | ([name :] type = expr)} ...
;;       -> {type | (name : type)}
;;       [-> expr])
;; Usage:
;; `{(name ...) | ...} ::' specify explicit wrapper function formal arguments
;;           `-> expr'     can be used instead of the last expr
;; `type'                  input type (implies input, but see type macros next)
;; `(name : type = expr)'  specify name and type, `= expr' means computed input
;; `-> type'               output type (possibly with name)
;; `-> expr'               specify different output, can use previous names
;; Also, see below for custom function types.

(provide _fun)
(define-syntax (_fun stx)
  (define (err msg . sub) (apply raise-syntax-error '_fun msg stx sub))
  (define (split-by key args)
    (let loop ([args args] [r (list '())])
      (cond [(null? args) (reverse! (map reverse! r))]
            [(eq? key (car args)) (loop (cdr args) (cons '() r))]
            [else (set-car! r (cons (car args) (car r)))
                  (loop (cdr args) r)])))
  (define (filtered-map f l)
    (let loop ([l l] [r '()])
      (if (null? l)
        (reverse! r)
        (let ([x (f (car l))]) (loop (cdr l) (if x (cons x r) r))))))
  (define id=? module-or-top-identifier=?)
  (syntax-case* stx (->) id=?
    [(_ x ...)
     (let ([xs (map (lambda (x)
                      (syntax-case* x (-> ::) id=? [:: '::] [-> '->] [_  x]))
                    (syntax->list #'(x ...)))]
           [inputs #f] [output #f] [pre '()] [post '()]
           [input-names #f] [output-type #f] [output-expr #f])
       (define (pre! x)  (set! pre  (append! pre  (list x))))
       (define (post! x) (set! post (append! post (list x))))
       (define (custom-type type0)
         (define stops
           (map (lambda (s) (datum->syntax-object type0 s #f))
                '(#%app #%top #%datum)))
         (define (with-arg x)
           (syntax-case* x (=>) id=?
             [(id => body) (identifier? #'id) (list #'id #'body)]
             [_else x]))
         (let loop ([t (local-expand type0 'expression stops)]
                    [empty? #t] [type #f] [pre #f] [post #f])
           (syntax-case* t (type: pre: post:) id=?
             [(type: t x ...)
              (if type
                (err "bad expansion of custom type (two type:s)" type0)
                (loop #'(x ...) #f
                      (syntax-case #'t () [#f #f] [_ #'t]) pre post))]
             [(pre: p x ...)
              (if pre
                (err "bad expansion of custom type (two pre:s)" type0)
                (loop #'(x ...) #f type (with-arg #'p) post))]
             [(post: p x ...)
              (if post
                (err "bad expansion of custom type (two post:s)" type0)
                (loop #'(x ...) #f type pre (with-arg #'p)))]
             [() (and (not empty?) (list type pre post))]
             [_else #f])))
       (define (t-n-e clause type name expr)
         (let ([s (custom-type type)])
           (define (arg x . no-expr?)
             (define use-expr?
               (and (list? x) (= 2 (length x)) (identifier? (car x))))
             ;; when the current expr is not used with a (x => ...) form,
             ;; either check that no expression is given or just make it
             ;; disappear from the inputs.
             (unless use-expr?
               (if (and (pair? no-expr?) (car no-expr?) expr)
                 (err "got an expression for a custom type that do not use it"
                      clause)
                 (set! expr (void))))
             (if use-expr?
               #`(let ([#,(car x) #,name]) #,(cadr x))
               x))
           (when s
             (set! type (car s))
             (when (cadr  s) (pre!  #`[#,name #,(arg (cadr  s) #t)]))
             (when (caddr s) (post! #`[#,name #,(arg (caddr s))])))
           (unless (or type expr)
             (err "got ignored input, with no expression" clause))
           (list type name expr)))
       (let ([dd (split-by ':: xs)])
         (case (length dd)
           [(0) (err "something bad happened (::)")]
           [(1) #f]
           [(2)
            (let ([ar (split-by '-> (car dd))])
              (case (length ar)
                [(0) (err "something bad happened (-> ::)")]
                [(1) (set! input-names (car dd))]
                [(2) (set! input-names (car ar)) (set! output-expr (cadr ar))]
                [else
                 (err "saw two or more instances of `->' on left of `::'")]))
            (if (and input-names (not (= 1 (length input-names))))
              (err "bad wrapper formals")
              (set! input-names (car input-names)))
            (set! xs (cadr dd))]
           [else (err "saw two or more instances of `::'")]))
       (let ([ar (split-by '-> xs)])
         (when (null? ar) (err "something bad happened (->)"))
         (when (null? (cdr ar)) (err "missing output type"))
         (set! inputs (car ar))
         (set! output-type (cadr ar))
         (unless (null? (cddr ar))
           (when output-expr (err "ambiguous output expression"))
           (set! output-expr (caddr ar))
           (unless (null? (cdddr ar))
             (err "saw three or more instances of `->'"))))
       (cond [(not output-type) (err "no output type")]
             [(null? output-type) (err "missing output type")]
             [(null? (cdr output-type)) (set! output-type (car output-type))]
             [else (err "extraneous output type" (cadr output-type))])
       (cond [(not output-expr)]
             [(null? output-expr) (err "missing output expression")]
             [(null? (cdr output-expr)) (set! output-expr (car output-expr))]
             [else (err "extraneous output expression" (cadr output-expr))])
       (set! inputs
             (map (lambda (sub temp)
                    (syntax-case* sub (: =) id=?
                      [(name : type)        (t-n-e sub #'type #'name #f)]
                      [(type = expr)        (t-n-e sub #'type temp   #'expr)]
                      [(name : type = expr) (t-n-e sub #'type #'name #'expr)]
                      [type                 (t-n-e sub #'type temp   #f)]))
                  inputs
                  (generate-temporaries (map (lambda (x) 'tmp) inputs))))
       ;; when processing the output type, only the post code matters
       (set! pre! (lambda (x) #f))
       (set! output
             (let ([temp (car (generate-temporaries #'(ret)))])
               (syntax-case* output-type (: =) id=?
                 [(name : type) (t-n-e output-type #'type #'name output-expr)]
                 [(type = expr) (if output-expr
                                  (err "extraneous output expression" #'expr)
                                  (t-n-e output-type #'type temp #'expr))]
                 [(name : type = expr)
                                (if output-expr
                                  (err "extraneous output expression" #'expr)
                                  (t-n-e output-type #'type #'name #'expr))]
                 [type          (t-n-e output-type #'type temp output-expr)])))
       (if (or (caddr output) input-names (ormap caddr inputs)
               (ormap (lambda (x) (not (car x))) inputs)
               (pair? pre) (pair? post))
         (let ([input-names (or input-names
                                (filtered-map (lambda (i)
                                                (and (not (caddr i)) (cadr i)))
                                              inputs))]
               [output-expr (let ([o (caddr output)])
                              (or (and (not (void? o)) o)
                                  (cadr output)))])
           #`(ffi-fun (list #,@(filtered-map car inputs)) #,(car output)
               (lambda (ffi)
                 (lambda #,input-names
                   (let* (#,@(filtered-map (lambda (i)
                                             (and (caddr i)
                                                  (not (void? (caddr i)))
                                                  #`[#,(cadr i) #,(caddr i)]))
                                           inputs)
                          #,@pre
                          [#,(cadr output)
                           (ffi #,@(filtered-map
                                    (lambda (x) (and (car x) (cadr x)))
                                    inputs))]
                          #,@post)
                     #,output-expr)))))
           #`(ffi-fun (list #,@(filtered-map car inputs)) #,(car output))))]))

;; ----------------------------------------------------------------------------
;; String types

;; The internal _string type uses the native ucs-4 encoding, also providing a
;; utf-16 type (note: these do not use #f as NULL).
(provide _string/ucs-4 _string/utf-16)

;; 8-bit string encodings (#f is NULL)
(define (false-or-op op) (lambda (x) (and x (op x))))
(define* _string/utf-8
  (make-ctype _bytes
    (false-or-op string->bytes/utf-8) (false-or-op bytes->string/utf-8)))
(define* _string/locale
  (make-ctype _bytes
    (false-or-op string->bytes/locale) (false-or-op bytes->string/locale)))
(define* _string/latin-1
  (make-ctype _bytes
    (false-or-op string->bytes/latin-1) (false-or-op bytes->string/latin-1)))

;; A generic _string type that usually does the right thing via a parameter
(define* default-_string-type
  (make-parameter _string/utf-8
    (lambda (x)
      (if (ctype? x)
        x (error 'default-_string-type "expecting a C type, got ~e" x)))))
;; The type looks like an identifier, but it's actually using the parameter
(provide _string)
(define-syntax _string
  (syntax-id-rules (_string)
    [_string (default-_string-type)]))

;; _symbol is defined in C, since it uses simple C strings
(provide _symbol)

;; `file' type: path-expands a path string, provide _path too.
(provide _path)
(define* _file (make-ctype _path expand-path #f))

;; `string/eof' type: converts an output #f (NULL) to an eof-object.
(define* _string/eof
  (make-ctype _string #f (lambda (x) (or x eof))))

;; ----------------------------------------------------------------------------
;; Utility types

;; Call this with a name (symbol) and a list of symbols, where a symbol can be
;; followed by a '= and an integer to have a similar effect of C's enum.
(define (_enum* name symbols)
  (define sym->int '())
  (define int->sym '())
  (define s->c (string->symbol (format "enum:~a->int" name)))
  (let loop ([i 0] [symbols symbols])
    (unless (null? symbols)
      (when (and (pair? (cdr symbols))
                 (eq? '= (cadr symbols))
                 (pair? (cddr symbols)))
        (set! i (caddr symbols))
        (set-cdr! symbols (cdddr symbols)))
      (set! sym->int (cons (cons (car symbols) i) sym->int))
      (set! int->sym (cons (cons i (car symbols)) int->sym))
      (loop (add1 i) (cdr symbols))))
  (make-ctype _int
    (lambda (x)
      (let ([a (assq x sym->int)])
        (if a
          (cdr a)
          (raise-type-error s->c (format "~a" name) x))))
    (lambda (x) (cond [(assq x int->sym) => cdr] [else #f]))))

;; Macro wrapper -- no need for a name
(provide _enum)
(define-syntax (_enum stx)
  (syntax-case stx ()
    [(_ syms) (with-syntax ([name (syntax-local-name)])
                #'(_enum* 'name syms))]))

;; Call this with a name (symbol) and a list of (symbol int) or symbols like
;; the above with '= -- but the numbers have to be specified in some way.  The
;; generated type will convert a list of these symbols into the logical-or of
;; their values and back.
(define (_bitmask* name symbols->integers)
  (define s->c (string->symbol (format "bitmask:~a->int" name)))
  (let loop ([s->i symbols->integers])
    (unless (null? s->i)
      (when (and (pair? (cdr s->i)) (eq? '= (cadr s->i)) (pair? (cddr s->i)))
        (set-car! s->i (list (car s->i) (caddr s->i)))
        (set-cdr! s->i (cdddr s->i)))
      (unless (and (pair? (car s->i)) (pair? (cdar s->i)) (null? (cddar s->i))
                   (symbol? (caar s->i)) (integer? (cadar s->i)))
        (error '_bitmask "bad spec in ~e" symbols->integers))
      (loop (cdr s->i))))
  (make-ctype _int
    (lambda (symbols)
      (if (null? symbols) ; probably common
        0
        (let loop ([xs (if (pair? symbols) symbols (list symbols))] [n 0])
          (cond [(null? xs) n]
                [(assq (car xs) symbols->integers) =>
                 (lambda (x) (loop (cdr xs) (bitwise-ior (cadr x) n)))]
                [else (raise-type-error s->c (format "~a" name) symbols)]))))
    (lambda (n)
      (if (zero? n) ; probably common
        '()
        (let loop ([s->i symbols->integers] [l '()])
          (if (null? s->i)
            (reverse! l)
            (loop (cdr s->i)
                  (let ([i (cadar s->i)])
                    (if (and (not (= i 0)) (= i (bitwise-and i n)))
                      (cons (caar s->i) l)
                      l)))))))))

;; Macro wrapper -- no need for a name
(provide _bitmask)
(define-syntax (_bitmask stx)
  (syntax-case stx ()
    [(_ syms) (with-syntax ([name (syntax-local-name)])
                #'(_bitmask* 'name syms))]))

;; ----------------------------------------------------------------------------
;; Custom function type macros

;; These macros get expanded by the _fun type.  They can expand to a form that
;; looks like (keyword: value ...), where the keyword is `type:' for the type
;; that will be used, pre: for a binding that will be inserted before the ffi
;; call, and post: for a binding after the ffi call.  These two bindings can be
;; of the form (id => expr) to use the existing value.  Note that if the pre:
;; expression is not (id => expr), then it means that there is no input for
;; this argument.  Also note that if a custom type is used as an output type of
;; a function, then only the post: code is used.  The resulting wrapper looks
;; like:
;;   (let* (...bindings for arguments...
;;          ...bindings for pre-code...
;;          (ret-name ffi-call)
;;          ...bindings for post-code...)
;;     return-expression)

;; _?
;; This is not a normal ffi type -- it is a marker for expressions that should
;; not be sent to the ffi function.  Use this to bind local values in a
;; computation that is part of an ffi wrapper interface.
(provide _?)
(define-syntax _? (syntax-id-rules (_?) [_? (type: #f)]))

;; (_ptr <mode> <type>)
;; This is for pointers, where mode indicates input or output pointers (or
;; both).  If the mode is 'o (output), then the wrapper will not get an
;; argument for it, instead it will generate the matching argument.
(provide _ptr)
(define-syntax _ptr
  (syntax-rules (i o io)
    [(_ i  t) (type: _pointer
               pre:  (x => (let ([p (malloc t)])
                             (ptr-set! p t x) p)))]
    [(_ o  t) (type: _pointer
               pre:  (malloc t)
               post: (x => (ptr-ref x t)))]
    [(_ io t) (type: _pointer
               pre:  (x => (let ([p (malloc t)])
                             (ptr-set! p t x) p))
               post: (x => (ptr-ref x t)))]))

;; (_list <mode> <type> [<len>])
;; Similar to _ptr, except that it is used for converting lists to/from C
;; vectors.  The length is needed for output values where it is used in the
;; post code, and in the pre code of an output mode to allocate the block.  In
;; any case it can refer to a previous binding for the length of the list which
;; the C function will most likely require.
(provide _list)
(define-syntax _list
  (syntax-rules (i o io)
    [(_ i  t  ) (type: _pointer
                 pre:  (x => (list->cblock x t)))]
    [(_ o  t n) (type: _pointer
                 pre:  (malloc n t)
                 post: (x => (cblock->list x t n)))]
    [(_ io t n) (type: _pointer
                 pre:  (x => (list->cblock x t))
                 post: (x => (cblock->list x t n)))]))

;; (_vector <mode> <type> [<len>])
;; Same as _list, except that it uses Scheme vectors.
(provide _vector)
(define-syntax _vector
  (syntax-rules (i o io)
    [(_ i  t  ) (type: _pointer
                 pre:  (x => (vector->cblock x t)))]
    [(_ o  t n) (type: _pointer
                 pre:  (malloc n t)
                 post: (x => (cblock->vector x t n)))]
    [(_ io t n) (type: _pointer
                 pre:  (x => (vector->cblock x t))
                 post: (x => (cblock->vector x t n)))]))

;; _bytes or (_bytes o n) is for a memory block represented as a Scheme string.
;; _bytes is just like a string, and (_bytes o n) is for pre-malloc of the
;; string.  There is no need for other modes: i or io would be just like _bytes
;; since the string carries its size information (so there is no real need for
;; the `o', but it's there for consistency with the above macros).
(provide (rename _bytes* _bytes))
(define-syntax _bytes*
  (syntax-id-rules (_bytes* o)
    [_bytes*  (type: _bytes)]
    [(_ o n) (type: _bytes
              pre:  (make-sized-byte-string (malloc n) n)
              ;; post is needed when this is used as a function output type
              post: (x => (make-sized-byte-string x n)))]))

;; ----------------------------------------------------------------------------
;; Safe raw vectors

(define-struct cvector (ptr type length))

(provide cvector? cvector-length cvector-type
         ;; make-cvector* is a dangerous operation
         (rename make-cvector make-cvector*))

(define* _cvector
  (make-ctype _pointer cvector-ptr
    (lambda (x)
      (error '_vector
             "cannot automatically convert a C pointer to a cvector"))))

(provide (rename allocate-cvector make-cvector))
(define (allocate-cvector type len)
  (let ([cblock (malloc len type)])
    (make-cvector cblock type len)))

(define* (cvector-ref v i)
  (if (and (integer? i) (<= 0 i (sub1 (cvector-length v))))
    (ptr-ref (cvector-ptr v) (cvector-type v) i)
    (error 'cvector-ref "bad index ~e for cvector bounds of 0..~e"
           i (sub1 (cvector-length v)))))

(define* (cvector-set! v i x)
  (if (and (integer? i) (<= 0 i (sub1 (cvector-length v))))
    (ptr-set! (cvector-ptr v) (cvector-type v) i x)
    (error 'cvector-ref "bad index ~e for cvector bounds of 0..~e"
           i (sub1 (cvector-length v)))))

(define* (cvector->list v)
  (cblock->list (cvector-ptr v) (cvector-type v) (cvector-length v)))

(define* (list->cvector l type)
  (make-cvector (list->cblock l type) type (length l)))

;; ----------------------------------------------------------------------------
;; SRFI-4 implementation

(define-syntax (make-srfi-4 stx)
  (syntax-case stx ()
    [(_ TAG type) (identifier? #'TAG)
     (let ([name (string-append (symbol->string (syntax-object->datum #'TAG))
                                "vector")])
       (define (make-TAG-id prefix suffix)
         (datum->syntax-object
          #'TAG (string->symbol (string-append prefix name suffix)) #'TAG))
       (with-syntax ([TAG?         (make-TAG-id "" "?")]
                     [TAG          (make-TAG-id "" "")]
                     [make-TAG     (make-TAG-id "make-" "")]
                     [TAG-ptr      (make-TAG-id "" "-ptr")]
                     [TAG-length   (make-TAG-id "" "-length")]
                     [allocate-TAG (make-TAG-id "allocate-" "")]
                     [TAG*         (make-TAG-id "" "*")]
                     [list->TAG    (make-TAG-id "list->" "")]
                     [TAG->list    (make-TAG-id "" "->list")]
                     [TAG-ref      (make-TAG-id "" "-ref")]
                     [TAG-set!     (make-TAG-id "" "-set!")]
                     [TAGname      name])
         #'(begin
             (define-struct TAG (ptr length))
             (provide TAG?)
             (provide TAG-length (rename allocate-TAG make-TAG))
             (define (allocate-TAG n . init)
               (let* ([p (malloc n type)]
                      [v (make-TAG p n)])
                 (when (pair? init)
                   (let ([init (car init)])
                     (let loop ([i (sub1 n)])
                       (unless (< i 0)
                         (ptr-set! p type i init)
                         (loop (sub1 i))))))
                 v))
             (provide (rename TAG* TAG))
             (define (TAG* . vals)
               (list->TAG vals))
             (define* (TAG-ref v i)
               (if (TAG? v)
                 (if (and (integer? i) (< -1 i (TAG-length v)))
                   (ptr-ref (TAG-ptr v) type i)
                   (error 'TAG-ref "bad index ~e for ~a bounds of 0..~e"
                          i 'TAG (sub1 (TAG-length v))))
                 (raise-type-error 'TAG-ref TAGname v)))
             (define* (TAG-set! v i x)
               (if (TAG? v)
                 (if (and (integer? i) (< -1 i (TAG-length v)))
                   (ptr-set! (TAG-ptr v) type i x)
                   (error 'TAG-set! "bad index ~e for ~a bounds of 0..~e"
                          i 'TAG (sub1 (TAG-length v))))
                 (raise-type-error 'TAG-set! TAGname v)))
             (define* (TAG->list v)
               (if (TAG? v)
                 (cblock->list (TAG-ptr v) type (TAG-length v))
                 (raise-type-error 'TAG->list TAGname v)))
             (define* (list->TAG l)
               (make-TAG (list->cblock l type) (length l))))))]))

(make-srfi-4 s8  _int8)
(make-srfi-4 u8  _uint8)
(make-srfi-4 s16 _int16)
(make-srfi-4 u16 _uint16)
(make-srfi-4 s32 _int32)
(make-srfi-4 u32 _uint32)
(make-srfi-4 s64 _int64)
(make-srfi-4 u64 _uint64)
(make-srfi-4 f32 _float)
(make-srfi-4 f64 _double)

;; ----------------------------------------------------------------------------
;; Tagged pointers

;; This is a kind of a pointer that gets a specific tag when converted to
;; Scheme, and accepts only such tagged pointers when going to C.  An optional
;; `ptr-type' can be given to be used as the base pointer type, instead of
;; _pointer.
(define* (make-cpointer-type tag . ptr-type)
  (let ([tagged->C (string->symbol (format "~a->C" tag))]
        [error-string (format "expecting a \"~a\" pointer, got ~~e" tag)])
    (make-ctype (if (pair? ptr-type) (car ptr-type) _pointer)
      (lambda (p)
        (if (cpointer? p)
          (unless (eq? tag (cpointer-type p))
            (error tagged->C error-string p))
          (error tagged->C error-string p))
        p)
      (lambda (p) (set-cpointer-type! p tag) p))))

;; A macro version of the above, using the defined name for a tag string, and
;; defining a predicate too.  The name should look like `_foo', the predicate
;; will be `foo?', and the tag will be "foo".  In addition, `foo-tag' is bound
;; to the tag.  The optional `ptr-type' argument is the same as that of
;; `make-cpointer-type'.
(provide define-cpointer-type)
(define-syntax (define-cpointer-type stx)
  (syntax-case stx ()
    [(_ _TYPE ptr-type)
     (and (identifier? #'_TYPE)
          (regexp-match #rx"^_.+" (symbol->string (syntax-e #'_TYPE))))
     (let ([name (cadr (regexp-match #rx"^_(.+)$"
                                     (symbol->string (syntax-e #'_TYPE))))])
       (define (id . strings)
         (datum->syntax-object
          #'_TYPE (string->symbol (apply string-append strings)) #'_TYPE))
       (with-syntax ([name-string name]
                     [TYPE?       (id name "?")]
                     [TYPE-tag    (id name "-tag")])
         #'(define-values (_TYPE TYPE? TYPE-tag)
             (let ([TYPE-tag name-string])
               (values (make-cpointer-type TYPE-tag ptr-type)
                       (lambda (x)
                         (and (cpointer? x) (eq? TYPE-tag (cpointer-type x))))
                       TYPE-tag)))))]
    [(_ _TYPE) #'(_ _TYPE _pointer)]))

;; ----------------------------------------------------------------------------
;; Struct wrappers

(define (compute-offsets types)
  (let loop ([ts types] [cur 0] [r '()])
    (if (null? ts)
      (reverse! r)
      (let* ([algn (ctype-alignof (car ts))]
             [pos  (+ cur (modulo (- (modulo cur algn)) algn))])
        (loop (cdr ts)
              (+ pos (ctype-sizeof (car ts)))
              (cons pos r))))))

;; Simple structs: call this with a list of types, and get a type that marshals
;; C structs to/from Scheme lists.
(define* (_list-struct types)
  (let ([stype (make-cstruct-type types)]
        [offsets (compute-offsets types)])
    (make-ctype stype
      (lambda (vals)
        (let ([block (malloc stype)])
          (for-each (lambda (type ofs val) (ptr-set! block type 'abs ofs val))
                    types offsets vals)
          block))
      (lambda (block)
        (map (lambda (type ofs) (ptr-ref block type 'abs ofs))
             types offsets)))))

;; (define-cstruct _foo ((slot type) ...)) defines a type called _foo for a C
;; struct, with user-procedues: make-foo, foo? foo-slot ... and set-foo-slot!
;; ....  The `_' prefix is required.  Objects of this new type are actually
;; cpointers, with a type tag that is "foo", provided as foo-tag, and tags of
;; pointers are checked before attempting to use them (see define-cpointer-type
;; above).  Note that since structs are implemented as pointers, they can be
;; used for a _pointer input to a foreign function: their address will be used,
;; to make this a little safer, the corresponding cpointer type is defined as
;; _foo-pointer.
(provide define-cstruct)
(define-syntax (define-cstruct stx)
  ;; It would be nice to extend this to handle inheritance...
  (syntax-case stx ()
    [(_ _TYPE ((slot slot-type) ...))
     (and (identifier? #'_TYPE)
          (andmap identifier? (syntax->list #'(slot ...)))
          (regexp-match #rx"^_.+" (symbol->string (syntax-e #'_TYPE))))
     (let ([name (cadr (regexp-match #rx"^_(.+)$"
                                     (symbol->string (syntax-e #'_TYPE))))]
           [slot-names (map (lambda (x) (symbol->string (syntax-e x)))
                            (syntax->list #'(slot ...)))])
       (define (id . strings)
         (datum->syntax-object
          #'_TYPE (string->symbol (apply string-append strings)) #'_TYPE))
       (define (ids name-func)
         (map (lambda (s stx)
                (datum->syntax-object
                 stx (string->symbol (apply string-append (name-func s))) stx))
              slot-names (syntax->list #'(slot ...))))
       (with-syntax
           ([name-string          name]
            [struct-string        (format "struct:~a" name)]
            [_TYPE-pointer        (id "_"name"-pointer")]
            [_TYPE*               (id "_"name"*")]
            [TYPE?                (id name"?")]
            [make-TYPE            (id "make-"name)]
            [TYPE->C              (id name"->C")]
            [TYPE-tag             (id name"-tag")]
            [(stype ...)          (ids (lambda (s) `(,name"-",s"-type")))]
            [(TYPE-SLOT ...)      (ids (lambda (s) `(,name"-",s)))]
            [(set-TYPE-SLOT! ...) (ids (lambda (s) `("set-",name"-",s"!")))]
            [(offset ...) (generate-temporaries
                                  (ids (lambda (s) `(,s"-offset"))))])
         #'(define-values (_TYPE _TYPE-pointer TYPE? TYPE-tag make-TYPE
                           TYPE-SLOT ... set-TYPE-SLOT! ...)
             (let*-values ([(stype ...) (values slot-type ...)]
                           [(types) (list stype ...)]
                           [(offset ...) (apply values
                                                (compute-offsets types))])
               (define _TYPE*
                 (make-cpointer-type TYPE-tag (make-cstruct-type types)))
               (define-cpointer-type _TYPE)
               (values _TYPE* _TYPE TYPE? TYPE-tag
                       (lambda (slot ...)
                         (let ([block (malloc _TYPE*)])
                           (set-cpointer-type! block TYPE-tag)
                           (ptr-set! block stype 'abs offset slot)
                           ...
                           block))
                       (lambda (x)
                         (unless (TYPE? x)
                           (raise-type-error 'TYPE-SLOT struct-string x))
                         (ptr-ref x stype 'abs offset))
                       ...
                       (lambda (x slot)
                         (unless (TYPE? x)
                           (raise-type-error 'set-TYPE-SLOT! struct-string
                                             0 x slot))
                         (ptr-set! x stype 'abs offset slot))
                       ...)))))]))

;; ----------------------------------------------------------------------------
;; Misc utilities

;; Used by set-ffi-obj! to get the actual value so it can be kept around
(define (get-lowlevel-object x type)
  (let ([basetype (ctype-basetype type)])
    (if basetype
      (let ([s->c (ctype-scheme->c type)])
        (get-lowlevel-object (if s->c (s->c x) x) basetype))
      (values x type))))

;; Converting Scheme lists to/from C vectors (going back requires a length)
(define* (list->cblock l type)
  (if (null? l)
    #f ; null => NULL
    (let ([cblock (malloc (length l) type)])
      (let loop ([l l] [i 0])
        (unless (null? l)
          (ptr-set! cblock type i (car l))
          (loop (cdr l) (add1 i))))
      cblock)))
(define* (cblock->list cblock type len)
  (cond [(zero? len) '()]
        [(cpointer? cblock)
         (let loop ([i (sub1 len)] [r '()])
           (if (< i 0)
             r
             (loop (sub1 i) (cons (ptr-ref cblock type i) r))))]
        [else (error 'cblock->list
                     "expecting a non-void pointer, got ~s" cblock)]))

;; Converting Scheme vectors to/from C vectors
(define* (vector->cblock v type)
  (let ([len (vector-length v)])
    (if (zero? len)
      #f ; #() => NULL
      (let ([cblock (malloc len type)])
        (let loop ([i (sub1 len)])
          (unless (< i 0)
            (ptr-set! cblock type i (vector-ref v i))
            (loop (add1 i))))
        cblock))))
(define* (cblock->vector cblock type len)
  (cond [(zero? len) '#()]
        [(cpointer? cblock)
         (let ([v (make-vector len)])
           (let loop ([i (sub1 len)])
             (unless (< i 0)
               (vector-set! v i (ptr-ref cblock type i))
               (loop (sub1 i))))
           v)]
        [else (error 'cblock->vector
                     "expecting a non-void pointer, got ~s" cblock)]))

;; Useful for automatic definitions
;; If a provided regexp begins with a "^" or ends with a "$", then
;; `regexp-replace' is used, otherwise use `regexp-replace*'.
(define* (regexp-replaces x rs)
  (let loop ([str (if (bytes? x) (bytes->string/utf-8 x) (format "~a" x))]
             [rs rs])
    (if (null? rs)
      str
      (loop ((if (regexp-match #rx"^\\^|\\$$"
                               (if (regexp? (caar rs))
                                 (object-name (caar rs)) (caar rs)))
               regexp-replace regexp-replace*)
             (caar rs) str (cadar rs)) (cdr rs)))))

)
