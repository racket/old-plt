;; interface with DrScheme. Robby has some code in collects/drscheme/tools/spidey2 that uses all that stuff...

(unit/sig spidey2^
  (import [zodiac : zodiac:system^]
          [mzlib : mzlib:core-flat^]
          [setexp : newspidey:datadef-setexp^]
          [type : newspidey:datadef-types^]
          [cgp : newspidey:constraints-gen-and-prop^]
          [da : newspidey:debug-arity^]
          [tr : newspidey:type-reconstruction^])

;; -> (listof (list zodiac:location zodiac:location symbol))
;; symbol = 'green | 'red
;; extracts locations of erroneous terms, to be flaged in red by DrScheme
(define (get-prims)
  (letrec ([get-prims-l
            (lambda (l)
              (if (null? l)
                  '()
                  (let* ([func (car l)]
                         [start-loc (zodiac:zodiac-start func)]
                         [end-loc (zodiac:zodiac-finish func)])
                    (cons (list start-loc end-loc 'red)
                          (get-prims-l (cdr l))))))])
    (get-prims-l da:*bad-apps*)))

;; symbol -> (union #f zodiac:location)
;; returns start location of term associated with set-var (represented as a symbol)
(define (get-loc sym)
  (let ([term (cgp:lookup-term-from-set-var sym)]) ;; XXX this should be a list because of top level redefinitions
    (if term
        (zodiac:zodiac-start term)
        #f)))

;; number -> (union #f symbol)
;; given a DrScheme offset, returns the first set-var (represented as a symbol)
;; in the list of set-vars associated with the terms that start at that offset.
(define (get-var offset)
  (letrec ([find-loc (lambda (offset l)
                       (if (null? l)
                           '()
                           ;; returns the list of set-vars that *exactly* have the given
                           ;; location (a list, to take macros into account)
                           (if (= offset (caar l))
                               (cons (cadar l)
                                     (find-loc offset (cdr l)))
                               (find-loc offset (cdr l)))))])
    (let ([set-vars (find-loc offset cgp:*location-list*)])
      (if (null? set-vars)
          #f
          ;; of course, we throw most of the list away, for now... XXX
          (car set-vars)))))

;; symbol -> Type
;; returns the type of the term associated with the given set-var (represented as a symbol)
(define (get-type sym)
  (tr:type-reduce (tr:mk-type (setexp:make-Set-var sym))))

;; Type -> string
;; type pretty printer
(define (pp-type type)
  (cond
    [(type:Type-Arrow? type)
     (string-append
      "("
      (apply string-append (map (lambda (tp) (string-append (pp-type tp) " "))
                                (type:Type-Arrow-doms type)))
      "-> "
      (pp-type (type:Type-Arrow-rng type))
      ")")]
    [(type:Type-Cons? type)
     (string-append "(cons "
                    (pp-type (type:Type-Cons-car type))
                    " "
                    (pp-type (type:Type-Cons-cdr type))
                    ")")]
    [(type:Type-Scheme? type)
     (string-append
      "(for all"
      (apply string-append (map (lambda (sym) (string-append " " (symbol->string sym)))
                                (type:Type-Scheme-vars type)))
      ": "
      (pp-type (type:Type-Scheme-type type))
      ")")]
    [(setexp:Set-var? type)
     (symbol->string (setexp:Set-var-name type))]
    [(type:Type-Rec? type)
     (string-append
      "(rec ("
      (apply string-append (map (lambda (binding)
                                  (string-append "["
                                                 (symbol->string (setexp:Set-var-name (type:Type-Binding-set-var binding)))
                                                 " "
                                                 (pp-type (type:Type-Binding-type binding))
                                                 "]"))
                                (type:Type-Rec-bindings type)))
      ") "
      (pp-type (type:Type-Rec-type type))
      ")")]
    [(type:Type-Union? type)
     (string-append
      "(union"
      (apply string-append (map (lambda (t)
                                  (string-append " " (pp-type t)))
                                (type:Type-Union-types type)))
      ")")]
    [(setexp:Const? type)
     (let ([val (setexp:Const-val type)])
       (cond
         [(number? val) (number->string (setexp:Const-val type))]
         [(string? val) "str"]
         [(symbol? val) "symbol"]
         [(null? val) "null"]
         [(boolean? val) (if val "true" "false")]
         [(void? val) "void"]
         [else (error 'pp-type "unknown constant type ~a" type)]))]
    [(type:Type-Struct? type)
     (string-append
      "(struct:"
      (symbol->string (type:Type-Struct-name type))
      (apply string-append (map (lambda (t)
                                  (string-append " " (pp-type t)))
                                (type:Type-Struct-fields type)))
      ")")]
    [(type:Type-Empty? type)
     "empty"]
    [else (error 'pp-type "unknown type: ~a" type)]))

;; symbol -> (listof symbol)
;; returns list of children of set-var (represented as symbol)
(define (children sym)
  (let ([chi-set-vars (cgp:lookup-lo-and-filter setexp:Set-var? (setexp:make-Set-var sym))])
    (map setexp:Set-var-name chi-set-vars)))
  
;; symbol -> (listof symbol)
;; returns list of parents of set-var (represented as symbol)
;; XXX should recur until the returned set-vars are associated with terms
(define (parents sym)
  (let ([par-set-vars (cgp:lookup-hi-and-filter setexp:Set-var? (setexp:make-Set-var sym))])
    (map setexp:Set-var-name par-set-vars)))

;; Type symbol -> boolean
;; symbol = 'number | 'null | 'pair | 'procedure
;; type predicate
(define (has-member? type sym)
  (if (type:Type-Union? type)
      (ormap (lambda (sub-type) (has-member? sub-type sym)) (type:Type-Union-types type))
      (cond
        [(mzlib:symbol=? sym 'number)
         (and (setexp:Const? type)
              (number? (setexp:Const-val type)))]
        [(mzlib:symbol=? sym 'null)
         (and (setexp:Const? type)
              (null? (setexp:Const-val type)))]
        [(mzlib:symbol=? sym 'pair)
         (type:Type-Cons? type)]
        [(mzlib:symbol=? sym 'procedure)
         (type:Type-Arrow? type)]
        ;; XXX struct ?
        [else (error 'has-member? "unknown predicate: ~a" sym)])))

  ) ;; unit/sig