(module error-messaging mzscheme
  
  (require "ast.ss")
  (require "types.ss")
  
  (provide make-error-pass get-expected type->ext-name id->ext-name get-call-type method-name->ext-name)
  
  (provide make-so build-src-list make-parm-string)
  
  ;make-error: 'a string 'a src -> void
  (define (make-error-pass parm)
    (lambda (kind message so src)
      (raise-syntax-error kind message (make-so so src parm))))
  
  ;make-so: symbol src (-> location) -> syntax-object
  (define (make-so id src parm)
    (datum->syntax-object #f id (build-src-list parm src)))
  
  ;build-src-list: src (-> location) -> (U bool (list loc int int int int))
  (define (build-src-list src parm)
    (if (not src)
        src
        (if (and (= (src-line src) 0)
                 (= (src-col src) 0)
                 (= (src-pos src) 0)
                 (= (src-span src) 0))
            #f
            (list (parm) (src-line src) (src-col src) (src-pos src) (src-span src)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Message helpers
  
  ;get-expected: symbol-> string
  (define (get-expected e)
    (case e
      ((bool) 'boolean)
      ((int) "int, short, byte or char")
      ((num) "double, float, long, int, short, byte or char")
      (else "dummy")))
  
 ;type->ext-name: type -> (U symbol string)
  (define (type->ext-name t)
    (cond 
      ((ref-type? t) (ref-type-class/iface t))
      ((array-type? t) 
       (format "~a~a" (type->ext-name (array-type-type t))
                      (let ((dims ""))
                        (let loop ((d (array-type-dim t)))
                          (if (= d 0)
                              dims
                              (begin (set! dims (string-append dims "[]"))
                                     (loop (sub1 d))))))))
      (else t))) 
  
  ;id->ext-name: id -> symbol
  (define (id->ext-name id)
    (string->symbol (id-string id)))
  
  ;get-call-type: type -> string
  (define (get-call-type t)
    (cond
      ((eq? t 'super) "the current super class")
      ((not t) "this class")
      (else (type->ext-name exp))))
  
  ;make-parm-string: (list field) -> string
  (define (make-parm-string parms)
    (if (null? parms)
        ""
        (substring (apply string-append
                          (map 
                           (lambda (p) (string-append (id-string (field-name p)) " "))
                           parms))
                   0 (sub1 (length parms)))))
  
  ;method-name->ext-name: string (list field) -> string
  (define (method-name->ext-name name parms)
    (format "~a(~a)" name (make-parm-string parms)))
  
  )