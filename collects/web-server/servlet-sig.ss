; This file is misnamed since it does not define unit signatures
; directly anymore.  See "sig.ss" for signatures.  This file's purpose
; is to provide the minimum stuff that a servlet needs.  The file
; "servlet-helpers.ss" includes more functions that servlets often use.
(module servlet-sig mzscheme
  (require "sig.ss"
	   (lib "xml.ss" "xml"))
  (provide response?
	   (struct response/full (code message seconds mime extras body))
           (struct response/incremental ())
           (struct request (method uri headers host-ip client-ip))
           (rename request-bindings request-bindings/raw)
           (rename get-parsed-bindings request-bindings)
	   (all-from "sig.ss"))

  ; response = (cons str (listof str)), where the first str is a mime-type
  ;          | x-expression
  ;          | (make-response/full nat str nat str (listof (cons sym str)) (listof str))
  ;          | (make-response/incremental nat str nat str (listof (cons sym str))
  ;              ((str -> void) -> void))

  ; : TST -> bool
  (define (response? page)
    (or (response/full? page)
        ; this could fail for dotted lists - rewrite andmap
	(and (pair? page) (pair? (cdr page)) (andmap string? page))
					; insist the xexpr has a root element
	(and (pair? page) (xexpr? page))))
  
  ; more here - these should really have a common super type, but I don't want to break
  ; the existing interface.
  (define-struct response/full (code message seconds mime extras body))
  (define-struct (response/incremental response/full) ())
  
  ; request = (make-request sym URL (listof (cons sym str)) (U str (listof (cons sym str))) str str)
  ; Outside this module, bindings looks like an association list (due to renaming request-bindings).
  ; Inside it is a string for normal requests, but for file uploads it is still an association list.
  ; more here - perhaps it should always be a string inside this module.
  (define-struct request (method uri headers bindings host-ip client-ip))
  
  ; get-parsed-bindings : request -> (listof (cons sym str))
  (define (get-parsed-bindings r)
    (let ([x (request-bindings r)])
      (if (list? x)
          x
          (parse-bindings x))))
  
  ; parse-bindings : (U #f String) -> (listof (cons Symbol String))
  (define (parse-bindings raw)
    (if (string? raw)
        (let ([len (string-length raw)])
          (let loop ([start 0])
            (let find= ([key-end start])
              (if (>= key-end len)
                  null
                  (if (eq? (string-ref raw key-end) #\=)
                      (let find-amp ([amp-end (add1 key-end)])
                        (if (or (= amp-end len) (eq? (string-ref raw amp-end) #\&))
                            (cons (cons (string->symbol (substring raw start key-end))
                                        (translate-escapes 
                                         (substring raw (add1 key-end) amp-end)))
                                  (loop (add1 amp-end)))
                            (find-amp (add1 amp-end))))
                      (find= (add1 key-end)))))))
        null))
  
  (define-struct servlet-error ())
  (define-struct (invalid-%-suffix servlet-error) (chars))
  (define-struct (incomplete-%-suffix invalid-%-suffix) ())
  
  ; This comes from Shriram's collection, and should be exported form there. 
  ; translate-escapes : String -> String
  (define (translate-escapes raw)
    (list->string
     (let loop ((chars (string->list raw)))
       (if (null? chars) null
           (let ((first (car chars))
                 (rest (cdr chars)))
             (let-values (((this rest)
                           (cond
                             ((char=? first #\+)
                              (values #\space rest))
                             ((char=? first #\%)
                              ; MF: I rewrote this code so that Spidey could eliminate all checks. 
                              ; I am more confident this way that this hairy expression doesn't barf. 
                              (if (pair? rest)
                                  (let ([rest-rest (cdr rest)])
                                    (if (pair? rest-rest)
                                        (values (integer->char
                                                 (or (string->number (string (car rest) (car rest-rest)) 16)
                                                     (raise (make-invalid-%-suffix
                                                             (if (string->number (string (car rest)) 16)
                                                                 (car rest-rest)
                                                                 (car rest))))))
                                                (cdr rest-rest))
                                        (raise (make-incomplete-%-suffix rest))))
                                  (raise (make-incomplete-%-suffix rest))))
                             (else (values first rest)))))
               (cons this (loop rest)))))))))
