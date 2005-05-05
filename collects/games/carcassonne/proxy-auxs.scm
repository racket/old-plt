#cs
(module proxy-auxs mzscheme 
  
  ;; auxiliaries for proxy remote calls, returns, and receptions of these 
  
  (require "if.scm"
           "tiles.scm"
           (lib "class.ss")
           (lib "etc.ss")
           (lib "match.ss")
           (lib "thread.ss")
           (lib "port.ss")
           (lib "xml.ss" "xml"))
  
  (provide 
   ;; CB = Symbol Any ... -> Void
   
   make-listen
   ;; IPort CB (-> Void) (Xexpr -> Void) -> [case-lambda (CB -> Any) (-> Void)]
   ;; create a function that listens on the given port
   ;; -- calls are interpreted by the given callback 
   ;; -- returns produce the final value 
   
   make-call ;; OPort -> (Symbol Any ... -> Void)
   ;; create a function that writes a method call to the given output port
   
   make-return ;; OPort -> (Any -> Void)
   ;; create a function that writes a method return to the given output port 
   
   ;; read-message ;; IPort (-> Void) -> Xexpr 
   ;; read a call or return from the given port, 
   ;; use handler for eof or xml mistakes   
   
   check-args ;; Symbol Number Listof[X] -> Void
   ;; a contract error unless the number is equal to the length of the list
   
   check-type ;; Symbol Number Listof[X] -> Void
   ;; a contract error unless the number is equal to the length of the list
   
   reader-redirect ;; IPort -> IPort IPort 
   ;; duplicate inputs from the given iport
   )
  
  (define (reader-redirect in)
    (define-values (in1 out1) (make-pipe))
    ; (define-values (in2 out2) (make-pipe))
    (define out2 (open-output-string))
    (thread (lambda () 
              (copy-port in out1 out2)
              (close-output-port out1)
              (close-output-port out2)))
    (values in1 out2))
  
  (define (check-args tag n args)
    (unless (= (length args) n)
      (contract/violation 
       (format "~s expected ~s arguments, given ~s" tag n args))))
  
  (define (check-type tag ty pred? arg)
    (unless (pred? arg)
      (contract/violation 
       (format "~s expected argument of type ~s, given ~s" tag ty arg))))  
  
  ;; --- LISTEN --- 
  
  (define (make-listen input cb0 err-eof err-bad-msg)
    (rec L
      (case-lambda 
        [() (L cb0)]
        [(cb)
         (let L ()
           (define r (read-message input err-eof))
           ; (printf ">>> ~s~n" r)
           (cond
             #;[(eof-object? r) (err-eof)]
               [(parse-call r) => (lambda (r-as-call) 
                                    (cb (car r-as-call) (cdr r-as-call))
                                    (L))]
               [(parse-return r) => car]
               [else (err-bad-msg r)]))])))
  
  (define (read-message in err-eof)
    (xml->xexpr
     [(eliminate-whitespace '(call return list li string number void boolean tile) identity)
      (with-handlers ([exn:xml? (lambda (x)
                                  ; (printf "--> ~s (~s)~n" x (exn:xml-locs x))
                                  (err-eof))])
        (read-xml/element in))]))
  
  ;; --- CALL & RETURN --- 
  
  (define (make-call o)
    (define (call m . args) 
      (display-xml/content (xexpr->xml (create-call m args)) o))
    call)
  
  (define (make-return o)
    (define (return x)
      (display-xml/content (xexpr->xml (create-return x)) o))
    return)  
  
  ;; --- CALL: to and from X-expressions --- 
  
  ;; Symbol Listof[Any] -> Xexpr
  (define (create-call x y)
    `(call ([name ,(symbol->string x)]) ,@(args->xexpr y)))
  
  ;; Xexpr -> (union false (cons Symbol Listof[Any]))
  (define (parse-call x) 
    (with-handlers ([exn:fail? (lambda _ #f)])
      (match x 
        [('call (['name n]) ('list () . a)) (cons (string->symbol n) (xexpr->args `(list () . ,a)))]
        [('call (['name n]) . a) (cons (string->symbol n) (xexpr->args `(list () . ,a)))]
        [else #f])))
  
  ;; Listof[Any] -> Xexpr[list]
  (define (args->xexpr args)
    `(list () ,@(map (lambda (a) `(li () ,(any->xexpr a))) args))
    (map (lambda (a) `(li () ,(any->xexpr a))) args)
    )
  
  ;; Xexpr[list] -> Listof[Any]
  (define (xexpr->args x)
    (let/ec done 
      (define (xexpr->arg a)
        (match a 
          [('li () x) (xexpr->any x)]
          [else (done #f)]))
      (match x
        [('list () . a) (map xexpr->arg a)]
        [else #f])))
  
  ;; --- RETURN: to and from X-expressions 
  
  ;; Any -> Xexpr 
  (define (create-return x) `(return () ,(any->xexpr x)))
  
  ;; Xexpr -> (union false (list Any))
  ;; effect: throw an exception? return false? 
  (define (parse-return x) 
    (with-handlers ([exn:fail? (lambda _ #f)])
      (match x
        [('return () v) (list (xexpr->any v))]
        [else #f])))
  
  ;; --- CALL & RETURN: common auxiliary functions 
  
  ;; Any -> Xexpr
  (define (any->xexpr a)
    (cond
      [(boolean? a) `(boolean ([value ,(boolean->string a)]))]
      [(number? a)  `(number  ([value ,(number->string a)]))]
      [(string? a)  `(string  ([value ,a]))]
      [(void? a)    `(void ())]
      [(is-a? a tile<%>) `(tile ,(tile-attrs->xexpr a))]
      [(null? a)    `(list ())]
      [(list? a)    `(list () ,@(map (lambda (a) `(li () ,(any->xexpr a))) a))]
      [(direction? a) `(direction ([value ,(direction->string a)]))]
      [(orientation? a) `(orientation ([value ,(orientation->string a)]))]
      [(position? a) `(position ([value ,(position->string a)]))]
      [else (error 'any->xexpr "not implemented yet: ~e" a)]))
  
  ;; Xexpr -> Any 
  (define (xexpr->any a)
    (define r
    (match a
      [('boolean (['value x])) (string->boolean x)]
      [('number  (['value x])) (string->number x)]
      [('string  (['value x])) x]
      [('void    ())           (void)]
      [('tile attributes)      (apply make-tile (xexpr->tile-attrs attributes))]
      [('list ())              '()]
      [('list () . rest)       
       (map (lambda (a)
              (match a
                [('li () a) (xexpr->any a)]
                [else (error 'xexpr->any "bad list item: ~e" a)]))
            rest)]
      [('direction (['value x])) (string->direction x)]
      [('position (['value x])) (string->position x)]
      [('orientation (['value x])) (string->orientation x)]
      [else (error 'xexpr->any "not implemented yet: ~e" a)]))
    (printf ">>> (x->a ~a) = ~a~n" a r)
    r)
  
  ;; tile<%> -> Listof[(list Symbol String)]
  (define (tile-attrs->xexpr t)
    `([index ,(index->string (send t get-index))]
      [x ,(coordinate->string (send t get-x))]
      [y ,(coordinate->string (send t get-y))]
      [orientation ,(orientation->string (send t get-o))]))
  
  ;; Listof[(list Symbol String)] -> (list String Number Number Orientation)
  (define (xexpr->tile-attrs x)
    (list (index> (cadr (assq 'index x)))
          (coordinate> (cadr (assq 'x x)))
          (coordinate> (cadr (assq 'y x)))
          (string->orientation (cadr (assq 'orientation x)))))
  
  ;; conversions and projection 
  (define (coordinate> x)
    (define r (string->number x))
    (if (coordinate? r) r (error 'number> "not a number: ~e" x)))
  
  (define (index> x)
    (if (index? x) x (error 'index> "not a tile index: ~e" x)))
  
  (define (orientation> x)
    (define r (string->number x))
    (if (orientation? r) r (error 'orientation> "not an orientation: ~e" x)))
  
  (define (boolean->string a) (if a "true" "false"))
  
  (define (string->boolean a) 
    (cond
      [(string=? "true" a) #t]
      [(string=? "false" a) #f]
      [else (error 'string->boolean "not a boolean: ~e" a)]))
  
  ;; --- timed action ---
  
  (require "contract.scm")
  
  (provide 
   timed-action ;; (-> X) N [-> Void] -> X
   ;; run to-do for n seconds
   ;; exception: qos/violation, if to-do doesn't return a proper value in n secs 
   )
  
  (define timed-action
    (opt-lambda (to-do n [on-time-error void])
      (define TIME-OUT (gensym))
      (define prp-result (make-channel))
      (define exn-result (make-channel))
      (define c (make-custodian))
      (define (with-exn-result exn) 
        (if (eq? TIME-OUT exn)
            (begin
              (on-time-error)
              (custodian-shutdown-all c)
              (qos/violation "time out"))
            (begin
              (custodian-shutdown-all c)
              (raise exn))))
      (define (with-prp-result val)
        val)
      (parameterize ([current-custodian c])
        (define (run-todo)
          (with-handlers ([exn:fail? (lambda (x) (channel-put exn-result x))]
                          [contract? (lambda (x) (channel-put exn-result x))])
            (channel-put prp-result (to-do))))
        (define (run-sleep)
          (sleep n)
          (channel-put exn-result TIME-OUT))
        (thread run-todo)
        (thread run-sleep))
      (sync ; object-wait-multiple
       ; #f
       (handle-evt #;make-wrapped-waitable exn-result with-exn-result)
       (handle-evt #;make-wrapped-waitable prp-result with-prp-result))))
  )
