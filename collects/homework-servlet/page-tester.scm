(module page-tester mzscheme
  (require
   (lib "list.ss")
   (lib "unitsig.ss")
   (lib "servlet-helpers.ss" "web-server")
   (lib "servlet-sig.ss" "web-server")
   (lib "servlet.ss" "web-server"))
  
  
  (provide test-pages^ test-pages@ test-pages-macro)
  
  (define-signature test-pages^ (test-pages))
  
  (define-syntax (test-pages-macro stx)
    (syntax-case stx ()
      [(_ ([titles thunks] ...))
       (with-syntax ([test-pages (datum->syntax-object stx 'test-pages)])
         #'(test-pages (list (list titles thunks) ...)))]))
  
  (define test-pages@
    (unit/sig test-pages^
      (import servlet^)
  
      (define PAGE-STR "page")
      (define PAGE-SYM (string->symbol PAGE-STR))
      
      (define make-checked
        (lambda (num1 num2)
          (if (= num1 num2)
              '([checked ""])
              '())))
      
      ;; make-test-page: (listof string) -> (string number -> request)
      (define make-test-page
        (lambda (titles)
          (lambda (str num)
            (send/suspend
             (lambda (action)
               `(html
                 (title "Test Page")
                 (body
                  (h3 "Test Page")
                  (p "Previous test output: ") (br)
                  ,str (br)
                  (form ([action ,action] [method "post"] [enctype "multipart/form-data"])
                        (p "Select a page to view")
                        
                        ;; all radio buttons in the same group must have the same name
                        (table
                         ,@(make-test-page-rows num titles))
                        (br)(input ([type "submit"] [name "submit"] [value "submit"]))))))))))
      
      ;; make-test-page-rows: number (listof string) -> (listof x-expression)
      (define make-test-page-rows
        (lambda (num titles)
          (let loop ([i 0] [titles titles])
            (cond
              [(null? titles) '()]
              [else
               (cons `(tr (td (input ([type "radio"] [name ,PAGE-STR]
                                      [value ,(format "p~a" (add1 i))]
                                      ,@(make-checked num i)) ,(car titles))))
                     (loop (add1 i) (cdr titles)))]))))
      
      ;; main-loop: (string number -> request) (listof (-> request)) ->
      (define main-loop
        (lambda (test-page thunks)
          (let loop ([str "No previous test."] [num 0])
            (let ([bindings (request-bindings (test-page str num))])
              (let ([bstr (extract-binding/single PAGE-SYM bindings)])
                (let cond-loop ([i 1] [tl thunks])
                  (cond
                    [(string=? bstr (format "p~a" i))
                     (let* ([result ((car tl))]
                            [bindings (if (request? result)
                                          (request-bindings result)
                                          result)])
                       (loop (format "~s" bindings) i))]
                    [else (cond-loop (add1 i) (cdr tl))])))))))
      
      ;; test-pages: (listof (list string (-> request))) ->
      (define test-pages
        (lambda (titles-and-thunks)
          (apply (lambda (titles thunks)
                   (main-loop (make-test-page titles) thunks))
                 (foldr
                  (lambda (title-and-thunk ls)
                    (list
                     (cons (car title-and-thunk)
                           (car ls))
                     (cons (cadr title-and-thunk)
                           (cadr ls))))
                  '(() ())
                  titles-and-thunks))))
      
     
  )))
