;; Mike Burns 2004-06-16 netgeek@speakeasy.net
;; Copyright 2004 Mike Burns
;; GNU GPL.

;; Common HTML Xexpr things to do.
(module widgets mzscheme
  (require (lib "etc.ss")
           (lib "contract.ss")
           (lib "xml.ss" "xml")
           (lib "servlet.ss" "web-server")
           )

  (provide/contract
    (build-page 
      ((string?) (listof xexpr/callback?) . ->* .  (xexpr/callback?)))
    (build-page/form 
      ((string?) (listof xexpr/callback?) . ->* . 
       ((string? . -> . xexpr/callback?))))
    (build-page/file-upload
      ((string?) (listof xexpr/callback?) . ->* . 
       ((string? . -> . xexpr/callback?))))
    (p
      ((xexpr/callback?) (listof xexpr/callback?) . ->* . (xexpr/callback?)))
    (hyperlink
      (((union string? procedure?)) (listof xexpr/callback?) . ->* .
       (xexpr/callback?)))
    (html-table 
      (string? (listof string?) list? . -> . xexpr/callback?))
    (input 
      (case-> (xexpr/callback? string? string? . -> . xexpr?)
              (xexpr/callback? string? string? string? . -> . xexpr?)))
    (text-input 
      (case-> (string? string? . -> . xexpr?)
              (string? string? string? . -> . xexpr?)))
    (password-input 
      (string? . -> . xexpr?))
    (submit-button 
      (() (string?) . opt-> . xexpr?))
    (actions-list 
      ((list-immutableof
         (cons-immutable/c string? (string? . -> . xexpr?))) . -> . xexpr?))
    (form 
      (((union xexpr/callback? string?) (listof (list/c symbol? string?)))
       (listof xexpr/callback?) . ->* . (xexpr/callback?)))
    )

  ;; build-page : String Xexpr ... -> Xexpr
  ;; Produce an average Web page.
  (define (build-page title . body)
    (when (not (andmap xexpr/callback? body))
      (raise body))
    `(html
       (head (title ,title))
       (body
         (h1 ,title)
         ,@body)))

  ;; Produce an average Web form, suitable for send/suspend and send/forward.
  (define (build-page/form title . body)
    ((form-page-maker '()) title body))

  ;; Produce an average Web form, suitable for send/suspend and send/forward,
  ;; but with a file-upload on it.
  (define (build-page/file-upload title . body)
    ((form-page-maker '((enctype "multipart/form-data"))) title body))

  ;; input : Xexpr-dispatch String String [String] -> Xexpr
  ;; A paragraph with an input with with a label, name, and optional value.
  (define input
    (case-lambda
      ((label name type)
       `(p (label ((for ,name)) ,label)
           (input ((type ,type) (id ,name) (name ,name)))))
      ((label name type value)
       `(p (label ((for ,name)) ,label)
           (input ((type ,type) (id ,name) (name ,name) (value ,value)))))))


  ;; text-input : String String [String] -> Xexpr
  ;; A paragraph with one text box with a label, name, and optional value.
  (define text-input
    (case-lambda
      ((label name) (input label name "text"))
      ((label name value) (input label name "text" value))))

  ;; password-input : String  -> Xexpr
  ;; A password input prompt, with a CGI name.
  (define (password-input name)
    (input "Password" name "password"))

  ;; submit-button : [String] -> Xexpr
  ;; A submit button, which might be titled.
  (define submit-button
    (opt-lambda ((label "Next"))
      `(p (input ((type "submit") (value ,label) (name "action"))))))

  ;; actions-list : (List (cons String (String -> Xexpr))) -> Xexpr
  ;; A list of actions that can be done, and what to do when they are done.
  (define (actions-list actions)
    `(ul
       ,@(map
           (lambda (action)
             `(li (a ((href ,(cdr action))) ,(car action))))
           actions)))

  ;; p : Any ... -> Xexpr
  ;; A basic, regular old paragraph.
  (define (p b . body)
    `(p ,b ,@body))

  ;; A hyperlink.
  (define (hyperlink url . body)
    `(a ((href ,url)) ,@body))

  ;; form : Any (listof (list Symbol String)) Xexpr ... -> Xexpr
  ;; A form.
  (define (form action attribs . body)
    `(form
       ((method "POST") ,@attribs (action ,action))
       ,@body))

  ;; Tabular data
  (define (html-table summary row-titles rows)
    `(table
       ((summary ,summary))
       (thead
         (tr
           ,@(map
               (lambda (title)
                 `(th ,title))
               row-titles)))
       (tbody ,@rows)
       (tfoot
         (tr
           ,@(map
               (lambda (title)
                 `(th ,title))
               row-titles)))))

  ;;;;;;;;;;;;
  ;;;; Helpers
  ;;;;;;;;;;;;

  ;; form-page-maker : (listof (list Symbol String)) -> 
  ;;                   String (listof Xexpr) -> String -> Xexpr
  ;; Make a page-builder for forms.
  (define/contract form-page-maker 
    ((listof (list/c symbol? string?)) . -> .
      (string? string? (listof any/c) . -> . (string? . -> . any)))
    (lambda (attribs)
      (lambda (title body)
        (lambda (k-url)
          (build-page title (apply
                                    (lambda (b)
                                      (form k-url attribs
                                            b (submit-button)))
                                    body))))))

)
