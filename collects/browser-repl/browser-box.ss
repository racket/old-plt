(require (lib "class.ss")
         (lib "mred.ss" "mred")
         (lib "htmltext.ss" "browser")
         (lib "external.ss" "browser")
         (lib "url.ss" "net")
         (lib "xml.ss" "xml")
         )


(define myprint printf)

(define browser-snip%
  (class editor-snip%
    (init-field initial-url)
    (init-field img-ok?)
    (init-field eval-ok?)
       
    (define/override (copy)
      this)
    
    (define/public (post-url a-url post-data)
      (send editor post-url a-url post-data))
    
    (field
     [editor (new browser-editor%
                  (initial-url initial-url)
                  (img-ok? img-ok?)
                  (eval-ok? eval-ok?))])
    
    (super-new
     (editor editor))))

(define banner-xexpr
  (let ([op (open-output-string)])
    (write-xml/content
     (xexpr->xml
      `(html (body (a ([mzscheme "(printf |hello~n|)"])
                      (h1 "This is the banner expression")))))
      op)
    (lambda ()
      (open-input-string (get-output-string op)))))
    
(define browser-editor%
  (class (html-text-mixin text%)
    (init-field initial-url)
    (init-field img-ok?)
    (init-field eval-ok?)
    (inherit set-clickback)
    
    (define the-url initial-url)
    
    (define/override (get-url)
      the-url)
    
    (define/override (post-url url-str post-data)
      (let ([new-url (combine-url/relative the-url url-str)])
        (let ([ip (post-pure-port new-url post-data)])
          (set! the-url new-url)
          (render-html-to-text ip this img-ok? eval-ok?)
          )))
    
    (define/override (add-link pos end-pos url-str)
      (let ([new-url (combine-url/relative the-url url-str)])
        (set-clickback pos end-pos (lambda (e start-pos eou-pos)
                                     (let ([ip (get-pure-port new-url)])
                                       (render-html-to-text (banner-xexpr) this img-ok? eval-ok?)
                                       (set! the-url new-url)
                                       (render-html-to-text ip this img-ok? eval-ok?)
                                       )))))
    
    (define/override (add-scheme-callback pos end-pos scheme)
      (set-clickback pos end-pos (lambda (e start-pos eou-pos)
                                   (myprint "You clicked me: scheme = ~s~n" scheme)
                                   (eval (read (open-input-string scheme))))))
    
    (super-instantiate ())
    
    (let ([ip (get-pure-port initial-url)])
      (render-html-to-text ip this img-ok? eval-ok?))
    ))

  
;; ************************************************************

(define f (new frame% (label "test") (width 700) (height 500)))
(define e (new text%))
(define c (new editor-canvas% (editor e) (parent f)))
(define bs (new browser-snip% (initial-url (string->url "http://www.plt-scheme.org/"))
                (img-ok? #t) (eval-ok? #t)))
(send e insert bs)
(send f show #t)
    
    
    
;; ************************************************************
;; for the interactions
;(new browser-snip% (initial-url (string->url "http://www.plt-scheme.org"))
;     (img-ok? #t) (eval-ok? #t))
;          
    
    