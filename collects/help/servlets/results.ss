
(module results mzscheme
  (require (lib "file.ss")
           (lib "list.ss")
           (lib "string.ss")
           (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           "../private/path.ss"
           "../private/docpos.ss"
           "../private/search.ss"
           "../private/manuals.ss"
           (lib "string-constant.ss" "string-constants"))
  
  (require "private/util.ss")
  (require "private/search-util.ss")
  (require "private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    (let ()
      ; doc subcollection name -> boolean
      
      (define search-sem (make-semaphore 1))
      
      (define (search-type->search-level st)
        (let loop ([n 0]
                   [lst (map car search-types)])
          (when (null? lst)
	    (raise 'bad-search-type))
          (if (string=? (car lst) st)
              n
              (loop (add1 n) (cdr lst)))))
      
      (define search-responses #f)

      ;; from what I can tell, this variable doesn't work as intended.
      ;; I've left it in for now, but this whole file needs to be rewritten.
      ;; -robby
      (define current-kind #f)

      (define last-header #f)
      
      (define max-reached #f)
      (define (build-maxxed-out k)
        (lambda ()
          (unless max-reached
            (set! max-reached #t)	    
            (set! search-responses
                  (cons `(B ,(color-with 
                              "red"
                              (string-constant plt:hd:search-stopped-too-many-matches)))
                        search-responses)))
          (k #f)))
      
      (define (add-header s key)
        (unless max-reached
          (set! last-header s)
          (set! search-responses
                (cons `(B ((STYLE "font-family:Verdana,Helvetica,sans-serif")) 
                          ,s)
                      (cons `(BR)
                            search-responses)))))
      
      (define (set-current-kind! s key)
        (set! current-kind
              (cadr (assoc s kind-types))))
      
      (define exp-web-root
        (explode-path 
         (normalize-path 
          (build-path (collection-path "mzlib") 'up))))
      (define web-root-len (length exp-web-root))
      
      ; given a manual path, convert to absolute Web path
      ; manual path is an anchored path to a collects/doc manual, never a servlet
      (define (tidy-manual-path manual-path)
        (let* ([exp-manual-path (explode-path manual-path)]
               [exp-tidy-path 
                (let loop ([path exp-manual-path]
                           [n 0])
                  (if (>= n web-root-len)
                      path
                      (loop (cdr path) (add1 n))))])
          ; insert internal slashes, make absolute by prepending slash
          (string-append "/" (fold-into-web-path exp-tidy-path))))
      
      (define (keyword-string? ekey)
        (and (string? ekey)
             (not (string=? ekey ""))))
      
      (define (pretty-label label ekey) 
        (if (keyword-string? ekey)
            `(FONT 
              ((FACE "monospace"))
              ; boldface keyword occurrences
              ,@(let ([mpos (regexp-match-positions (non-regexp ekey) label)])
                  (if mpos
                      (let* ([item (car mpos)]
                             [start (car item)]
                             [stop (cdr item)])
                        (list
                         (substring label 0 start)
                         `(B ,(substring label start stop))
                         (substring label stop
                                    (string-length label))))
                      (list label))))
            label))
      
      (define (maybe-extract-coll s)
        (let ([len (string-length s)])
          (if (and (> len 17)
                   (string=? (substring s 0 4) "the ")
                   (string=? (substring s (- len 11) len)
                             " collection"))
              (substring s 4 (- len 11))
              s)))
      
      (define no-anchor-format 
        (string-append
         "/servlets/doc-anchor.ss?"
         "file=~a&"
         "caption=~a&"
         "name=~a"))
      
      (define with-anchor-format 
        (string-append no-anchor-format "&offset=~a#temp"))
      
      (define (make-caption coll)
        (format "Documentation for the ~a collection" coll))
      
      (define (make-search-link href label src ekey)
        `(TABLE ((CELLSPACING "0")
                 (CELLPADDING "0"))
                (TR 
                 (TD
		  (DIV ((ALIGN "left-outdent"))
		       (A ((HREF ,href)) ,(pretty-label label ekey))
		       " in "
		       "\"" ,src "\"")))))
      
      ; page-label is #f or a string that labels an HTML anchor
      ; path is either an absolute pathname (possibly not normalized) 
      ; in the format of the native OS, or, in the case of Help Desk 
      ; servlets, a forward-slashified path beginning with "/servlets/"
      (define (make-anchored-path page-label path)
        (let ([normal-path 
               (if (servlet-path? path) 
                   path
                   (normalize-path path))])
          (if (and page-label
                   (string? page-label)
                   (not (or (string=? page-label "NO TAG") 
                            (regexp-match "\\?|&" page-label))))
              (string-append normal-path "#" page-label)
              normal-path)))
      
      (define (doc-txt? url)
        (let ([len (string-length url)])
          (and (> len 8)
               (string=? 
                (substring url
                           (- len 7)
                           len)
                "doc.txt"))))
      
      (define (make-html-href page-label path)
        (let ([anchored-path (make-anchored-path page-label path)])
          (cond
            [(servlet-path? anchored-path) 
             anchored-path]
            [(doc-txt? anchored-path) ; collection doc.txt
             (let ([maybe-coll (maybe-extract-coll last-header)])
               (format 
                no-anchor-format
                (hexify-string anchored-path)
                (hexify-string (make-caption maybe-coll))
                maybe-coll))]
            [else ; manual, so have absolute path
             (tidy-manual-path anchored-path)])))
      
      ; path is absolute pathname
      (define (make-text-href page-label path)
        (let* ([maybe-coll (maybe-extract-coll last-header)]
               [hex-path (hexify-string (normalize-path path))]
               [hex-caption (if (eq? maybe-coll last-header)
                                hex-path
                                (hexify-string (make-caption maybe-coll)))]
               [offset (or (and (number? page-label)
                                page-label)
                           0)])
          (format 
           with-anchor-format
           hex-path
           hex-caption
           (hexify-string maybe-coll)
           offset)))
      
      (define (html-entry? path)
        (and (not (suffixed? path "doc.txt"))
             (or (eq? current-kind 'html)
                 (suffixed? path ".html"))))
      
      (define (suffixed? path suffix)
        (let ([path-len (string-length path)]
              [suffix-len (string-length suffix)])
          (and (path-len . >= . suffix-len)
               (string=? (substring path 
                                    (- path-len suffix-len)
                                    path-len)
                         suffix))))
      
      (define (goto-lucky-entry ekey label src path page-label key)
        (let* ([href (if (html-entry? path)
                         (make-html-href page-label path)
                         (make-text-href page-label path))])
          (send/finish
           (redirect-to href))))
      
      (define (add-entry ekey label src path page-label key)
        (let* ([entry (if (html-entry? path)
                          (make-search-link 
                           (make-html-href page-label path)
                           label src ekey)
                          (make-search-link 
                           (make-text-href page-label path)
                           label src ekey))])
          (set! search-responses
                (cons entry search-responses))))
      
      (define (make-results-page search-string lang-name items regexp? exact?)
        (let-values ([(string-finds finds) (build-string-finds/finds search-string regexp? exact?)])
          `(HTML
            (HEAD ,hd-css
                  ,@hd-links
                  (TITLE "PLT Help Desk search results"))
            (BODY
             (FONT ((SIZE "+2"))
                   ,(color-with 
                     "blue" 
                     `(div
                       ,(if lang-name
                            `(b "Searched in manuals related to the language \""
                                ,lang-name
                                "\" for")
                            `(b "Search results for"))
                       ,@(map (lambda (sf) (format " \"~a\"" sf))
                              string-finds))))
             (BR)
             ,@items))))
      
      (define (search-results lucky? search-string search-type match-type manuals doc-txt? lang-name)
        (semaphore-wait search-sem)
        (set! search-responses '())
        (set! max-reached #f)
        (let* ([search-level (search-type->search-level search-type)]
               [regexp? (string=? match-type "regexp-match")]
               [exact-match? (string=? match-type "exact-match")]
               [key (gensym)]
               [result (let/ec k
                         (do-search search-string 
                                    search-level
                                    regexp?
                                    exact-match?
                                    manuals
                                    doc-txt?
                                    key
                                    (build-maxxed-out k)
                                    add-header
                                    set-current-kind!
                                    (if lucky? goto-lucky-entry add-entry)))]
               [html (make-results-page
                      search-string
                      lang-name
                      (if (string? result) ; error message
                          `((H2 ((STYLE "color:red")) ,result))
                          (reverse search-responses))
                      regexp? 
                      exact-match?)])
          (semaphore-post search-sem)
          html))
      
      (define empty-search-page
        `(HTML
          (HEAD
           (TITLE "Empty search string in PLT Help Desk"))
          (BODY
           (H2 "Empty search string"))))
      
      (define (lucky-search? bindings)
        (with-handlers ([not-break-exn? (lambda _ #f)])
          (let ([result (extract-binding/single 'lucky bindings)])
            (not (string=? result "false")))))
      
      (define (maybe-update-box b s)
        (unless (string=? s "")
          (set-box! b s)))
      
      (define (convert-manuals manuals)
        (cond
          [manuals
           (let ([parsed (read-from-string manuals)])
             (cond
               [(and (list? parsed)
                     (andmap symbol? parsed))
                (map symbol->string parsed)]
               [else (map car (find-doc-names))]))]
          [else (map car (find-doc-names))]))
      
      (let* ([bindings (request-bindings initial-request)]
             [maybe-get (lambda (sym)
                          (with-handlers ([not-break-exn? (lambda _ #f)])
                            (extract-binding/single sym bindings)))]
             [search-string (maybe-get 'search-string)]
             [search-type (maybe-get 'search-type)]
             [match-type (maybe-get 'match-type)]
             [manuals (maybe-get 'manuals)]
             [doc.txt (maybe-get 'doctxt)]
             [lang-name (maybe-get 'langname)])
        (cond
          [(or (not search-string) (= (string-length search-string) 0))
           empty-search-page]
          [else
           (search-results
            (lucky-search? bindings)
            search-string
            (or search-type "keyword-index")
            (or match-type "containing-match")
            (convert-manuals manuals)
            (cond
              [(not doc.txt) #t]
              [(equal? doc.txt "false") #f]
              [else #t])
            lang-name)])))))

