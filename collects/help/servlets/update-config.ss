(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "file.ss"))

(require "private/util.ss")
(require "private/headelts.ss")

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  ; names as sent from FORM
  (define names '(search-bg search-fg search-link))
  ; names of corresponding preferences
  (define pref-names '(plt:hd:search-bg plt:hd:search-fg plt:hd:search-link))

  (define (make-error-page msgs)
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
	    (TITLE "PLT Help Desk configuration error"))
      (BODY
       (H1 ((STYLE "color:red"))
	   "Configuration error")
       (P)
       (UL
       ,@(map 
	  (lambda (s)
	    `(LI ,s))
	  msgs))
       (P)
       (A ((HREF "/servlets/hd-config.ss"))
	  "Try configuration again")
       'nbsp 'nbsp 'nbsp 'nbsp
       ,home-page)))

  (define error-msgs '())

  (define (errors?)
    (not (null? error-msgs)))

  (define (add-error! s)
    (set! error-msgs
	  (cons s error-msgs)))

  (let* ([bindings (request-bindings initial-request)]
	 [extract-fun (lambda (sym)
			(extract-binding/single sym bindings))]
	 [vals (map extract-fun names)]
	 [use-frames-val (with-handlers 
			  ([void (lambda _ "false")])
			  (extract-fun 'use-frames))]
	 [search-height-val (with-handlers 
			     ([void (lambda _ #f)])
			     (extract-fun 'search-height))])

    (when (and search-height-val 
	       (not (string->number search-height-val)))
	  (add-error!
	   (format "Search height \"~a\" not a number" search-height-val)))

    (unless (errors?)
	    (with-handlers
	     ([void (lambda _ 
		      (add-error! "Error saving configuration"))])
	     (put-prefs pref-names vals)
	     (when search-height-val
		   (put-prefs (list 'plt:hd:search-height)
			      (list search-height-val)))
	     (put-prefs (list 'plt:hd:use-frames)
			(list use-frames-val))))

    (if (errors?)
	(send/finish 
	 (make-error-page error-msgs))
	(redirect-to "/servlets/home.ss"))))








