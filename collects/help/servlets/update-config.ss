(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "file.ss"))

(require "private/util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define names '(search-height search-bg search-fg search-link))

  (define complete-page	
    `(HTML 
      (HEAD ,hd-css)
      (BODY
       (H1 "Configuration complete")
       (P)
       ,home-page)))

  (define (make-error-page msgs)
    `(HTML 
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
	 [vals (map extract-fun names)])

    (when (not (string->number (car vals)))
	  (add-error!
	   (format "String height \"~a\" not a number" (car vals))))

    (unless (errors?)
	    (with-handlers
	     ([void (lambda _ 
		      (add-error! "Error saving configuration"))])
	     (put-prefs names vals)))

    (if (errors?)
	 (make-error-page error-msgs)
	 complete-page)))







