(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "servlet-helpers.ss" "web-server"))

(require "private/util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define search-height
    (get-pref/default 'search-height search-height-default))

  ; three ways to get here
  ; - by starting Help Desk anew
  ; - by a search from an external program (like DrScheme), using
  ;    search-for-docs
  ; - by a manual search from an external program, using 
  ;    goto-manual-link
  ; in latter two cases, bottom frame shows search results

  (let* ([bindings (request-bindings initial-request)]
	 [get-binding (lambda (sym)
			(with-handlers
			 ([void (lambda _ #f)]) 
			 (extract-binding/single sym bindings)))]
	 [search-string (get-binding 'search-string)]
	 [search-type (get-binding 'search-type)]
	 [match-type (get-binding 'match-type)]
	 [lucky? (get-binding 'lucky)]
	 [hd-url (get-binding 'hd-url)])
  `(HTML 
    (HEAD ,hd-css
	  (TITLE "PLT Help Desk"))
    (FRAMESET ((ROWS ,(string-append search-height ",*")))
	      (FRAME ((NAME "search")
		      (SRC "/servlets/search.ss")
		      (MARGINHEIGHT "2")
		      (MARGINWIDTH "2")))
	      (FRAME ((NAME "main")
		      (SRC
		       ,(cond
			 [hd-url hd-url]     ;; from goto-manual-link
			 [(and search-string ;; from search-for-docs
			       search-type
			       match-type
			       lucky?)  ; pass args along to bottom frame
			  (format (string-append 
				   "/servlets/results.ss?"
				   "search-string=~a&"
				   "search-type=~a&"
				   "match-type=~a&"
				   "lucky=~a")
				  (hexify-string search-string)
				  search-type
				  match-type
				  lucky?)]
			 [else ; just the main page
			  "/servlets/main.ss"]))))))))







