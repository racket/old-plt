(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "servlet-helpers.ss" "web-server"))

(require "private/util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define search-height
    (get-pref/default 'search-height search-height-default))

  ; two ways to get here
  ; - by starting Help Desk anew
  ; - by a search from an external program (like DrScheme)
  ; in latter case, bottom frame shows search results

  (let* ([bindings (request-bindings initial-request)]
	 [get-binding (lambda (sym)
			(with-handlers
			 ([void (lambda _ #f)]) 
			 (extract-binding/single sym bindings)))]
	 [search-string (get-binding 'search-string)]
	 [search-type (get-binding 'search-type)]
	 [match-type (get-binding 'match-type)]
	 [lucky? (get-binding 'lucky)])

  `(HTML 
    (HEAD ,hd-css)
    (FRAMESET ((ROWS ,(string-append search-height ",*")))
	      (FRAME ((NAME "search")
		      (SRC "/servlets/search.ss")
		      (MARGINHEIGHT "2")
		      (MARGINWIDTH "2")))
	      (FRAME ((NAME "main")
		      ,(if (and search-string
				search-type
				match-type
				lucky?)
                            ; pass args along to bottom frame
			   `(SRC ,(format (string-append 
					   "/servlets/results.ss?"
					   "search-string=~a&"
					   "search-type=~a&"
					   "match-type=~a&"
					   "lucky=~a")
					  (hexify-string search-string)
					  search-type
					  match-type
					  lucky?))
					; just the main page
			   `(SRC "/servlets/main.ss"))))))))







