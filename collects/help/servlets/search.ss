(require (lib "unitsig.ss")
	 (lib "string.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "xml.ss" "xml")
	 (lib "search.ss" "help" "private"))

(require "private/util.ss")
(require "private/search-util.ss")

(unit/sig ()
  (import servlet^)

  (adjust-timeout! +inf.0)

  (define search-bg
    (get-pref/default 'search-bg search-bg-default))

  (define search-fg
    (get-pref/default 'search-fg search-text-default))

  (define search-link-color
    (get-pref/default 'search-link search-link-default))

  (define sys-link-color
    (get-pref/default 'sys-link sys-link-default))

  (define (make-search-link url label frame)
    `(A ((HREF ,url)
	 (TARGET ,frame))
	,(color-with search-link-color label)))

  (define (make-sys-link url label frame)
    `(A ((HREF ,url)
	 (TARGET ,frame))
	,(color-with sys-link-color
		     `(FONT ((SIZE "-1")) ,label))))

  (define (default-option? opt)
    (and (= 3 (length opt))
	 (eq? (caddr opt) '*)))

  (define (make-option opt)
    (let* ([value-attrib `(VALUE ,(car opt))]
	   [attribs (if (default-option? opt)
			`(,value-attrib
			  (SELECTED "true"))
			`(,value-attrib))]
	   [txt (cadr opt)])
      `(OPTION ,attribs ,txt)))

  `(HTML 
    (BODY ((BGCOLOR ,search-bg))
	  (FORM ((ACTION "/servlets/results.ss")
		 (TARGET "main")
		 (METHOD "POST"))
		(TABLE  
		 (TR 
		  (TD 
		   (TABLE 
		    (TR  
		     (TD ((ALIGN "right"))
			 (B ,(color-with search-fg
					 "Search for:")))
		     (TD 
		      (INPUT ((TYPE "text")
			      (NAME "search-string")
			      (VALUE "")
			      (SIZE "35"))))
		     (TD 
		      (INPUT ((TYPE "submit")
			      (NAME "search")
			      (VALUE "Search")))
		      'nbsp 'nbsp
		      (INPUT ((TYPE "submit")
			      (NAME "lucky")
			      (VALUE "Lucky!"))))
		     (TD ((WIDTH "20")) 'nbsp))
		    (TR 
		     (TD ((ALIGN "right")) 
			 (B  ,(color-with search-fg "Options:")))
		     (TD ((COLSPAN "3"))
			 (SELECT ((NAME "search-type"))
				 ,@(map make-option
					search-types))
			 'nbsp 'nbsp
			 (SELECT ((NAME "match-type"))
				 ,@(map make-option 
					match-types))))
		    (TR 
		     (TD 'nbsp)
		     (TD ((COLSPAN "2"))
			 ,(make-sys-link
			   "/servlets/hd-config.ss"
			   "Configure Help Desk"
			   "_top")
			 'nbsp 'nbsp
			 ,(make-sys-link
			   "/servlets/shutdown.ss"
			   "Shutdown Help Desk server"
			   "_top")))))
		  (TD
		   (TABLE ((BGCOLOR ,search-bg)
			   (CELLSPACING "0")
			   (CELLPADDING "0"))
			  (TR 
			   (TD
			    ,(make-search-link "/servlets/main.ss" "Help Desk home"
					       "main")))
			  (TR
			   (TD 
			    ,(make-search-link "/servlets/manuals.ss"
					       "Show manuals"
					       "main")))
			   (TR 
			    (TD
			     ,(make-search-link "/servlets/bug-report.ss"
					       "Send bug report"
					       "_top")))

			   (TR 
			    (TD
			     ,(make-search-link "http://bugs.plt-scheme.org/query/"
					       "Query bug reports"
					       "main")))))))))))
