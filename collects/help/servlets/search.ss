(require (lib "unitsig.ss")
	 (lib "string.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "string-constant.ss" "string-constants")
         (lib "xml.ss" "xml"))

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

  (define (make-make-link color)
    (lambda (url label frame)
      `(A ((HREF ,url)
	   (TARGET ,frame))
	  ,(color-with color
		       `(FONT ((SIZE "-1")) ,label)))))

  (define make-search-link (make-make-link search-link-color))

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
    (HEAD (TITLE "PLT Help Desk search"))
    (BODY ((BGCOLOR ,search-bg))
	  (FORM ((ACTION "/servlets/results.ss")
		 (TARGET "main")
		 (METHOD "POST"))
		(TABLE ((CELLSPACING "0")
			(CELLPADDING "0"))
		 (TR ((VALIGN "top"))
		  (TD 
		   (TABLE
		    (TR 
		     (TD ((ALIGN "right"))
			 (B ,(color-with search-fg
					 (string-constant search-for)
					 ":")))
		     (TD 
		      (INPUT ((TYPE "text")
			      (NAME "search-string")
			      (VALUE "")
			      (SIZE "35")))
		      'nbsp
		      (INPUT ((TYPE "submit")
			      (NAME "search")
			      (VALUE ,(string-constant search))))
		      'nbsp 
		      (INPUT ((TYPE "submit")
			      (NAME "lucky")
			      (VALUE ,(string-constant lucky)))))
		     (TD 'nbsp 'nbsp))
		    (TR 
		     (TD ((ALIGN "right")) 
			 (B  ,(color-with search-fg (string-constant
						     options) ":")))
		     (TD ((COLSPAN "2"))
			 (SELECT ((NAME "search-type"))
				 ,@(map make-option
					search-types))
			 'nbsp 
			 (SELECT ((NAME "match-type"))
				 ,@(map make-option 
					match-types))))))
		  (TD 'nbsp)
		  (TD
		   (TABLE ((BGCOLOR ,search-bg)
			   (CELLSPACING "0") 
			   (CELLPADDING "0"))
			  (TR 
			   (TD
			    ,(make-search-link "/servlets/main.ss" 
					       (string-constant hd-home)
					       "main")))
			  (TR
			   (TD 
			    ,(make-search-link "/servlets/manuals.ss"
					       (string-constant 
						show-manuals)
					       "main")))
			   (TR 
			    (TD
			     ,(make-search-link "/servlets/bug-report.ss"
					       (string-constant 
						send-bug-report)
					       "_top")))

			   (TR 
			    (TD
			     ,(make-search-link "http://bugs.plt-scheme.org/query/"
					       (string-constant 
						query-bug-reports)
					       "main")))))))))))

