(module search-pane mzscheme

  (require (lib "string-constant.ss" "string-constants")
	   (lib "etc.ss"))
  (require "search-util.ss")
  (require "util.ss")

  (provide search-pane)
  
  (define (search-bg)
    (get-pref/default 'plt:hd:search-bg search-bg-default))

  (define (search-fg)
    (get-pref/default 'plt:hd:search-fg search-text-default))

  (define (search-link-color)
    (get-pref/default 'plt:hd:search-link search-link-default))

  (define (make-make-link color-fun)
    (lambda (url label frame)
      `(A ((HREF ,url)
	   (TARGET ,frame))
	  ,(color-with (color-fun)
		       `(FONT ((SIZE "-1")) ,label)))))

  (define make-search-link (make-make-link search-link-color))

  (define (default-option? opt)
    (and (= 3 (length opt))
	 (eq? (caddr opt) '*)))

  (define (make-option curr opt)
    (let* ([val (car opt)]
	   [value-attrib `(VALUE ,val)]
	   [attribs (if (or (and curr
				 (string=? curr val))
			    (and (not curr) 
				 (default-option? opt)))
			`(,value-attrib
			  (SELECTED "true"))
			`(,value-attrib))]
	   [txt (cadr opt)])
      `(OPTION ,attribs ,txt)))

  (define (search-pane-form s)
    `(FORM ((ACTION "/servlets/results.ss")
	    (TARGET ,(text-frame))
	    (METHOD "POST"))
	   (TABLE ((CELLSPACING "0")
		   (CELLPADDING "0"))
		  (TR ((VALIGN "top"))
		      (TD 
		       (TABLE
			(TR 
			 (TD ((ALIGN "right"))
			     (B ,(color-with (search-fg)
					     (string-constant plt:hd:search-for)
					     ":")))
			 (TD ((NOWRAP "true"))
			     (INPUT ((TYPE "text")
				     (NAME "search-string")
				     (VALUE ,s)
				     (SIZE "35")))
			     'nbsp
			     (INPUT ((TYPE "submit")
				     (NAME "search")
				     (VALUE ,(string-constant plt:hd:search))))
			     'nbsp 
			     (INPUT ((TYPE "submit")
				     (NAME "lucky")
				     (VALUE ,(string-constant plt:hd:lucky)))))
			 (TD 'nbsp 'nbsp))
			(TR 
			 (TD ((ALIGN "right")) 
			     (B  ,(color-with (search-fg) (string-constant
							 plt:hd:options) ":")))
			 (TD ((COLSPAN "2")
			      (NOWRAP "true"))
			     (SELECT ((NAME "search-type"))
				     ,@(map (lambda (st)
					      (make-option
					       (unbox curr-search-type-box)
					       st))
					    search-types))
			     'nbsp 
			     (SELECT ((NAME "match-type"))
				     ,@(map (lambda (mt)
					      (make-option 
					       (unbox curr-match-type-box)
					       mt))
					    match-types))))))
		      (TD 'nbsp)
		      (TD
		       (TABLE ((BGCOLOR ,(search-bg))
			       (CELLSPACING "0") 
			       (CELLPADDING "0"))
			      (TR 
			       (TD
				,(make-search-link "/servlets/home.ss" 
						   (string-constant plt:hd:home)
						   "_top")))
			      (TR
			       (TD 
				,(make-search-link "/servlets/manuals.ss"
						   (string-constant 
						    plt:hd:show-manuals)
						   (text-frame))))
			      (TR 
			       (TD
				,(make-search-link "/servlets/bug-report.ss"
						   (string-constant 
						    plt:hd:send-bug-report)
						   "_top")))
			      
			      (TR 
			       (TD
				,(make-search-link "http://bugs.plt-scheme.org/query/"
						   (string-constant 
						    plt:hd:query-bug-reports)
						   "_top")))))))))

  (define search-pane
    (opt-lambda ([search-string ""])
      `(TABLE ((CELLPADDING "4")
	       (CELLSPACING "0")
	       (BGCOLOR ,(search-bg))
	       (WIDTH "100%"))
	      (TR (TD ,(search-pane-form search-string)))))))


  





