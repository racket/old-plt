(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "file.ss")
	 (lib "xml.ss" "xml"))

(require "private/util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define search-height 
    (get-pref/default 'search-height search-height-default))
  (define search-bg-color 
    (get-pref/default 'search-bg search-bg-default))
  (define search-text-color 
    (get-pref/default 'search-fg search-text-default))
  (define search-link-color 
    (get-pref/default 'search-link search-link-default))
  (define sys-link-color 
    (get-pref/default 'sys-link sys-link-default))
  (define new-browser
    (get-pref/default 'new-browser new-browser-default))

  (define (make-option default)
    (lambda (s)
      (if (string=? s default)
      `(OPTION ((SELECTED "true")
		(VALUE ,s)) ,s)
      `(OPTION ((VALUE ,s)) ,s))))

  (define config-page
    `(HTML 
      (HEAD
       ,(make-javascript
	 "function updateBgColor(new_color) {"
	 " document.getElementById(\"sample_table\").style.backgroundColor=new_color"
	 "}"
	 "function updateColor(item,new_color) {"
	 " document.getElementById(item).style.color=new_color"
	 "}"
	 "function updateTextColor(new_color) {"
	 " updateColor(\"sample_text\",new_color)"
	 "}"
	 "function updateLinkColor(new_color) {"
	 " updateColor(\"sample_link\",new_color)"
	 "}"
	 "function updateSysLinkColor(new_color) {"
	 " updateColor(\"sample_syslink\",new_color)"
	 "}"
	 "function resetValues() {"
	 (string-append " updateBgColor(\"" search-bg-color "\")")
	 (string-append " updateTextColor(\"" search-text-color "\")")
	 (string-append " updateLinkColor(\"" search-link-color "\")")
	 (string-append " updateSysLinkColor(\"" sys-link-color "\")")
	 (string-append " updateTextColor(\"" search-text-color "\")")
         "}")
	,hd-css)
      (BODY 
       (H1  "Help Desk configuration")
       (P)
       (FORM ((ACTION "/servlets/update-config.ss")
	      (METHOD "POST"))
	     (TABLE ((ALIGN "center"))
		    ,(let ([attrs '((TYPE "checkbox")
			      (NAME "new-browser")
			      (VALUE "dummy"))])
		       `(TR (TD
			     (INPUT ,(if new-browser 
					 (cons '(CHECKED "true") attrs)
					 attrs)))
			    (TD (B "Request new window for Help Desk browser")))))
	     (P)
	     (TABLE ((BGCOLOR "white")
		     (ALIGN "center")
		     (BORDER "2")
		     (BORDERCOLOR "black")
		     (CELLPADDING "4")
		     (CELLSPACING "2")
		     (COLS "2"))
		    (TR (TH ((ALIGN "center")
			     (COLSPAN "2")) 
			    (FONT ((FACE "serif")
				   (SIZE "+2"))
				  "Search frame options")))
		    (TR
		     (TD ((ALIGN "right")) (B "Height:")) 
		     (TD (INPUT ((TYPE "text")
				 (NAME "search-height")
				 (VALUE ,search-height)
				 (SIZE "5")))))
		    (TR (TD ((ALIGN "right")) (B "Background color:"))
			(TD (SELECT ((NAME "search-bg")
				     (onChange "updateBgColor(this.value)"))
				    ,@(map (make-option search-bg-color)
					   color-choices))))
		    (TR (TD ((ALIGN "right")) (B "Text color:"))
			(TD (SELECT ((NAME "search-fg")
				     (onChange "updateTextColor(this.value)"))
				    ,@(map (make-option search-text-color)
					   color-choices))))
		    (TR (TD ((ALIGN "right")) (B "Link color:"))
			(TD (SELECT ((NAME "search-link")
				     (onChange "updateLinkColor(this.value)"))
				    ,@(map (make-option search-link-color)
					   color-choices))))
		    (TR (TD ((ALIGN "right")) (B "System link color:"))
			(TD (SELECT ((NAME "sys-link")
				     (onChange "updateSysLinkColor(this.value)"))
				    ,@(map (make-option sys-link-color)
					   color-choices)))))
	     (P)
	     (CENTER
	      (TABLE ((BGCOLOR ,search-bg-color)
		      (ID "sample_table")
		      (CELLSPACING "0")
		      (CELLPADDING "4"))
		     (TR (TD 
			  (FONT ((COLOR ,search-text-color)
				 (ID "sample_text"))
				   "Search frame text appears in this color")))
		     (TR (TD (FONT ((COLOR ,search-link-color)
				    (ID "sample_link")
				    (STYLE "text-decoration:underline"))
				   "Search frame links appear in this color")))
		     (TR (TD (FONT ((COLOR ,sys-link-color)
				    (ID "sample_syslink")
				    (STYLE "text-decoration:underline"))
				   "System links appear in this color")))))
	     (P)
	     (CENTER
	      (TABLE ((BGCOLOR "white")
		      (CELLSPACING "0")
		      (CELLPADDING "4")
		      (WIDTH "50%"))
		      (TR (TD 
			   "The colors above will change in response "
			   "to your selections "
			   "if you have Javascript enabled and "
			   "a recent, standards-compliant browser."))))
	     (P)
	     (TABLE ((ALIGN "center"))
		    (TR 
		     (TD (INPUT ((TYPE "submit")
				 (VALUE "Save changes"))))
		     (TD 'nbsp 'nbsp 'nbsp 'nbsp)
		     (TD (INPUT ((TYPE "reset")
				 (VALUE "Reset")
				 (onClick "resetValues()"))))))
	     (P)
	     (CENTER 
	      ,home-page)))))
  
  config-page)










  