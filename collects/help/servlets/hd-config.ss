(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "file.ss")
	 (lib "etc.ss")
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

  (define (make-labelled-option default)
    (lambda (s-val s-label)
      (if (string=? s-val default)
      `(OPTION ((SELECTED "true")
		(VALUE ,s-val)) ,s-label)
      `(OPTION ((VALUE ,s-val)) ,s-label))))

  (define (make-option default)
    (let ([f (make-labelled-option default)])
      (lambda (s)
	(f s s))))

  (define (color->index color) 
    (let loop ([ndx 0]
	       [colors color-choices])
      (if (null? colors)
	  0
	  (if (string=? color (car colors))
	      ndx
	      (loop (add1 ndx) (cdr colors))))))

  (define (js-color-funcall name color) 
    (string-append " " name "(\"" color "\")"))

  (define (js-color-selection name color) 
    (string-append " updateSelection(\"" name "\","
		   (number->string (color->index color))
		   ")"))

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
	 "function updateSelection(item,val) {"
	 " document.getElementById(item).selectedIndex = val"
	 "}"
	 "function resetValues() {"
	 (js-color-funcall "updateBgColor" search-bg-color)
	 (js-color-funcall "updateTextColor" search-text-color)
	 (js-color-funcall "updateLinkColor" search-link-color)
	 (js-color-funcall "updateSysLinkColor" sys-link-color)
	 (js-color-funcall "updateTextColor" search-text-color)
         "}"
	 "function defaultValues() {"
	 (js-color-funcall "updateBgColor" search-bg-default)
	 (js-color-funcall "updateTextColor" search-text-default)
	 (js-color-funcall "updateLinkColor" search-link-default)
	 (js-color-funcall "updateSysLinkColor" sys-link-default)
	 (js-color-funcall "updateTextColor" search-text-default)
         (js-color-selection "search_bg_select" search-bg-default)
         (js-color-selection "search_text_select" search-text-default)
         (js-color-selection "search_link_select" search-link-default)
         (js-color-selection "search_sys_link_select" sys-link-default)
	 (string-append
	  " document.getElementById(\"search_height\").value="
	  search-height-default)
         "}")
	(TITLE "PLT Help Desk configuration")
	,hd-css)
      (BODY 
       (H1  "PLT Help Desk configuration")
       (P)
       (FORM ((ACTION "/servlets/update-config.ss")
	      (METHOD "POST"))
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
		     (TD ((ALIGN "right"))
			 (B "Height:")) 
		     (TD (INPUT ((TYPE "text")
				 (NAME "search-height")
				 (ID "search_height")
				 (VALUE ,search-height)
				 (SIZE "5")))
			 'nbsp
			 (FONT ((SIZE "-1")) "pixels")))
		    (TR (TD ((ALIGN "right")) (B "Background color:"))
			(TD (SELECT ((NAME "search-bg")
	                             (ID "search_bg_select")
				     (onChange "updateBgColor(this.value)"))
				    ,@(map (make-option search-bg-color)
					   color-choices))))
		    (TR (TD ((ALIGN "right")) (B "Text color:"))
			(TD (SELECT ((NAME "search-fg")
				     (ID "search_text_select")
				     (onChange "updateTextColor(this.value)"))
				    ,@(map (make-option search-text-color)
					   color-choices))))
		    (TR (TD ((ALIGN "right")) (B "Link color:"))
			(TD (SELECT ((NAME "search-link")
				     (ID "search_link_select")
				     (onChange "updateLinkColor(this.value)"))
				    ,@(map (make-option search-link-color)
					   color-choices))))
		    (TR (TD ((ALIGN "right")) (B "System link color:"))
			(TD (SELECT ((NAME "sys-link")
				     (ID "search_sys_link_select")
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
			   "The selections you make will be shown here "
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
				 (onClick "resetValues()"))))
		     (TD 'nbsp 'nbsp 'nbsp 'nbsp)
		     (TD (INPUT ((TYPE "button")
				 (VALUE "Defaults")
				 (onClick "defaultValues()"))))))
	     (P)
	     (CENTER 
	      ,home-page)))))
  
  config-page)








  