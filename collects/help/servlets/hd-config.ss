(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "file.ss")
	 (lib "etc.ss")
	 (lib "string-constant.ss" "string-constants")
	 (lib "xml.ss" "xml")
	 (lib "url.ss" "net")

(require "private/util.ss"
	 "private/hd-css.ss"
	 "private/external.ss")

(unit/sig ()
  (import servlet^)

  (check-external 	
   send/finish
   (url-path (request-uri initial-request)))

  (define search-height 
    (get-pref/default 'plt:hd:search-height search-height-default))
  (define search-bg-color 
    (get-pref/default 'plt:hd:search-bg search-bg-default))
  (define search-text-color 
    (get-pref/default 'plt:hd:search-fg search-text-default))
  (define search-link-color 
    (get-pref/default 'plt:hd:search-link search-link-default))

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
	 "function updateSelection(item,val) {"
	 " document.getElementById(item).selectedIndex = val"
	 "}"
	 "function resetValues() {"
	 (js-color-funcall "updateBgColor" search-bg-color)
	 (js-color-funcall "updateTextColor" search-text-color)
	 (js-color-funcall "updateLinkColor" search-link-color)
	 (js-color-funcall "updateTextColor" search-text-color)
         "}"
	 "function defaultValues() {"
	 (js-color-funcall "updateBgColor" search-bg-default)
	 (js-color-funcall "updateTextColor" search-text-default)
	 (js-color-funcall "updateLinkColor" search-link-default)
	 (js-color-funcall "updateTextColor" search-text-default)
         (js-color-selection "search_bg_select" search-bg-default)
         (js-color-selection "search_text_select" search-text-default)
         (js-color-selection "search_link_select" search-link-default)
	 (string-append
	  " document.getElementById(\"search_height\").value="
	  search-height-default)
         "}"
         "function showSearchPaneHeight() {"
	 " var elt = document.getElementById(\"search_height\")"
	 " if (document.getElementById(\"use_frames\").checked==true) {"
	 "  elt.disabled=false"
	 " } else {"
	 "  elt.disabled=true"
	 " }"
         "}")
	(TITLE "PLT Help Desk configuration")
	,hd-css)
      (BODY ((onLoad "showSearchPaneHeight()"))
       (H1 ,(string-constant plt:hd:configuration))
       (P)
       (FORM ((ACTION "/servlets/update-config.ss")
	      (METHOD "POST"))
	     (TABLE ((ALIGN "center"))
	      (TR (TD
		   (TABLE ((ALIGN "center"))
		    (TR
		     (TD (INPUT ,(append 
				  `((TYPE "checkbox")
				    (NAME "use-frames")
				    (ID "use_frames")
				    (onClick "showSearchPaneHeight()"))
				  (if (use-frames?)
				      `((CHECKED "true") (VALUE "true"))
				      `()))))
		     (TD (B ,(string-constant plt:hd:use-html-frames)))))))
	      (TR (TD 'nbsp))
	      (TR (TD
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
					,(string-constant plt:hd:search-pane-options))))
			  (TR
			   (TD ((ALIGN "right"))
			       (B ,(string-constant plt:hd:height) ":")) 
			   (TD (INPUT ((TYPE "text")
				       (NAME "search-height")
				       (ID "search_height")
				       (VALUE ,search-height)
				       (SIZE "5")))
			       'nbsp
			       (FONT ((SIZE "-1")) ,(string-constant plt:hd:pixels))))
			  (TR (TD ((ALIGN "right")) (B ,(string-constant 
							 plt:hd:bg-color) ":"))
			      (TD (SELECT ((NAME "search-bg")
					   (ID "search_bg_select")
					   (onChange "updateBgColor(this.value)"))
					  ,@(map (make-option search-bg-color)
						 color-choices))))
			  (TR (TD ((ALIGN "right")) (B ,(string-constant plt:hd:text-color) ":"))
			      (TD (SELECT ((NAME "search-fg")
					   (ID "search_text_select")
					   (onChange "updateTextColor(this.value)"))
					  ,@(map (make-option search-text-color)
						 color-choices))))
			  (TR (TD ((ALIGN "right")) (B ,(string-constant plt:hd:link-color) ":"))
			      (TD (SELECT ((NAME "search-link")
					   (ID "search_link_select")
					   (onChange "updateLinkColor(this.value)"))
					  ,@(map (make-option search-link-color)
						 color-choices))))))))
	     (P)
	     (CENTER
	      (TABLE ((BGCOLOR ,search-bg-color)
		      (ID "sample_table")
		      (CELLSPACING "0")
		      (CELLPADDING "4"))
		     (TR (TD 
			  (FONT ((COLOR ,search-text-color)
				 (ID "sample_text"))
				,(string-constant
				  plt:hd:text-sample))))
		     (TR (TD (FONT ((COLOR ,search-link-color)
				    (ID "sample_link")
				    (STYLE "text-decoration:underline"))
				   ,(string-constant
				     plt:hd:link-sample))))))
	     (P)
	     (CENTER
	      (TABLE ((BGCOLOR "white")
		      (CELLSPACING "0")
		      (CELLPADDING "4")
		      (WIDTH "50%"))
		     (TR (TD 
			  ,(string-constant plt:hd:javascript-note)))))
	     (P)
	     (TABLE ((ALIGN "center"))
		    (TR 
		     (TD (INPUT ((TYPE "submit")
				 (VALUE ,(string-constant plt:hd:save-changes)))))
		     (TD 'nbsp 'nbsp 'nbsp 'nbsp)
		     (TD (INPUT ((TYPE "reset")
				 (VALUE ,(string-constant plt:hd:reset))
				 (onClick "resetValues()"))))
		     (TD 'nbsp 'nbsp 'nbsp 'nbsp)
		     (TD (INPUT ((TYPE "button")
				 (VALUE ,(string-constant plt:hd:defaults))
				 (onClick "defaultValues()"))))))
	     (P)
	     (CENTER 
	      ,home-page)))))

  config-page)






























































      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
