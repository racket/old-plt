(module util mzscheme
  (require (lib "file.ss")
	   (lib "list.ss")
	   (lib "xml.ss" "xml")
	   (lib "finddoc.ss" "help" "private")
	   (lib "string-constant.ss" "string-constants")
 	   (lib "plt-browser.ss" "help" "private")
	   (lib "docpos.ss" "help" "private"))

  (provide get-pref/default
	   get-bool-pref/default
	   put-prefs
	   cvs?
	   use-frames?
	   search-height-default
	   search-bg-default
	   search-text-default
	   search-link-default
	   use-frames-default
	   color-highlight
	   color-with
	   hexify-string
	   main-manual-page
           make-main-frameset
	   manual-entry
	   collection-doc-link
	   fold-into-web-path
	   color-choices
	   home-page
	   plt-version
           text-frame
	   format-collection-message
	   nl
	   make-javascript
	   redir-javascript
	   onload-redir)

  (define home-page 
    `(A ((HREF "/servlets/home.ss") (TARGET "_top"))
	,(string-constant plt:hd:home)))

  (define (get-pref/default pref default)
    (get-preference pref (lambda () default)))

  (define (get-bool-pref/default pref default)
    (let ([raw-pref (get-pref/default pref default)])
      (if (string=? raw-pref "false") #f #t)))

  (define (put-prefs names vals)
    (put-preferences names vals)) 

  (define search-height-default "85")
  (define search-bg-default "lightsteelblue")
  (define search-text-default "black")
  (define search-link-default "darkblue")
  (define use-frames-default "true")

  (define *the-highlight-color* "forestgreen")

  ; string xexpr ... -> xexpr
  (define (color-with color . s)
    `(FONT ((COLOR ,color)) ,@s))

  ; xexpr ... -> xexpr
  (define (color-highlight . s)
    (apply color-with *the-highlight-color* s))

  (define (cvs?)
    (directory-exists? 
     (build-path (collection-path "help") "CVS")))

  (define (use-frames?)
    (and (not (use-plt-browser?))
	 (get-bool-pref/default 'plt:hd:use-frames use-frames-default)))

  (define search-frame-servlet "/servlets/search.ss")
  
  (define (make-main-frameset search-string lower-url)
    (let ([search-height 
	   (get-pref/default 'plt:hd:search-height search-height-default)])
      `(FRAMESET ((ROWS ,(string-append search-height ",*")))
		 (FRAME ((NAME "search")
			 (SRC ,(if search-string
				   (string-append 
				    search-frame-servlet
				    "?search-string="
				    (hexify-string search-string))
				   search-frame-servlet))
			 (MARGINHEIGHT "2")
			 (MARGINWIDTH "2")))
		 (FRAME ((NAME "main")
			 (SRC ,lower-url))))))

  ; manual is doc collection subdirectory, e.g. "mred"
  (define (main-manual-page manual)
    (let* ([entry (assoc manual known-docs)]
	   [name (or (and entry (cdr entry))
		      manual)]
	   [main-page (build-path (collection-path "doc")
				  manual
				  "index.htm")]
	   [href (if (file-exists? main-page)
		     (string-append "/doc/" manual "/")
		     (string-append "/servlets/missing-manual.ss?"
				    "manual=" manual "&"
				    "name="
				    (hexify-string name)))])
      `(A ((HREF ,href)) ,name)))

  ; string string string -> xexpr
  ; man is manual name
  ; ndx is index into the manual
  ; txt is the link text
  (define (manual-entry man ndx txt)
    (with-handlers 
     ([void (lambda _
	      (color-with "red" 
			  `(B "[Bad manual entry: "
			      ,man "::" ,ndx "]")))])
     `(A ((HREF ,(finddoc-page man ndx)))
	 ,txt)))

  (define hexifiable '(#\: #\; #\? #\& #\% #\# #\< #\>))
  ; string -> string
  (define (hexify-string s)
    (apply string-append 
	   (map (lambda (c) 
		  (if (or (char-whitespace? c) (memq c hexifiable))
		      (format "%~X" (char->integer c))
		      (format "~a" c)))
		(string->list s))))

  ; string string -> xexpr
  (define (collection-doc-link coll txt)
    (let ([coll-file (build-path 
		      (collection-path coll) "doc.txt")])
      (if (file-exists? coll-file)
	  `(A ((HREF 
		,(format 
		  "/servlets/doc-anchor.ss?file=~a&name=~a&caption=Documentation for the ~a collection"
		  (hexify-string coll-file)
		  coll
		  coll)))
	      ,txt)
	  "")))

  ; (listof string) -> string
  ; result is forward-slashed web path
  ;  e.g. ("foo" "bar") -> "foo/bar"
  (define (fold-into-web-path lst)
      (foldr (lambda (s a)
	       (if a
		   (string-append s "/" a)
		   s))
	     #f
	     lst))

  (define color-choices
    '("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure"
      "beige" "bisque" "black" "blanchedalmond" "blue"
      "blueviolet" "brown" "burlywood" "cadetblue" "chartreuse"
      "chocolate" "coral" "cornflower" "cornsilk" "crimson" "cyan"
      "darkblue" "darkcyan" "darkgoldenrod" "darkgray"
      "darkgreen" "darkkhaki" "darkmagenta" "darkolivegreen"
      "darkorange" "darkorchid" "darkred" "darksalmon"
      "darkseagreen" "darkslateblue" "darkslategray"
      "darkturquoise" "darkviolet" "deeppink" "deepskyblue"
      "dimgray" "dodgerblue" "firebrick" "floralwhite"
      "forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold"
      "goldenrod" "gray" "green" "greenyellow" "honeydew"
      "hotpink" "indianred" "indigo" "ivory" "khaki" "lavender"
      "lavenderblush" "lawngreen" "lemonchiffon" "lightblue"
      "lightcoral" "lightcyan" "lightgoldenrodyellow"
      "lightgreen" "lightgray" "lightpink" "lightsalmon"
      "lightseagreen" "lightskyblue" "lightslategray"
      "lightsteelblue" "lightyellow" "lime" "limegreen" "linen"
      "magenta" "maroon" "mediumaquamarine" "mediumblue"
      "mediumorchid" "mediumpurple" "mediumseagreen"
      "mediumslateblue" "mediumspringgreen"
      "mediumturquoise" "mediumvioletred" "midnightblue"
      "mintcream" "mistyrose" "moccasin" "navajowhite" "navy"
      "oldlace" "olive" "olivedrab" "orange" "orangered" "orchid"
      "palegoldenrod" "palegreen" "paleturquoise"
      "palevioletred" "papayawhip" "peachpuff" "peru" "pink"
      "plum" "powderblue" "purple" "red" "rosybrown" "royalblue"
      "saddlebrown" "salmon" "sandybrown" "seagreen" "seashell"
      "sienna" "silver" "skyblue" "slateblue" "slategray" "snow"
      "springgreen" "steelblue" "tan" "teal" "thistle" "tomato"
      "turquoise" "violet" "wheat" "white" "whitesmoke" "yellow"
      "yellowgreen"))

  (define (text-frame)
    (if (use-frames?) "main" "_top"))

  (define (format-collection-message s)
    `(B ((STYLE "color:green")) ,s))

  (define nl (string #\newline))

  (define (make-javascript . ss)
    `(SCRIPT ((LANGUAGE "Javascript"))
	     ,(make-comment
	       (apply string-append 
		      nl
		      (map (lambda (s)
			     (string-append s nl))
			   ss)))))

  (define (plt-version)
    (let ([mz-version (version)]
	  [stamp-collection
	   (with-handlers
	    ([void (lambda _ #f)])
	    (collection-path "cvs-time-stamp"))])
      (if (and stamp-collection (file-exists? (build-path stamp-collection "stamp.ss")))
	  (format "~a-cvs~a" mz-version (dynamic-require '(lib "stamp.ss" "cvs-time-stamp") 'stamp))
	  mz-version)))

  (define (redir-javascript k-url)
    (make-javascript
     "function redir() {"
     (string-append
       "  document.location.href=\"" k-url "\"") 
     "}"))

  (define (onload-redir secs)
    (string-append 
     "setTimeout(\"redir()\","
     (number->string (* secs 1000))
     ")")))





