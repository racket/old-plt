(module util mzscheme
  (require (lib "file.ss")
	   (lib "etc.ss")
	   (lib "xml.ss" "xml")
	   (lib "url.ss" "net")
           ; help-desk here gives load cycle
 	   (lib "finddoc.ss" "help" "private"))
	
  (provide get-pref/default
	   put-prefs
	   cvs?
	   search-height-default
	   search-bg-default
	   search-text-default
	   search-link-default
	   sys-link-default
	   color-highlight
	   color-with
	   hexify-string
	   manual-entry
	   collection-doc-link
	   color-choices
	   password-file
	   home-page
	   plt-version
	   nl
	   make-javascript
	   redir-javascript
	   onload-redir)

  (define home-page 
    `(A ((HREF "/servlets/home.ss") (TARGET "_top"))
	"Help Desk home"))

  (define password-file
    (build-path (collection-path "doc") "help" "passwd"))

  (define (get-pref/default pref default)
    (get-preference pref  (lambda () default)))

  (define (put-prefs names vals)
    (put-preferences names vals)) 

  (define search-height-default "130")
  (define search-bg-default "palegreen")
  (define search-text-default "black")
  (define search-link-default "darkblue")
  (define sys-link-default "dimgray")

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

  ; string string string -> xexpr
  ; man is manual name
  ; ndx is index into the manual
  ; txt is the link text
  (define (manual-entry man ndx txt)
    (let ([fname (with-handlers
		  ([void (lambda _ #f)])
		  (finddoc-page-anchor man ndx))])
      (if fname
	  `(A ((HREF ,(string-append 
		       "/doc/"
		       man
		       "/"
		       fname)))
	      ,txt)
	  (color-with "red" 
		      `(B "[Bad manual entry: "
			  ,man "::" ,ndx)))))

  (define hexifiable '(#\: #\?))
  ; string -> string
  (define (hexify-string s)
    (apply string-append 
	   (map (lambda (c) 
		  (if (or (char-whitespace? c) (member c hexifiable))
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





