(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "manuals.ss" "help" "private"))

(require "private/hd-css.ss")

(define doc-root (format "http://download.plt-scheme.org/doc/~a"
			   (version)))

(define (no-manual manual label)
  (let ([html-url (format "~a/~a/index.htm" doc-root manual)]
	[plt-url (format "~a/bundles/~a-doc.plt" doc-root manual)])
    `(HTML
      (HEAD ,hd-css)
      (BODY ((BGCOLOR "white")) 
       ,(color-with "red"
		    `(H1 "Documentation missing"))
       (P)
       "You tried to access documentation for "
       ,manual ". "
       "The documentation is not installed on this "
       "machine, probably because it is not part of the "
       "standard DrScheme distribution."
       (P)
       (H2 "To read the documentation online")
       (P)
       (UL
	(LI 
	 (A ((HREF ,html-url)) "Click here")))
       (H2 "To download and install the documentation")
       (P)
       (UL 
	   (LI 
	       (A ((HREF 
		    ,(format "/servlets/download-manual.ss?manual=~a&label=~a"
			    manual (hexify-string label))))
		  "Click here")))
       (P)
       (H2 "Just download the documentation (no installation)")
       (P)
       (UL 
	   (LI 
	       (A ((HREF ,plt-url))
		  "Click here")))))))

(unit/sig ()
  (import servlet^)

  (let ([bindings (request-bindings initial-request)])
    (no-manual (extract-binding/single 'manual 
				       bindings)
	       (extract-binding/single 'name
				       bindings))))

	       
     


