(require (lib "unitsig.ss")
         (lib "servlet-helpers.ss" "web-server")
         (lib "servlet-sig.ss" "web-server")
	 (lib "help-desk-mz.ss" "help"))

(require "private/hd-css.ss")
(require "private/util.ss")
(require "private/external.ss")

(define doc-root "http://download.plt-scheme.org/doc")

(define (no-manual manual label)
  (let* ([vno (version)]
	 [html-url (format "~a/~a/html/~a/index.htm" 
			  doc-root vno manual)]
	 [plt-url (format "~a/~a/bundles/~a-doc.plt" 
			  doc-root 
			  (if (cvs?) "pre-release" vno)
			  manual)]
	 [external-connections? (unbox external-box)])
    `(HTML
      (HEAD ,hd-css
            (TITLE "Missing PLT manual")) 
      (BODY ((BGCOLOR "white")) 
       ,(color-with "red"
		    `(H1 "Documentation missing"))
       (P)
       "You tried to access documentation for "
       ,(color-with "blue" `(B ,label)) ". "
       "The documentation is not installed on this "
       "machine, probably because it is not part of the "
       "standard DrScheme distribution."
       (P)
       (H2 "To read the documentation online")
       (P)
       (UL
	(LI 
	 (A ((HREF ,html-url)) "Click here")))
       ,@(if external-connections?
	     `("")
	     `((H2 "To download and install the documentation")
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
		    "Click here")))))))))

(unit/sig ()
  (import servlet^)

  (let ([bindings (request-bindings initial-request)])
    (no-manual (extract-binding/single 'manual 
				       bindings)
	       (extract-binding/single 'name
				       bindings))))


	       
     


