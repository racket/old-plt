(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
	 (lib "string.ss"))

(require "../private/util.ss")
(require "../private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define (make-bug-link name)
    (let ([dir (string-copy name)])
      (string-lowercase! dir)
      `(LI (A ((HREF ,(string-append 
		       "/servlets/doc-anchor.ss?file="
		       (hexify-string (build-path (collection-path "mzlib") 'up 'up "notes" dir "OPENBUGS"))
		       "&caption=Open " name " bugs"
		       "&name=" name)))
	      ,name " bugs"))))

 `(HTML
   (HEAD ,hd-css
         (TITLE "Known Bugs"))
   (BODY
    (H1 "Known Bugs in PLT Scheme")
    (A ((NAME "bugs") (VALUE "Bugs")))
    "Significant known bugs in the current release (v" ,(version) "):"
    (UL 
     ,@(map make-bug-link 
	    '("DrScheme" "MzScheme" "MrEd" "Stepper")))
    (P)
    "For an up-to-date list of bug reports, see the "
    (A ((HREF "http://bugs.plt-scheme.org/query/")) "PLT bug report query page")) "."))

