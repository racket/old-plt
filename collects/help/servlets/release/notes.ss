(require (lib "unitsig.ss")
	 (lib "list.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "../private/util.ss")
(require "../private/hd-css.ss")

(define (make-entry s)
  (let* ([label (car s)]
	 [dir (cadr s)]
	 [filename (caddr s)]
	 [file (build-path (collection-path "mzlib") 'up 'up "notes" dir filename)])
    (if (file-exists? file)
	`(LI (A ((HREF ,(format "/servlets/doc-anchor.ss?file=~a&name=~a&caption=~a"
				(hexify-string file)
				filename
				label)))
		,label))
	#f)))

(unit/sig ()
  (import servlet^)

  `(HTML
    (TITLE "Release notes")
    (HEAD ,hd-css)
    (H1 "Release Notes for PLT Scheme version " ,(version))
    (A ((NAME "relnotes") (VALUE "Release notes")))
    "Detailed release notes:"
    (UL
     ,@(filter 
	(lambda (x) x) ; delete #f entries
	(map make-entry
	     '(("DrScheme release notes"
		"drscheme" "HISTORY")
	       ("Teachpack release notes"
		"teachpack" "HISTORY")
	       ("MzScheme version 200"
		"mzscheme" "MzScheme_200.txt")
	       ("MzScheme release notes"
		"mzscheme" "HISTORY")
	       ("MrEd release notes"
		"mred" "HISTORY")
	       ("Stepper release notes"
		"stepper" "HISTORY")
	       ("MrFlow release notes"
		"mrflow" "HISTORY")
	       ("MysterX release notes"
		"mysterx" "HISTORY")
	       ("MzCOM release notes"
		"mzcom" "HISTORY")
	       ("SrPersist release notes"
		"srpersist" "HISTORY")))))))
