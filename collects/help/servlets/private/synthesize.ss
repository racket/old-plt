(module synthesize mzscheme
  (require (lib "list.ss"))
  (require (lib "string-constant.ss" "string-constants"))
  (require "buginfo.ss")

  (provide get-synthesized-info)

  (define (get-synthesized-info)
    (let* ([synth:version (version)]
	   [synth:environment
	    (format "~a ~s (~a)"
		    (system-type)
		    (system-type #t)
		    (system-library-subpath))]
	   [synth:computer-language 
	    (or (get-dr-bug-report-item 'computer-language)
		"unknown")]
	   [synth:human-language (format "~s" (this-language))]
	   [synth:docs 
	    (format "~s"
		    (with-handlers 
		     ([void (lambda _ "none")])
		     (directory-list (collection-path "doc"))))]
	   [synth:collects
	    (format "~s"
		    (map 
		     (lambda (path)
		       (list path 
			     (if (directory-exists? path)
				(directory-list path)
				"bad path")))
		     (current-library-collection-paths)))])
      `((version ,synth:version)
	(environment ,synth:environment)
	(computer-language ,synth:computer-language)
	(human-language ,synth:human-language)
	(documentation ,synth:docs)
	(collections ,synth:collects)))))



      

