(compound-unit/sig (import [mred : mred^]
			   [basis : drscheme:basis^]
			   [interface : drscheme:interface^]
			   [pretty-print : mzlib:pretty-print^]
			   [file : mzlib:file^])
  (link [beginner-parameters : plt:parameters^
			     ((unit/sig plt:parameters^ (import)
				(define check-syntax-level 'core)))]
	[beginner : zodiac:system^
		  ((reference-library-unit/sig "link.ss" "zodiac")
		   (interface : zodiac:interface^)
		   beginner-parameters pretty-print file)]
	[intermediate-parameters : plt:parameters^
				 ((unit/sig plt:parameters^ (import)
				    (define check-syntax-level 'side-effecting)))]
	[intermediate : zodiac:system^
		      ((reference-library-unit/sig "link.ss" "zodiac")
		       (interface : zodiac:interface^)
		       intermediate-parameters pretty-print file)]
	[advanced-parameters : plt:parameters^
			     ((unit/sig plt:parameters^ (import)
				(define check-syntax-level 'structured)))]
	[advanced : zodiac:system^
		  ((reference-library-unit/sig "link.ss" "zodiac")
		   (interface : zodiac:interface^)
		   advanced-parameters pretty-print file)]
	[quasi-r4rs-parameters : plt:parameters^
			       ((unit/sig plt:parameters^ (import)
				  (define check-syntax-level 'advanced)))]
	[quasi-r4rs : zodiac:system^
		    ((reference-library-unit/sig "link.ss" "zodiac")
		     (interface : zodiac:interface^)
		     quasi-r4rs-parameters pretty-print file)]
	[zodiac : drscheme:zodiac^
		((reference-unit/sig "zodiac.ss")
		 mred
		 beginner
		 intermediate
		 advanced
		 quasi-r4rs
		 basis)])
  (export (open zodiac)))