(compound-unit/sig (import [mred : mred^]
			   [basis : drscheme:basis^]
			   [interface : drscheme:interface^]
			   [pretty-print : mzlib:pretty-print^]
			   [file : mzlib:file^])
  (link [beginner-parameters : plt:parameters^
			     ((unit/sig plt:parameters^ (import)
				(define check-syntax-level 'core)))]
	[beginner : zodiac:system^
		  ((require-library-unit/sig "link.ss" "zodiac")
		   (interface : zodiac:interface^)
		   beginner-parameters pretty-print file)]
	[intermediate-parameters : plt:parameters^
				 ((unit/sig plt:parameters^ (import)
				    (define check-syntax-level 'structured)))]
	[intermediate : zodiac:system^
		      ((require-library-unit/sig "link.ss" "zodiac")
		       (interface : zodiac:interface^)
		       intermediate-parameters pretty-print file)]
	[advanced-parameters : plt:parameters^
			     ((unit/sig plt:parameters^ (import)
				(define check-syntax-level 'side-effecting)))]
	[advanced : zodiac:system^
		  ((require-library-unit/sig "link.ss" "zodiac")
		   (interface : zodiac:interface^)
		   advanced-parameters pretty-print file)]
	[quasi-r4rs-parameters : plt:parameters^
			       ((unit/sig plt:parameters^ (import)
				  (define check-syntax-level 'advanced)))]
	[quasi-r4rs : zodiac:system^
		    ((require-library-unit/sig "link.ss" "zodiac")
		     (interface : zodiac:interface^)
		     quasi-r4rs-parameters pretty-print file)]
	[zodiac : drscheme:zodiac^
		((require-unit/sig "zodiac.ss")
		 mred
		 beginner
		 intermediate
		 advanced
		 quasi-r4rs
		 basis)])
  (export (open zodiac)))
