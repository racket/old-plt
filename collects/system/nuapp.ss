(unit/sig mred:application^
  (import [mred : mred^]
	  [core : mzlib:core^])
  (define app-name "MrEd")
  (define console (make-object wx:frame% '() "hidden"))
  (define eval-string (lambda (string) (void))))