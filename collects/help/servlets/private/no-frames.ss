(module no-frames mzscheme

  (require "search-pane.ss"
           "main-pane.ss"
           "util.ss")

  (provide home-no-frames)

  (define (search-bg)
    (get-pref/default 'search-bg search-bg-default))
	
  (define (home-no-frames)
    `(BODY
      ,(search-pane)
      (HR)
      ,(main-pane))))


