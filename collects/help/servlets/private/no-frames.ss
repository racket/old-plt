(module no-frames mzscheme

  (require "search-pane.ss"
           "main-pane.ss"
           "util.ss"
           (lib "plt-browser.ss" "help" "private"))

  (provide home-no-frames)

  (define (search-bg)
    (get-pref/default 'plt:hd:search-bg search-bg-default))
	
  (define (home-no-frames)
    `(BODY
      ,@(if (use-plt-browser?)
            (list)
            (list (search-pane)))
      (HR)
      ,(main-pane))))


