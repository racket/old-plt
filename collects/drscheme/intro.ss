(unit/sig drscheme:intro^
  (import mred^
	  framework^)

  (define (show-release-notes)
    (let ([frame (make-object frame% (format "Release Notes for ~a" (version:version))
			      #f 600 450)]
	  [text (make-object text%)])
      (make-object editor-canvas% frame text)
      (for-each
       (lambda (fn)
	 (call-with-input-file (build-path (collection-path "release-notes") fn)
	   (lambda (port)
	     (let loop ()
	       (let ([l (read-line port)])
		 (unless (eof-object? l)
		   (send text insert l)
		   (send text insert #\newline)
		   (loop)))))
	   'text)
	 (send text insert #\newline))
       (require-library "contents" "release-notes"))

      (send text change-style (make-object style-delta% 'change-family 'modern)
	    0 (send text last-position))
      (send text set-position 0 0)
      (send text lock #t)
      
      (send frame show #t)))

  (define (show-intro)
    (let ([f (make-object frame% "Introduction to DrScheme")])
      (make-object message% "More to come here.... stay tuned" f)
      (make-object button% "Release Notes" f (lambda x (show-release-notes)))
      (send f show #t))))
