(require-library "unitsig.ss")
(require-library "file.ss")		; for normalize-path, load-recent

(define plt-home-directory
  (let ([plt (getenv "PLTHOME")])
    (normalize-path
      (or plt
        (case (system-type)
          [unix "/usr/local/lib/plt/"]
          [windows "C:\\PLT"]
          [else (let-values ([(base name dir?)
			       (split-path (current-directory))])
		  (if (string? base)
		    base
		    (current-directory)))])))))
(require-library (build-path plt-home-directory "lib" "require.ss"))
(plt:require-library "sparams.ss")
(plt:require-library "sparamu.ss")

(load-recent "zsigs")
(load-recent "sigs")
(load-recent "zcode")
(load-recent "misc")
(load-recent "sexp")
(load-recent "pattern")
(load-recent "x")
(load-recent "corelate")
(load-recent "back")
(load-recent "scm-core")
(load-recent "scm-main")
(load-recent "scm-spdy")
(load-recent "scm-obj")
(load-recent "scm-unit")
(load-recent "scm-ou")
(load-recent "link")
(load-recent "invoke")
