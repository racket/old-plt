(require-library "file.ss")
(require-library "macro.ss")
(require-library "include.ss")
(require-library "pretty.ss")

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

(include "zsigs.ss")
(include "sigs.ss")

(define zodiac:system@ (include-unit "link.ss"))
(include "invoke.ss")
