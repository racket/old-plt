(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "string-constant.ss" "string-constants"))

(require "private/refresh-util.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (let* ([bindings (request-bindings initial-request)]
	 [tmp-dir (extract-binding/single 'tmp-dir bindings)])
    (delete-directory/r tmp-dir)

    `(HTML 
      (HEAD ,hd-css
	    (TITLE ,(string-constant refresh-stopped)))
      (BODY
	(H2 (B ,(color-with "red" (string-constant refresh-stopped))))
	(P)
	,home-page))))
