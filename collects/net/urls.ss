(require-library "macro.ss")
(require-library "files.ss")

(define-signature mzlib:url^
  ((struct url (scheme host port path params query fragment))
   unixpath->path
   get-pure-port			; url [x list (str)] -> in-port
   get-impure-port			; url [x list (str)] -> in-port
   display-pure-port			; in-port -> ()
   purify-port				; in-port -> list (mime-header)
   netscape/string->url			; (string -> url)
   string->url				; str -> url
   url->string
   call/input-url			; url x (url -> in-port) x
					; (in-port -> ())
					; [x list (str)] -> ()
   combine-url/relative))		; url x str -> url
