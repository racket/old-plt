(module remote mzscheme

  (require (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "defmacro.ss"))

  (require "headelts.ss")

  (provide remote-box
	   check-remote)

  (define remote-box (box #f))
  
  (define (check-remote show url)
    (when (unbox remote-box)
      (show 
       `(HTML
	 (HEAD ,hd-css
               ,@hd-links
	       (TITLE "Servlet unavailable"))
	 (BODY
	  (H3
	   (FONT ((COLOR "red"))
		 "Servlet unavailable"))
	  (P)
	  "Because the PLT Help Desk server is "
	  "accepting remote connections, the "
	  "requested Help Desk servlet"
	  (BLOCKQUOTE (TT ,url))
	  "is not available."))))))









