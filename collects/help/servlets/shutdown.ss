(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server"))

(require "private/exit.ss")
(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define (closing-page k-url)
    `(HTML
      (HEAD 
       ,hd-css
      (BODY 
       (CENTER
	,(color-with 
	  "red"
	  `(H2 "PLT Help Desk server has been shut down.")
	  `(P)
	  "You can restart the server by running the "
	  "Help Desk launcher, or starting Help Desk from "
	  "DrScheme's Help menu."
	  `(P)
	  "You can close your browser, if desired."))))))

  (thread
   (lambda ()
     (sleep 5)
     (let ([exit-proc (unbox exit-box)])
       (and exit-proc
	    (exit-proc)
	    (exit)))))

  (send/suspend closing-page))




