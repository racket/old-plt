(module internal-server mzscheme
  (require (lib "tcp-redirect.ss" "net")
           (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "sendurl.ss" "net")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           (lib "etc.ss")
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")
           (lib "browser-sig.ss" "browser")
           (lib "browser-unit.ss" "browser")
           (lib "web-server-unit.ss" "web-server")
           (lib "configuration-structures.ss" "web-server")
           (lib "contracts.ss")
           (lib "servlet-sig.ss" "web-server"))
  
  (provide/contract 
   (serve (opt->*
           (configuration?)
           ((and/f number? integer? exact? positive?)
            (union string? false?)
            (make-mixin-contract frame%))
           ((-> void?)
            (string . -> . (is-a?/c frame%))))))
  
  ;; to serve web connections on a port without TCP/IP.
  ;; rebinds the tcp primitives via the tcp-redirect unit to functions
  ;; that simulate their behavior without using the network.
  (define serve
    (opt-lambda (configuration
                 [port (configuration-port configuration)]
                 [only-from-host #f]
                 [hyper-frame-mixin (lambda (x) x)])
      (invoke-unit/sig
       (compound-unit/sig
         (import
          (plt-installer : setup:plt-installer^)
          (mred : mred^))
         (link
          [TCP : net:tcp^ ((tcp-redirect (list port)))]
          [BROWSER : browser^ (browser@ plt-installer mred TCP)]
          [WEB-SERVER : web-server^ (web-server@ TCP)]
          [MAIN : () ((unit/sig ()
                        (import net:tcp^
                                browser^
                                web-server^)
                        
                        (define browser-and-server-cust (make-custodian))
                        
			(define browser-frame #f)
                        
                        (parameterize ([current-custodian browser-and-server-cust])
                          (serve configuration
                               (let ([listener (tcp-listen port)])
                                 (lambda ()
                                   (tcp-accept listener)))))
                        (values
			 (lambda () 
			   (when browser-frame
			     (send browser-frame show #f)
			     (set! browser-frame #f))
			   (custodian-shutdown-all browser-and-server-cust))
			 (lambda (url-str)
			   (if browser-frame
			       (begin (send browser-frame show #t)
				      (send (send (send browser-frame get-hyper-panel) get-canvas) goto-url url-str #f))
			       (set! browser-frame (make-object (hyper-frame-mixin hyper-frame%) url-str)))
			   browser-frame)))
                      TCP BROWSER WEB-SERVER)])
         (export))
       setup:plt-installer^
       mred^))))