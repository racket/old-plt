(module internal-server mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "tcp-redirect.ss" "net")
           (lib "tcp-sig.ss" "net")
           (lib "url-sig.ss" "net")
           (lib "url-unit.ss" "net")
           (lib "sendurl.ss" "net")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")
           (lib "browser-sig.ss" "browser")
           (lib "browser-unit.ss" "browser")
           (lib "web-server-unit.ss" "web-server")
           (lib "configuration.ss" "web-server")
           (lib "servlet-sig.ss" "web-server"))
  
  (provide/contract 
   (internal-serve (opt->*
	   (unit/sig?
	    (and/c number? integer? exact? positive?)
            (union string? false?))
           ((make-mixin-contract frame%))
           ((-> void?)
            (any? . -> . (union false? string?))  ;; any? should be url? but that comes into unit
            (any? . -> . (union false? string?))  ;; any? should be url? but that comes into unit
            (any? . -> . string?)                 ;; any? should be url? but that comes into unit
            (-> (union false? (is-a?/c frame%)))
            (-> (is-a?/c frame%))))))
  
  ;; to serve web connections on a port without TCP/IP.
  ;; rebinds the tcp primitives via the tcp-redirect unit to functions
  ;; that simulate their behavior without using the network.
  (define internal-serve
    (opt-lambda (configuration@ port listen-ip [hyper-frame-extension (lambda (x) x)])
      (invoke-unit/sig
       (compound-unit/sig
         (import
          (plt-installer : setup:plt-installer^)
          (mred : mred^))
         (link
          [tcp : net:tcp^ ((tcp-redirect (list port)))]
          [url : net:url^ (url@ tcp)]
          [browser : browser^ (browser@ plt-installer mred tcp url)]
	  [config : web-config^ ((update-configuration
				  configuration@
				  `((port . ,port)
				    (ip-address . ,listen-ip)
				    (namespace . , (lambda ()
						     (current-namespace))))))]
          [web-server : web-server^ (web-server@ tcp config)]
          [main : () ((unit/sig ()
                        (import net:tcp^
                                browser^
                                web-server^
                                net:url^)
                        
                        (define browser-and-server-cust (make-custodian))
                        
                        ;; (listof frame%)
			(define browser-frames null)
                        
                        (define (shutdown-server)
                          (let ([bfs browser-frames])
                            (set! browser-frames null)
                            (for-each (lambda (browser-frame)
                                        (when (send browser-frame can-close?)
                                          (send browser-frame on-close)
                                          (send browser-frame show #f)))
                                      bfs)
                            (custodian-shutdown-all browser-and-server-cust)))
                        
                        (define (url-on-server-test url)
                          (cond
                            [(url? url)
                             (cond
                               [(and (url-host url)
                                     (string=? (url-host url) "127.0.0.1"))
                                #f]
                               [(and (url? url)
                                     (url-scheme url)
                                     (string=? (url-scheme url) "file"))
                                #f]
                               [else (url->string url)])]
                            [else #f]))
                        
                        (define (extract-url-path url)
                          (cond
                            [(url? url) (url-path url)]
                            [else #f]))
                        
                        (define (remove-from-list-mixin %)
                          (class %
                            (rename [super-on-close on-close])
                            (define/override (on-close)
                              (set! browser-frames (remq this browser-frames))
                              (super-on-close))
                            (super-instantiate ())
                            (set! browser-frames (cons this browser-frames))))
                        
                        (define browser-frame% 
                          (remove-from-list-mixin
                           (hyper-frame-extension 
                            hyper-no-show-frame%)))
                        
                        (define (new-browser) (make-object browser-frame%))
                        
                        (define (find-browser)
                          (if (null? browser-frames)
                              #f
                              (car browser-frames)))
                          
                        (parameterize ([current-custodian browser-and-server-cust])
                          (serve))
                        
                        (values
			 shutdown-server
			 url-on-server-test
                         extract-url-path
                         url->string
                         find-browser
                         new-browser))
                      tcp browser web-server url)])
         (export))
       setup:plt-installer^
       mred^))))
