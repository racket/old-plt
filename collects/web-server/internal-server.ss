(module internal-server mzscheme
  (provide serve)
  (require (lib "tcp-redirect.ss" "net")
           (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "sendurl.ss" "net")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           (lib "plt-installer.ss" "setup")
           (lib "browser-sig.ss" "browser")
           (lib "browser-unit.ss" "browser")
           (lib "web-server-unit.ss" "web-server")
           (lib "servlet-sig.ss" "web-server"))
  
  ; : configuration nat [str] -> (-> void)
  ; to serve web connections on a port without TCP/IP
  (define (serve configuration port . dud)
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
                      
                      (external-browser
                       (let ([browser-frame #f])
                         (lambda (url-str)
                           (if browser-frame
                               (begin (send browser-frame show #t)
                                      (send (send (send browser-frame get-hyper-panel) get-canvas) goto-url url-str #f))
                               (set! browser-frame (open-url url-str))))))
                      
                      (serve configuration
                             (let ([listener (tcp-listen port)])
                               (lambda ()
                                 (tcp-accept listener)))))
                    TCP BROWSER WEB-SERVER)])
       (export))
     setup:plt-installer^
     mred^)))