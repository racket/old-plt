(module proxy-prefs mzscheme
  (provide add-proxy-prefs-panel)
  
  (require (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "url.ss" "net"))
  
  ;; set-current-proxy-servers : (union #f (list "http" string number)) -> void
  ;; sets the current-proxy-servers parameter based on the input.
  (define (set-current-proxy-servers val)
    (let* ([ps (current-proxy-servers)]
           [filtered (filter (lambda (x) (not (string=? "http" (car x))))
                             ps)])
      (current-proxy-servers
       (if val
           (cons val filtered)
           filtered))))


  (preferences:set-default 'drscheme:help-desk:http-proxy
                           #f
                           (lambda (x) (or (not x)
                                           (and (list? x)
                                                (= (length x) 3)
                                                (equal? (car x) "http")
                                                (string? (cadr x))
                                                (number? (caddr x))))))
  
  (let ([http-proxy-setting (preferences:get 'drscheme:help-desk:http-proxy)])
    (when http-proxy-setting
      (set-current-proxy-servers http-proxy-setting)))

  (preferences:add-callback 
   'drscheme:help-desk:http-proxy 
   (lambda (name val)
     (set-current-proxy-servers val)))
  
  (define (make-preferences-panel parent)
    (letrec ([p (instantiate vertical-panel% (parent)
                  [stretchable-width #f]
                  [stretchable-height #t]
                  [alignment '(left top)])]
             [rb (make-object radio-box% 
                   #f (list (string-constant proxy-direct-connection)
                            (string-constant proxy-use-proxy))
                   p
                   (lambda (r e)
                     (let ([proxy? (= 1 (send r get-selection))])
                       (send proxy-spec enable proxy?)
                       (if proxy?
                           (update-proxy)
                           (preferences:set 'drscheme:help-desk:http-proxy #f)))))]
             [proxy-spec (instantiate horizontal-panel% (p)
                           [stretchable-width #f]
                           [stretchable-height #f]
                           [alignment '(left center)])]
             [update-proxy (lambda ()
                             (let ([host (send host get-value)]
                                   [port (send port get-value)])
                               (let ([ok? (and (regexp-match "^[0-9a-zA-Z.]+$" host)
                                               (regexp-match "^[0-9]+$" port)
                                               (string->number port)
                                               (<= 1 (string->number port) 65535))])
                                 (when ok?
                                   (preferences:set 
                                    'drscheme:help-desk:http-proxy 
                                    (list "http" host (string->number port))))
                                 (send bad-host show (not ok?)))))]
             [host (make-object text-field%
                     (string-constant proxy-host)
                     proxy-spec (lambda (x y) (update-proxy))
                     "www.someplacethatisaproxy.domain.comm")]
             [port (make-object text-field%
                     (string-constant proxy-port)
                     proxy-spec (lambda (x y) (update-proxy)) "65535")]
             [bad-host (make-object message%
                         (string-constant proxy-bad-host)
                         p)]
             [update-gui
              (lambda (proxy-val)
                (if proxy-val 
                    (begin
                      (send rb set-selection 1)
                      (send proxy-spec enable #t)
                      (send host set-value (cadr proxy-val))
                      (send port set-value (number->string (caddr proxy-val))))
                    (begin
                      (send rb set-selection 0)
                      (send proxy-spec enable #f)
                      (send host set-value "")
                      (send port set-value ""))))])
      
      (preferences:add-callback 'drscheme:help-desk:http-proxy
                                (lambda (name val)
                                  (update-gui val)))
      (update-gui (preferences:get 'drscheme:help-desk:http-proxy))
      (send bad-host show #f)
      p))
  
  (define (add-proxy-prefs-panel)
    (preferences:add-panel (string-constant http-proxy) make-preferences-panel)))
