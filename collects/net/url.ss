(module url mzscheme
  (require (lib "unitsig.ss")
           "url-sig.ss"
           "url-unit.ss"
           "tcp-sig.ss"
           "tcp-unit.ss")
  
  (define-values/invoke-unit/sig
   net:url^
   (compound-unit/sig
     (import)
     (link
      [T : net:tcp^ (tcp@)]
      [U : net:url^ (url@ T)])
     (export (open U))))
  
  (provide (struct url/user (user)))
  
  (provide/contract
   (struct url ([scheme (union false? string?)]
                [host (union false? string?)]
                [port (union false? number?)]
                [path string?]
                [params (union false? string?)]
                [query (union false? string?)]
                [fragment (union false? string?)]))
   (get-pure-port (opt-> (url?) ((listof string?)) input-port?))
   (get-impure-port (opt-> (url?) ((listof string?)) input-port?))
   (post-pure-port (opt-> (url?) ((listof string?)) input-port?))
   (post-impure-port (opt-> (url?) ((listof string?)) input-port?))
   (display-pure-port (input-port? . -> . void?))
   (purify-port (input-port? . -> . (listof mime-header)))
   (netscape/string->url (string? . -> . url?))
   (string->url (string? . -> . url?))
   (url->string (url? . -> . string?))
   (decode-some-url-parts (url? . -> . url?))
   (call/input-url (opt-> (url? 
                           (url? . -> . input-port?)
                           (input-port? . -> . any?))
                          ((listof string?))
                          any))
   (combine-url/relative (url? string? . -> . url?))
   (url-exception? (any? . -> . boolean?))
   
   (current-proxy-servers
    (case-> ((union false? (listof (list string? string? number?))) . -> . void?)
            (-> (union false? (listof (list string? string? number?))))))))

