(module meta-server-cache mzscheme
  (provide cached? cache! refresh!)

  (define the-cache (make-hash-table))

  ;; refresh!: ->
  ;; refresh the cache
  (define (refresh!)
    (set! the-cache (make-hash-table)))

  ;; cached?: path -> (union (connection request-line -> boolean) #f)
  ;; if the meta server is cached return it otherwise just return #f
  (define (cached? meta-server-path)
    (hash-table-get the-cache meta-server-path
                    (lambda ignore-args #f)))

  ;; cache!: path (connection request-line -> boolean) ->
  ;; store a meta-serve proc in the cache
  (define (cache! meta-server-path meta-serve)
    (hash-table-put! the-cache meta-server-path meta-serve))
  )
