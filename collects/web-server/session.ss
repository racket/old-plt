;; ################################################################################
;; This is the timeout version of sessions
(module session mzscheme
  (require "timer.ss")
  
  (provide session->resource
           session?
           make-session-collection
           renew-session!
           kill-session!)
  
  ;; ********************************************************************************
;; General philosophy of sessions:
;;
;; A session manages a particular class of resource. e.g. connections, servlet-instances
;; Sessions may expire.
;;   e.g.: (1) a timer may time out and cause the session to expire.
;;         (2) in a queued model, a session may eventually be pushed out of a queue and expire.
;; When a session expires the resource managed by the session is destroyed.
;; At any time prior to expiration, a session may be renewed, given a time in seconds.
;;  The session is guaranteed to last at least as long as the provided time.
;; All session which manage a particular class belong to a collection.
;; Sessions in a collection are created via a curried function: make-create-session which returns
;;  a session creator for the collection.
;; A session may be killed befor its expiration time.
  
  (define-struct session (payload timer kill-thunk))
  
  ;; session->resource: session -> alpha
  ;; pull out the resource that is managed by the session
  (define session->resource  session-payload)
  
  ;; make-session-collection: -> procedure procedure
  ;; For the case of timeout sessions this is mostly trivial since we don't have a collection.
  (define (make-session-collection)
    (values
      
     ;; create-session: number alpha -> (session-of alpha)
     (lambda (time x shutdown)
       (make-session x (start-timer time shutdown) shutdown))
     
     ;; kill-all-sessions!: ->
     ;; nothing to do in the timeout case since we don't really have a collection
     void))
  
  ;; renew-session!: session number ->
  ;; renew the session so that it can live a little longer
  (define (renew-session! ses new-time-to-live)
    (reset-timer (session-timer ses) new-time-to-live))
  
  ;; kill-session!: ->
  ;; kill this session
  (define (kill-session! ses)
    ((session-kill-thunk ses)))
  )
