#|
  
semaphores make things much more predictable...
  
|#  

(module threads mzscheme
  (require "../reduction-semantics.ss"
           "../gui.ss")
  
  (reduction-steps-cutoff 100)
  
  (define lang
    (language
     (p ((store (variable v) ...)
         (semas (variable sema-count) ...)
         (threads e ...)))
     (sema-count number
                 none)
     (e (e e)
        (+ e e)
        (* e e)
        (set! variable e)
        (begin e ...)
        (semaphore-wait e)
        (semaphore-post e)
        variable
        v)
     (p-ctxt ((store (variable v) ...)
              (semas (variable sema-count) ...)
              (threads e ... e-ctxt e ...)))
     (e-ctxt (e-ctxt e)
             (v e-ctxt)
             (+ e-ctxt e)
             (+ v e-ctxt)
             (* e-ctxt e)
             (* v e-ctxt)
             (set! variable e-ctxt)
             (begin e-ctxt e ...)
             (semaphore-wait e-ctxt)
             (semaphore-post e-ctxt)
             hole)
     (v (semaphore variable)
        (lambda (variable) e)
        number
        (void))))
  
  (define reductions
    (list
     (reduction lang
                (in-hole (name c p-ctxt) (+ (name n1 number) (name n2 number)))
                (replace c hole (+ n1 n2)))
     (reduction lang
                (in-hole (name c p-ctxt) (* (name n1 number) (name n2 number)))
                (replace c hole (* n1 n2)))
     (reduction lang
                (in-hole (name c p-ctxt) (begin v (name e1 e) (name e2 e) (name es e) ...))
                (replace c hole `(begin ,e1 ,e2 ,@es)))
     (reduction lang
                (in-hole (name c p-ctxt) (begin v (name e1 e)))
                (replace c hole e1))
     (reduction lang
                (in-hole (name c p-ctxt) (begin (name v v)))
                (replace c hole v))
     (reduction lang
                ((store
                  (name befores (variable v)) ...
                  ((name x variable) (name v v))
                  (name afters (variable v)) ...)
                 (name semas any)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (name x variable))
                  (name e-after e) ...))
                `((store
                   ,@befores
                   (,x ,v)
                   ,@afters)
                  ,semas
                  (threads
                   ,@e-before
                   ,(replace c hole v)
                   ,@e-after)))
     (reduction lang
                ((store
                  (name befores (variable v)) ...
                  ((name x variable) v)
                  (name afters (variable v)) ...)
                 (name semas any)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (set! (name x variable) (name new-v v)))
                  (name e-after e) ...))
                `((store
                   ,@befores
                   (,x ,new-v)
                   ,@afters)
                  ,semas
                  (threads
                   ,@e-before
                   ,(replace c hole `(void))
                   ,@e-after)))
     (reduction lang
                ((name store any)
                 (semas
                  (name befores (variable v)) ...
                  ((name x variable) (name n number))
                  (name afters (variable v)) ...)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (semaphore-wait (semaphore (name x variable))))
                  (name e-after e) ...))
                `(,store
                  (semas
                   ,@befores
                   (,x ,(if (= n 1)
                            `none
                            (- n 1)))
                   ,@afters)
                  (threads
                   ,@e-before
                   ,(replace c hole `(void))
                   ,@e-after)))
     (reduction lang
                ((name store any)
                 (semas
                  (name befores (variable v)) ...
                  ((name x variable) (name n number))
                  (name afters (variable v)) ...)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (semaphore-post (semaphore (name x variable))))
                  (name e-after e) ...))
                `(,store
                  (semas
                   ,@befores
                   (,x ,(+ n 1))
                   ,@afters)
                  (threads
                   ,@e-before
                   ,(replace c hole '(void))
                   ,@e-after)))
     
     (reduction lang
                ((name store any)
                 (semas
                  (name befores (variable v)) ...
                  ((name x variable) none)
                  (name afters (variable v)) ...)
                 (threads
                  (name e-before e) ...
                  (in-hole (name c e-ctxt) (semaphore-post (semaphore (name x variable))))
                  (name e-after e) ...))
                `(,store
                  (semas
                   ,@befores
                   (,x 1)
                   ,@afters)
                  (threads
                   ,@e-before
                   ,(replace c hole '(void))
                   ,@e-after)))))
  
  (gui lang
       reductions
       `((store (y 3))
         (semas)
         (threads (set! y (+ y y))
                  (set! y (* y y)))))
  
  (gui lang
       reductions
       `((store (y 3))
         (semas (x 1))
         (threads (begin (semaphore-wait (semaphore x)) 
                         (set! y (+ y y)) 
                         (semaphore-post (semaphore x)))
                  (begin (semaphore-wait (semaphore x))
                         (set! y (* y y))
                         (semaphore-post (semaphore x)))))))
