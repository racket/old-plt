;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; program.ss
;; Richard Cobbe
;; $Id: program.ss,v 1.46 2004/04/23 20:03:28 cobbe Exp $
;;
;; This module defines functions that act on the class inheritance tree.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module program mzscheme

  (require (lib "contract.ss")
           "ast.ss")

  (provide/contract (find-method (-> class? method-name?
                                     (union method? false?)))
                    (find-field (-> class? field-name?
                                    (union field? false?)))
                    (find-class (-> program? class-type? class?))
                    (init-fields (-> class? (listof field?)))
                    (acquired-fields (-> class? (listof field?)))
                    (type-exists? (-> program? (-> type? boolean?)))
                    (type<=? (-> program? type? type? boolean?))
                    (type-lub (-> program? type? type? (union type? false?)))
                    (can-contain? (-> program? type? class? boolean?))
                    (can-be-contained-in? (-> program? class? class? boolean?))
                    (provides-field? (-> program? class-type? field?
                                         boolean?)))

  ;; find-method :: Class Method-Name -> (Union Method #f)
  ;; finds the named method in the specified class; #f if doesn't exist
  (define find-method
    (lambda (c m)
      (let class-loop ([c c])
        (if (not c)
            #f
            (let method-loop ([ms (class-methods c)])
              (cond
               [(null? ms) (class-loop (class-superclass c))]
               [(eq? m (method-name (car ms))) (car ms)]
               [else (method-loop (cdr ms))]))))))

  ;; find-field :: Class Field-Name -> (Union Field #f)
  ;; finds the named field in the specified class; #f if doesn't exist
  (define find-field
    (lambda (c f)
      (letrec ([search-fields
                (lambda (fs)
                  (cond
                   [(null? fs) #f]
                   [(eq? f (field-name (car fs))) (car fs)]
                   [else (search-fields (cdr fs))]))])
        (let class-loop ([c c])
          (if (not c)
              #f
              (or (search-fields (class-fields c))
                  (search-fields (class-contained-fields c))
                  (search-fields (class-acquired-fields c))
                  (class-loop (class-superclass c))))))))

  ;; find-class :: Program Class-Name -> Class
  ;; Looks up the specified class in the program.
  (define find-class
    (lambda (p ct)
      (hash-table-get (program-classes p) (class-type-name ct))))

  ;; type-exists? :: Program -> Type -> Boolean
  ;; determines if the named type exists within the program.
  (define type-exists?
    (lambda (p)
      (lambda (t)
        (cond
         [(class-type? t)
          (and (hash-table-get (program-classes p) (class-type-name t)
                               (lambda () #f))
               #t)]
         [(ground-type? t) (or (eq? (ground-type-name t) 'int)
                               (eq? (ground-type-name t) 'bool))]
         [(any-type? t) #t]
         [else #f]))))

  ;; type<=? :: Program Type Type -> Boolean
  ;; determines whether t1 is a subtype of t2 in program p
  (define type<=?
    (lambda (p t1 t2)
      (cond
       [(ground-type? t1) (and (ground-type? t2)
                               (eq? (ground-type-name t1)
                                    (ground-type-name t2)))]
       [(ground-type? t2) #f]
       [(any-type? t1) (or (any-type? t2) (class-type? t2))]
       [(class-type? t1) (and (class-type? t2) (subclass?
                                                (find-class p t1)
                                                (find-class p t2)))])))

  ;; subclass? :: Class Class -> Boolean
  ;; determines whether c1 is a subclass of c2
  (define subclass?
    (lambda (c1 c2)
      (let loop ([c1 c1])
        (cond
         [(boolean? c1) #f]
         [(eq? c1 c2) #t]
         [else (loop (class-superclass c1))]))))

  ;; type-lub :: Program Type Type -> (union Type #f)
  ;; finds the least upper bound of t1 and t2 within p; returns #f if
  ;; lub doesn't exist.
  (define type-lub
    (lambda (p t1 t2)
      (cond
       [(ground-type? t1) (if (and (ground-type? t2)
                                   (eq? (ground-type-name t1)
                                        (ground-type-name t2)))
                              t1
                              #f)]
       [(ground-type? t2) #f]
       [(any-type? t1) t2]
       [(any-type? t2) t1]
       [else (class-lub (find-class p t1) (find-class p t2))])))

  ;; class-lub :: Class Class -> Type[Class]
  ;; like type-lub, but works on class records rather than types.
  ;; note that lub always exists: Object.
  (define class-lub
    (lambda (c1 c2)
      (let loop ([c1 c1])
        (if (subclass? c2 c1)
            (class-name c1)
            (loop (class-superclass c1))))))

  ;; init-fields :: Class -> (Listof Field)
  ;; Returns a list of all fields in the class that need to be initialized in
  ;; the constructor.  Superclass's fields precede subclass's.
  (define init-fields
    (lambda (c)
      (let loop ([c c])
        (if (not c)
            null
            (append (init-fields (class-superclass c))
                    (class-fields c)
                    (class-contained-fields c))))))

  ;; acquired-fields :: Class -> (Listof Field)
  ;; Returns a list of all acquired fields in the given class; superclass's
  ;; fields first.
  (define acquired-fields
    (lambda (c)
      (let loop ([c c])
        (if (not c)
            null
            (append (acquired-fields (class-superclass c))
                    (class-acquired-fields c))))))

  ;; can-be-contained-in? :: Program Class Class -> Boolean
  ;; indicates whether c2 is a valid container for c1.
  ;; Implements sq<_P from the specs.
  (define can-be-contained-in?
    (lambda (p c1 c2)
      (or (eq? (class-containers c1) 'any)
          (ormap (lambda (container) (subclass? c2 (find-class p container)))
                 (class-containers c1)))))

  ;; can-contain? :: Program Type[Class] Class -> Boolean
  ;; Indicates whether t1 has a contained field that can contain c2
  ;; implements sq>_P from the specs.
  (define can-contain?
    (lambda (p t1 c2)
      (let loop ([c1 (find-class p t1)])
        (if (not c1)
            #f
            (or (ormap (lambda (f)
                         (subclass? c2 (find-class p (field-type f))))
                       (class-contained-fields c1))
                (loop (class-superclass c1)))))))

  ;; can-be-acquired-from? :: Program Field Class -> Boolean
  ;; determines whether we can acquire field f from class c.
  ;; Note that the presence of an acquired field of the required name and
  ;; compatible type will satisfy this requirement; this allows us to avoid
  ;; cycles.
  (define can-be-acquired-from?
    (lambda (p f c)
      (let ([containers (class-containers c)])
        (cond
         [(find-field c (field-name f)) =>
          (lambda (source-field)
            (type<=? p (field-type source-field) (field-type f)))]
         [(or (eq? containers 'any) (null? containers)) #f]
         [else (andmap
                (lambda (container) (can-be-acquired-from? p f container))
                containers)]))))

  (define provides-field?
    (lambda (p c fd)
      (provides-field-helper p (find-class p c) fd (make-hash-table))))

  ;; provides-field-helper :: Program Class Field
  ;;                          (Hash-Table Class (Union 'pending Boolean))
  ;;                       -> Boolean
  (define provides-field-helper
    (lambda (p c fd history)
      (let ([class-color (hash-table-get history c (lambda () 'unvisited))])
        (cond
         [(eq? class-color 'pending) 'cycle]
         [(boolean? class-color) class-color]
         [(find-field c (field-name fd)) =>
          (lambda (source-field)
            (let ([result (type<=? p (field-type source-field)
                                   (field-type fd))])
              (hash-table-put! history c result)
              result))]
         [(or (eq? 'any (class-containers c))
              (null? (class-containers c)))
          (hash-table-put! history c #f)
          #f]
         [else
          (hash-table-put! history c 'pending)
          (let ([result (check-containers-for-field
                         p fd history
                         (map (lambda (cn) (find-class p cn))
                              (class-containers c))
                         'cycle)])
            (hash-table-put! history c result)
            result)]))))

  ;; check-containers-for-field :: Program Field
  ;;                               (Hash-Table Class (Union 'pending Boolean))
  ;;                               (NEListof Class)
  ;;                               (Union 'cycle Boolean)
  ;;                            -> (Union 'pending Boolean)
  (define check-containers-for-field
    (lambda (p fd history containers accum)
      (let ([current-container
             (provides-field-helper p (car containers) fd history)])
      (cond
       [(null? (cdr containers)) (eq? current-container #t)]
       [(not current-container) #f]
       [(or (eq? current-container #t)
            (eq? accum #t))
        (check-containers-for-field p fd history (cdr containers) #t)]
       [else (check-containers-for-field p fd history (cdr containers)
                                         accum)])))))
