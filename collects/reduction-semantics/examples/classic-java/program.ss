;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; program.ss
;; Richard Cobbe
;; $Id: program.ss,v 1.4 2004/08/24 20:35:22 cobbe Exp $
;;
;; This module defines functions that act on the class inheritance tree.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module program mzscheme

  (require (lib "contract.ss")
           (lib "etc.ss")
           "ast.ss")

  (provide/contract [find-method (-> class? method-name?
                                     (union method? false/c))]
                    [find-field (-> class? field-name?
                                    (union field? false/c))]
                    [find-all-fields (-> class? (listof field?))]
                    [find-class (-> program? class-type? class?)]
                    [type-exists? (-> program? (-> type? boolean?))]
                    [type<=? (-> program? type? type? boolean?)]
                    [type-lub (-> program? type? type? (union type? false/c))])

  ;; find-first :: (x -> Boolean) (Listof x) -> (Union x #f)
  ;; finds first element in list which satisfies predicate; #f if none.
  (define find-first
    (lambda (p l)
      (recur loop ([l l])
        (cond
         [(null? l) #f]
         [(p (car l)) (car l)]
         [else (loop (cdr l))]))))

  ;; find-slot :: (Class -> (Listof x)) (x -> y) -> Class y -> (Union x #f)
  ;; searches within slots produced by class-slots to find a slot whose name
  ;; (given by slot-name) matches n.  Returns #f if no match found.
  (define find-slot
    (lambda (class-slots slot-name)
      (lambda (c n)
        (let ([target? (lambda (slot) (eq? n (slot-name slot)))])
          (recur class-loop ([c c])
            (and c
                 (or (find-first target? (class-slots c))
                     (class-loop (class-superclass c)))))))))

  ;; find-method :: Class Method-Name -> (Union Method #f)
  ;; finds the named method in the specified class; #f if doesn't exist
  (define find-method (find-slot class-methods method-name))

  ;; find-field :: Class Field-Name -> (Union Field #f)
  ;; finds the named field in the specified class; #f if doesn't exist
  (define find-field (find-slot class-fields field-name))

  ;; find-all-fields :: Class -> (Listof Field)
  ;; returns all field definitions present in class, even shadowed
  (define find-all-fields
    (lambda (c)
      (if (class-superclass c)
          (append (class-fields c) (find-all-fields (class-superclass c)))
          (class-fields c))))

  ;; find-class :: Program Class-Name -> Class
  ;; finds named class in program; throws exn:fail:contract if not found.
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
            (loop (class-superclass c1)))))))
