#cs
(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss") 
           (lib "etc.ss")
           (lib "class.ss")
	   (lib "string-constant.ss" "string-constants")
           (file "class.scm")
           (file "wizard.ss")
           )

  (provide tool@)
  
  ;; Strings
  (define insert-java-class "Insert Java Class")
  (define insert-java-union "Insert Java Union")
  
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define (phase1) (void))
      (define (phase2) (void))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  Wire up to DrScheme
      ;;

      ;; insert a Java Class

      (define (java-class-wizard-mixin %)
        (class %
          (inherit get-special-menu get-edit-target-object)
          
          (super-new)
          
          ;; String (-> X) (X -> String) -> Void
          (define (make-menu-item% insert-what get-class-info make-what)
            (new menu-item%
                 (label insert-what) (parent (get-special-menu))
                 (callback 
                  (lambda (menu event)
                    (let* ([class-as-info (get-class-info)]
                           [class-as-strn (if (boolean? class-as-info) "" 
                                              (apply make-what class-as-info))])
                      (send (get-edit-target-object) insert class-as-strn))))))

          (make-menu-item% insert-java-class get-class-info make-class)
          (make-menu-item% insert-java-union get-union-info make-union)
	  ))
      
      (drscheme:get/extend:extend-unit-frame java-class-wizard-mixin)
      
      ))
  
  )
