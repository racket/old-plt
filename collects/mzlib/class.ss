(module class mzscheme

  ;; All of the implementation is actually in private/class-internal.ss,
  ;;  which provides extra (private) functionality to contract.ss.
  (require "private/class-internal.ss")
  
  (provide class
	   class* class*/names
           class?
	   interface interface?
	   object% object?
           object=?
	   new make-object instantiate
	   send send/apply send* class-field-accessor class-field-mutator with-method
           get-field field-bound? field-names
	   private* public*  pubment*
	   override* overment*
	   augride* augment*
	   define/private define/public define/pubment
	   define/override define/overment
	   define/augride define/augment
	   define-local-member-name
	   generic make-generic send-generic
	   is-a? subclass? implementation? interface-extension?
	   object-interface object-info object->vector
	   method-in-interface? interface->method-names class->interface class-info
	   (struct exn:fail:object ())
	   make-primitive-class

	   ;; "keywords":
	   private public override augment
	   pubment overment augride
	   field init init-field
	   rename-super rename-inner inherit
	   super inner))