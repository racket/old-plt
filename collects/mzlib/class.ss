(module class mzscheme
  (require "private/class-sneaky.ss")
  
  (provide class
	   class* class*/names
           class?
	   interface interface?
	   object% object?
           object=?
	   new make-object instantiate
	   send send/apply send* class-field-accessor class-field-mutator with-method
           get-field field-bound? field-names
	   private* public*  public-final* override* override-final*
	   define/private define/public define/public-final define/override define/override-final
	   define-local-member-name
	   generic make-generic send-generic
	   is-a? subclass? implementation? interface-extension?
	   object-interface object-info object->vector
	   method-in-interface? interface->method-names class->interface class-info
	   exn:object? struct:exn:object make-exn:object
	   make-primitive-class))