(module get-base (lib "base.ss" "python")
  
  ;; following collects/algol60's recipe
  
  (#%provide base-importing-stx)
  (#%define base-importing-stx #'here))