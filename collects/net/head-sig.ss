
(module head-sig mzscheme
  (import (lib "unitsig.ss"))

  (export net:head^)
  (define-signature net:head^
    (empty-header
     validate-header
     extract-field
     remove-field
     insert-field
     append-headers
     standard-message-header
     data-lines->data
     extract-addresses
     assemble-address-field)))

