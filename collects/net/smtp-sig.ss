
(module smtp-sig mzscheme
  (import (lib "unitsig.ss"))

  (export net:smtp^)
  (define-signature net:smtp^
    (smtp-send-message
     smtp-sending-end-of-message)))

