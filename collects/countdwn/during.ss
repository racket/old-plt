(unit/sig during^
  (import before^)
  (define remember
    (opt-lambda (name second [minute #f] [hour #f] [day #f] [month #f] [year #f])
      (send edit new-counter name second minute hour day month year))))
  