(lambda (sym fail)
  (case sym
    [(blurb)
     (list
      "Errortrace is a Poor Man's stack-trace-on-exceptions/profiler for "
      "MzScheme. Errortrace is not a complete debugger, and a real debugger "
      "in DrScheme is imminent; meanwhile, using errotrace might be better "
      "than nothing.")]
    [(name) "Errortrace"]
    [else (fail)]))
