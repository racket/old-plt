

(load-relative "loadtest.ss")

(require (lib "async-channel.ss"))

(arity-test make-async-channel 0 1)
(err/rt-test (make-async-channel 0))


(report-errs)

