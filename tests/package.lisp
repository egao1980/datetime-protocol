(defpackage #:datetime-protocol/tests
  (:use #:cl #:rove #:datetime-protocol)
  (:shadowing-import-from #:datetime-protocol
   #:+ #:- #:< #:<= #:> #:>= #:= #:/= #:min #:max))

(in-package #:datetime-protocol/tests)
