(defsystem "datetime-protocol"
  :version "0.1.2"
  :description "CLOS datetime protocol for cl-stack (instant/duration/period, chronology, clock, timezone, ISO 8601/RFC 3339/RFC 7231 parsing)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :properties (:cl-repo (:ci (:with ("cl-stack-tzdata") :load-before-test ("cl-stack-tzdata"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "chronology")
               (:file "types")
               (:file "dates")
               (:file "operators")
               (:file "recurrence")
               (:file "timezone")
               (:file "clock")
               (:file "parse"))
  :in-order-to ((test-op (test-op "datetime-protocol/tests"))))

(defsystem "datetime-protocol/calendars"
  :version "0.1.2"
  :description "Easter, Hebrew, Islamic, solar astronomy, and Chinese lunisolar calendars"
  :author "egao1980"
  :license "MIT"
  :depends-on ("datetime-protocol")
  :pathname "src"
  :serial t
  :components ((:file "calendars")
               (:file "astronomy")
               (:file "chinese")
               (:file "event-schedules")))

(defsystem "datetime-protocol/tests"
  :depends-on ("datetime-protocol" "datetime-protocol/calendars" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "chronology-test")
               (:file "operators-test")
               (:file "parse-test")
               (:file "timezone-test")
               (:file "calendars-test")
               (:file "chrono-gold-test")
               (:file "recurrence-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
