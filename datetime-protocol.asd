(defsystem "datetime-protocol"
  :version "0.1.0"
  :description "CLOS datetime protocol for cl-stack (instant/duration/period, chronology, clock, timezone, ISO 8601/RFC 3339/RFC 7231 parsing)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "chronology")
               (:file "types")
               (:file "dates")
               (:file "operators")
               (:file "timezone")
               (:file "clock")
               (:file "parse"))
  :in-order-to ((test-op (test-op "datetime-protocol/tests"))))

(defsystem "datetime-protocol/calendars"
  :version "0.1.0"
  :description "Easter computus (Western/Orthodox), Hebrew, and Islamic calendars on top of datetime-protocol"
  :author "egao1980"
  :license "MIT"
  :depends-on ("datetime-protocol")
  :pathname "src"
  :components ((:file "calendars")))

(defsystem "datetime-protocol/tests"
  :depends-on ("datetime-protocol" "datetime-protocol/calendars" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "chronology-test")
               (:file "operators-test")
               (:file "parse-test")
               (:file "timezone-test")
               (:file "calendars-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
