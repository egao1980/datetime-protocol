(in-package #:datetime-protocol)

(define-condition datetime-error (error)
  ((message :initarg :message :reader datetime-error-message :initform nil))
  (:report (lambda (c s)
             (format s "datetime error~@[: ~a~]" (datetime-error-message c)))))

(define-condition datetime-parse-error (datetime-error)
  ((input :initarg :input :reader datetime-parse-error-input :initform nil))
  (:report (lambda (c s)
             (format s "datetime parse error~@[: ~a~]~@[ (input ~s)~]"
                     (datetime-error-message c) (datetime-parse-error-input c)))))

(define-condition datetime-arithmetic-error (datetime-error) ()
  (:report (lambda (c s)
             (format s "datetime arithmetic error~@[: ~a~]" (datetime-error-message c)))))

(define-condition zone-not-found (datetime-error)
  ((zone-id :initarg :zone-id :reader zone-not-found-zone-id :initform nil))
  (:report (lambda (c s)
             (format s "time zone not found: ~s~@[ (~a)~]"
                     (zone-not-found-zone-id c) (datetime-error-message c)))))

(define-condition ambiguous-zone-abbreviation (datetime-error)
  ((abbreviation :initarg :abbreviation :reader ambiguous-zone-abbreviation-abbreviation)
   (candidates :initarg :candidates :reader ambiguous-zone-abbreviation-candidates :initform nil))
  (:report (lambda (c s)
             (format s "ambiguous zone abbreviation ~s; candidates: ~s"
                     (ambiguous-zone-abbreviation-abbreviation c)
                     (ambiguous-zone-abbreviation-candidates c)))))

(define-condition ambiguous-local-time (datetime-error)
  ((moment :initarg :moment :reader ambiguous-local-time-moment)
   (earlier-offset :initarg :earlier-offset :reader ambiguous-local-time-earlier-offset :initform nil)
   (later-offset :initarg :later-offset :reader ambiguous-local-time-later-offset :initform nil))
  (:report (lambda (c s)
             (format s "ambiguous local time ~a (offsets ~s / ~s) — pass :on-overlap :earlier/:later"
                     (ambiguous-local-time-moment c)
                     (ambiguous-local-time-earlier-offset c)
                     (ambiguous-local-time-later-offset c)))))

(define-condition nonexistent-local-time (datetime-error)
  ((moment :initarg :moment :reader nonexistent-local-time-moment))
  (:report (lambda (c s)
             (format s "local time ~a falls in a zone gap — pass :on-gap :earlier/:later"
                     (nonexistent-local-time-moment c)))))
