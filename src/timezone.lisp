(in-package #:datetime-protocol)

;;;; Timezone protocol. cl-stack-tzdata is an optional, soft dependency:
;;;; it is neither in :depends-on nor referenced by package-qualified
;;;; symbols at read time. Instead we check (asdf:find-system :cl-stack-tzdata
;;;; nil) at runtime and, when present, call into it via UIOP:SYMBOL-CALL so
;;;; this file compiles cleanly whether or not that system is loaded.
;;;;
;;;; Without cl-stack-tzdata, only +UTC+ and other FIXED-OFFSET-ZONEs work;
;;;; NAMED-ZONE lookups signal ZONE-NOT-FOUND.

(defclass tz-repository () ()
  (:documentation "Abstract source of zone-rule data."))

(defclass minimal-tz-repository (tz-repository) ()
  (:documentation "Fallback used when cl-stack-tzdata is not loaded: only
\"UTC\" resolves."))

(defclass tzdata-tz-repository (tz-repository) ()
  (:documentation "Repository backed by the optional cl-stack-tzdata system."))

(defgeneric repository-resolve-id (repository id)
  (:documentation "Canonicalize a zone ID, signalling ZONE-NOT-FOUND if unknown."))

(defgeneric repository-offset-at (repository zone-name unix-seconds)
  (:documentation "Return (values offset-seconds abbreviation dst-p) for
ZONE-NAME at UNIX-SECONDS."))

(defmethod repository-resolve-id ((repository minimal-tz-repository) id)
  (if (member id '("UTC" "Etc/UTC") :test #'string=)
      "UTC"
      (error 'zone-not-found :zone-id id
             :message "cl-stack-tzdata is not loaded; only \"UTC\" is available")))

(defmethod repository-offset-at ((repository minimal-tz-repository) zone-name unix-seconds)
  (declare (ignore zone-name unix-seconds))
  (values 0 "UTC" nil))

(defmethod repository-resolve-id ((repository tzdata-tz-repository) id)
  (handler-case (uiop:symbol-call :cl-stack-tzdata 'canonical-zone-id id)
    (error (e) (error 'zone-not-found :zone-id id :message (princ-to-string e)))))

(defmethod repository-offset-at ((repository tzdata-tz-repository) zone-name unix-seconds)
  (handler-case
      (multiple-value-bind (offset dst-p abbreviation)
          (uiop:symbol-call :cl-stack-tzdata 'zone-offset-at zone-name unix-seconds)
        (values offset abbreviation dst-p))
    (error (e) (error 'zone-not-found :zone-id zone-name :message (princ-to-string e)))))

(defvar *tz-repository* nil
  "The ambient TZ-REPOSITORY. Set by DEFAULT-TZ-REPOSITORY on first use.")

(defun tzdata-available-p ()
  (and (asdf:find-system :cl-stack-tzdata nil) t))

(defun %ensure-tzdata-loaded ()
  (let ((system (asdf:find-system :cl-stack-tzdata nil)))
    (when system
      (unless (asdf:component-loaded-p system)
        (asdf:load-system system))
      t)))

(defun default-tz-repository ()
  (or *tz-repository*
      (setf *tz-repository*
            (if (%ensure-tzdata-loaded)
                (make-instance 'tzdata-tz-repository)
                (make-instance 'minimal-tz-repository)))))

;;; --- zone identities -------------------------------------------------------

(defclass zone-id () ()
  (:documentation "Abstract base for a time zone identity."))

(defun zone-id-p (x) (typep x 'zone-id))

(defclass fixed-offset-zone (zone-id)
  ((offset-seconds :initarg :offset-seconds :reader zone-offset-seconds :type integer))
  (:documentation "A zone with a constant offset from UTC — no DST, no
history. A named IANA zone with a constant offset (e.g. most of Africa) is
still represented as a NAMED-ZONE; this class is for offsets with no zone
identity at all (RFC 3339 \"+02:00\")."))

(defun make-fixed-offset-zone (offset-seconds)
  (make-instance 'fixed-offset-zone :offset-seconds offset-seconds))

(defclass named-zone (zone-id)
  ((name :initarg :name :reader zone-name :type string)
   (repository :initarg :repository :reader zone-repository))
  (:documentation "An IANA zone identifier (e.g. \"Europe/Berlin\"), backed
by a TZ-REPOSITORY for offset/transition lookups."))

(defun make-named-zone (name &optional (repository (default-tz-repository)))
  (make-instance 'named-zone
                 :name (repository-resolve-id repository name)
                 :repository repository))

(defun resolve-zone-id (id &optional (repository (default-tz-repository)))
  "Look up a zone ID as a NAMED-ZONE. History-aware: when cl-stack-tzdata is
loaded, links/aliases (e.g. \"US/Eastern\" -> \"America/New_York\") resolve
to their canonical zone."
  (make-named-zone id repository))

(defvar +utc+ (make-fixed-offset-zone 0))

(defun %format-offset (offset-seconds)
  (if (cl:zerop offset-seconds)
      "UTC"
      (multiple-value-bind (h m) (floor (floor (abs offset-seconds) 60) 60)
        (format nil "~:[+~;-~]~2,'0d:~2,'0d" (cl:< offset-seconds 0) h m))))

;;; --- offset / abbreviation for an INSTANT --------------------------------

(defgeneric zone-offset-for-instant (zone instant)
  (:documentation "The offset (seconds east of UTC) ZONE was observing at INSTANT."))

(defmethod zone-offset-for-instant ((zone fixed-offset-zone) instant)
  (declare (ignore instant))
  (zone-offset-seconds zone))

(defmethod zone-offset-for-instant ((zone named-zone) instant)
  (nth-value 0 (repository-offset-at (zone-repository zone) (zone-name zone)
                                      (instant-seconds instant))))

(defgeneric zone-abbreviation-for-instant (zone instant)
  (:documentation "A short label for ZONE at INSTANT (e.g. \"CET\", \"UTC\",
or a formatted +HH:MM offset)."))

(defmethod zone-abbreviation-for-instant ((zone fixed-offset-zone) instant)
  (declare (ignore instant))
  (%format-offset (zone-offset-seconds zone)))

(defmethod zone-abbreviation-for-instant ((zone named-zone) instant)
  (nth-value 1 (repository-offset-at (zone-repository zone) (zone-name zone)
                                      (instant-seconds instant))))

;;; --- local (wall-clock) resolution: gaps and overlaps ---------------------

(defun %naive-seconds-from-moment (moment)
  "MOMENT's wall-clock reading, as if it were a Unix timestamp — the
starting point for zone offset lookups. Sub-second nanos are not part of
this value (no real zone changes offset mid-second)."
  (cl:+ (cl:* (cl:- (date-rd (moment-date moment)) +unix-epoch-rd+) +seconds-per-day+)
        (floor (time-of-day-nanos-of-day (moment-time moment)) +nanos-per-second+)))

(defun %named-zone-resolve (zone naive-seconds)
  "Classify NAIVE-SECONDS as local wall-clock time in ZONE. Probes the zone's
offset a full day on each side of NAIVE-SECONDS — comfortably bracketing any
single nearby transition, since real-world UTC offsets are always within a
day and DST-style transitions are spaced months apart — to form two UTC
candidates, each checked for round-trip self-consistency:
  (values :normal offset)
  (values :gap earlier-offset later-offset)      ; local time skipped
  (values :overlap earlier-offset later-offset)  ; local time ambiguous
Probing symmetrically around NAIVE-SECONDS (rather than re-deriving one probe
from the other's already-offset-shifted candidate) avoids both probes
collapsing onto the same side of a transition, which would otherwise leave
only one candidate for a genuine gap/overlap and signal an out-of-range
lookup instead of resolving it."
  (flet ((offset-at (utc-seconds)
           (nth-value 0 (repository-offset-at (zone-repository zone) (zone-name zone) utc-seconds))))
    (let* ((offset-before (offset-at (cl:- naive-seconds +seconds-per-day+)))
           (offset-after (offset-at (cl:+ naive-seconds +seconds-per-day+)))
           (candidates (remove-duplicates
                        (list (cl:- naive-seconds offset-before) (cl:- naive-seconds offset-after))))
           (consistent (remove-if-not
                        (lambda (utc) (cl:= (cl:+ utc (offset-at utc)) naive-seconds))
                        candidates)))
      (cond
        ((cl:= (length consistent) 1)
         (values :normal (offset-at (first consistent))))
        ((cl:>= (length consistent) 2)
         (let ((sorted (sort consistent #'cl:<)))
           (values :overlap (offset-at (first sorted)) (offset-at (second sorted)))))
        (t
         (let ((sorted (sort candidates #'cl:<)))
           (values :gap (offset-at (first sorted)) (offset-at (second sorted)))))))))

(defgeneric zone-local-info (zone moment)
  (:documentation "Classify MOMENT as local wall-clock time in ZONE. Returns
(values :normal offset), (values :gap earlier later), or (values :overlap
earlier later) — see MOMENT-IN-ZONE for how to resolve gaps/overlaps."))

(defmethod zone-local-info ((zone fixed-offset-zone) moment)
  (declare (ignore moment))
  (values :normal (zone-offset-seconds zone)))

(defmethod zone-local-info ((zone named-zone) moment)
  (%named-zone-resolve zone (%naive-seconds-from-moment moment)))

(defun moment-in-zone (moment zone &key (on-gap :later) (on-overlap :earlier))
  "Build a ZONED-MOMENT for MOMENT interpreted as wall-clock time in ZONE.
ON-GAP/ON-OVERLAP (:EARLIER :LATER :STRICT) resolve DST gaps (nonexistent
local times) and overlaps (ambiguous local times); :STRICT signals
NONEXISTENT-LOCAL-TIME / AMBIGUOUS-LOCAL-TIME."
  (multiple-value-bind (kind earlier later) (zone-local-info zone moment)
    (ecase kind
      (:normal (make-zoned-moment moment earlier zone))
      (:gap (ecase on-gap
              (:earlier (make-zoned-moment moment earlier zone))
              (:later (make-zoned-moment moment later zone))
              (:strict (error 'nonexistent-local-time :moment moment))))
      (:overlap (ecase on-overlap
                  (:earlier (make-zoned-moment moment earlier zone))
                  (:later (make-zoned-moment moment later zone))
                  (:strict (error 'ambiguous-local-time
                                  :moment moment :earlier-offset earlier :later-offset later)))))))

(defun zoned-moment-to-instant (zoned-moment)
  (let ((naive (%naive-seconds-from-moment (zoned-moment-moment zoned-moment))))
    (make-instant (cl:- naive (zoned-moment-offset-seconds zoned-moment))
                  (time-of-day-nano (zoned-moment-time zoned-moment)))))

(defun instant-in-zone (instant zone)
  "Build a ZONED-MOMENT for INSTANT observed in ZONE. Always well-defined
(no gap/overlap ambiguity going from an exact instant to local time)."
  (let* ((offset (zone-offset-for-instant zone instant))
         (naive (cl:+ (instant-seconds instant) offset)))
    (multiple-value-bind (day-number second-of-day) (floor naive +seconds-per-day+)
      (make-zoned-moment
       (make-moment (date-from-rd (cl:+ day-number +unix-epoch-rd+))
                    (time-of-day-from-nanos (cl:+ (cl:* second-of-day +nanos-per-second+)
                                                   (instant-nanos instant))))
       offset zone))))

;;; --- ambiguous abbreviations (e.g. "EST") ---------------------------------

(defun resolve-zone-abbreviation (abbreviation unix-seconds &key zone-hints region)
  "Resolve an abbreviation like \"EST\" to a NAMED-ZONE at UNIX-SECONDS via
cl-stack-tzdata's abbreviation index. Signals AMBIGUOUS-ZONE-ABBREVIATION if
more than one zone remains after ZONE-HINTS/REGION filtering, or
ZONE-NOT-FOUND if tzdata is unavailable or the abbreviation is unknown."
  (unless (%ensure-tzdata-loaded)
    (error 'zone-not-found :zone-id abbreviation
           :message "cl-stack-tzdata is not loaded; abbreviations require it"))
  (multiple-value-bind (zone-name offset candidates)
      (uiop:symbol-call :cl-stack-tzdata 'resolve-abbreviation abbreviation unix-seconds
                         :zone-hints zone-hints :region region)
    (declare (ignore offset))
    (cond
      ((null zone-name) (error 'zone-not-found :zone-id abbreviation))
      ((and candidates (cl:> (length candidates) 1))
       (error 'ambiguous-zone-abbreviation :abbreviation abbreviation :candidates candidates))
      (t (make-named-zone zone-name)))))

;;; --- arithmetic on ZONED-MOMENT (methods added to operators.lisp's GFs) ---

(defmethod plus ((a zoned-moment) (b duration))
  (instant-in-zone (plus (zoned-moment-to-instant a) b) (zoned-moment-zone a)))
(defmethod plus ((a duration) (b zoned-moment)) (plus b a))

(defmethod plus ((a zoned-moment) (b period))
  (moment-in-zone (plus (zoned-moment-moment a) b) (zoned-moment-zone a)))
(defmethod plus ((a period) (b zoned-moment)) (plus b a))

(defmethod minus ((a zoned-moment) (b duration)) (plus a (negate b)))
(defmethod minus ((a zoned-moment) (b period)) (plus a (negate b)))
(defmethod minus ((a zoned-moment) (b zoned-moment))
  (minus (zoned-moment-to-instant a) (zoned-moment-to-instant b)))

;;; --- WITH-FIELDS for ZONED-MOMENT (generic defined in chronology.lisp) ---

(defmethod with-fields ((zm zoned-moment) &key year month day hour minute second nano
                                             (overflow :error) (on-gap :later) (on-overlap :earlier))
  (moment-in-zone
   (with-fields (zoned-moment-moment zm)
     :year year :month month :day day :hour hour :minute minute :second second :nano nano
     :overflow overflow)
   (zoned-moment-zone zm)
   :on-gap on-gap :on-overlap on-overlap))
