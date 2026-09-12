(in-package #:datetime-protocol)

;;;; EVENT-SCHEDULE methods for astronomy + computus. Holiday / business-day
;;;; specialize on the calendar that owns the law (cl-stack-calendars).

(defun standard-to-moment (std)
  "STD is a standard-zone RD moment (day + fraction). NIL stays NIL."
  (when std
    (multiple-value-bind (rd frac) (floor std)
      (make-moment (date-from-rd rd)
                   (time-of-day-from-nanos
                    (min (cl:1- +nanos-per-day+)
                         (max 0 (round (* frac +nanos-per-day+)))))))))

(defun %as-dates (x)
  (cond ((null x) nil)
        ((listp x) x)
        (t (list x))))

(defclass solar-schedule (schedule)
  ((location :initarg :location :reader solar-schedule-location)
   (compute :initarg :compute :reader solar-schedule-compute))
  (:documentation "Daily solar event at LOCATION. COMPUTE is (rd loc) →
standard-zone RD moment or NIL (polar night / polar day)."))

(defun %solar-moment (schedule rd)
  (standard-to-moment
   (funcall (solar-schedule-compute schedule) rd (solar-schedule-location schedule))))

(defmethod occurrence-p ((s solar-schedule) (date date))
  (and (%solar-moment s (date-rd date)) t))

(defmethod map-occurrences (function (s solar-schedule) &key from to count)
  (multiple-value-bind (to* count*)
      (%ensure-bound s to count nil nil)
    (let ((start (or from (error 'unbounded-schedule :schedule s
                                 :message "solar-schedule needs :from"))))
      (unless (or to* count*)
        (error 'unbounded-schedule :schedule s
               :message "solar-schedule needs :to or :count"))
      (loop with n = 0
            for rd from (date-rd start)
            until (or (and to* (cl:>= rd (date-rd to*)))
                      (and count* (cl:>= n count*))
                      (and (null to*) (cl:> rd (cl:+ (date-rd start) 400))))
            for m = (%solar-moment s rd)
            when m do (funcall function m) (cl:incf n)))))

(defmethod next-occurrence ((s solar-schedule) after &key inclusive)
  (let ((rd (date-rd (%as-date after))))
    (unless inclusive (cl:incf rd))
    (loop for i from rd to (cl:+ rd 400)
          for m = (%solar-moment s i)
          when m return m)))

(defmethod previous-occurrence ((s solar-schedule) before &key inclusive)
  (let ((rd (date-rd (%as-date before))))
    (unless inclusive (cl:decf rd))
    (loop for i from rd downto (cl:- rd 400)
          for m = (%solar-moment s i)
          when m return m)))

(defclass yearly-event-schedule (schedule)
  ((compute :initarg :compute :reader yearly-event-compute))
  (:documentation "COMPUTE is (year) → DATE, list of DATEs, or NIL."))

(defun %yearly-dates (s year)
  (%as-dates (funcall (yearly-event-compute s) year)))

(defmethod occurrence-p ((s yearly-event-schedule) (date date))
  (find date (%yearly-dates s (date-year date)) :test #'value=))

(defmethod map-occurrences (function (s yearly-event-schedule) &key from to count)
  (multiple-value-bind (to* count*)
      (%ensure-bound s to count nil nil)
    (let ((start (or from (schedule-origin s))))
      (unless (and start (or to* count*))
        (error 'unbounded-schedule :schedule s
               :message "yearly event needs :from and :to or :count"))
      (loop with n = 0
            for year from (date-year start)
            until (or (and to* (cl:> year (date-year to*)))
                      (and count* (cl:>= n count*))
                      (and (null to*) (cl:> year (cl:+ (date-year start) 20))))
            do (dolist (d (%yearly-dates s year))
                 (when (and (or (null from) (cl:>= (date-rd d) (date-rd from)))
                            (or (null to*) (cl:< (date-rd d) (date-rd to*))))
                   (funcall function d) (cl:incf n)
                   (when (and count* (cl:>= n count*)) (return))))))))

(defmethod next-occurrence ((s yearly-event-schedule) after &key inclusive)
  (let* ((d0 (%as-date after))
         (rd0 (if inclusive (date-rd d0) (cl:1+ (date-rd d0)))))
    (loop for year from (date-year d0) to (cl:+ (date-year d0) 20)
          for found = (find-if (lambda (d) (cl:>= (date-rd d) rd0))
                               (%yearly-dates s year)
                               :key #'identity)
          when found return found)))

(defmethod previous-occurrence ((s yearly-event-schedule) before &key inclusive)
  (let* ((d0 (%as-date before))
         (rd0 (if inclusive (date-rd d0) (cl:1- (date-rd d0)))))
    (loop for year from (date-year d0) downto (cl:- (date-year d0) 20)
          for dates = (remove-if (lambda (d) (cl:> (date-rd d) rd0))
                                 (%yearly-dates s year))
          when dates return (car (last dates)))))

(defun %daily-solar-schedule (loc compute)
  (make-instance 'solar-schedule :location loc :compute compute))

(defun %yearly-date-schedule (compute)
  (make-instance 'yearly-event-schedule :compute compute))

(defun %hebrew-in-gregorian (g-year festival)
  "GREGORIAN year → DATE of Hebrew FESTIVAL (Hebrew year) that falls in it."
  (loop for hy from (cl:+ g-year 3760) to (cl:+ g-year 3762)
        for d = (funcall festival hy)
        when (and d (cl:= (date-year d) g-year))
          return d))

;;; --- solar / ritual (need lat/lon) ---------------------------------------

(defmethod event-schedule ((loc astro-location) (event (eql :sunrise)) &key)
  (%daily-solar-schedule loc #'sunrise))

(defmethod event-schedule ((loc astro-location) (event (eql :sunset)) &key)
  (%daily-solar-schedule loc #'sunset))

(defmethod event-schedule ((loc astro-location) (event (eql :dawn))
                           &key (depression 6d0))
  (%daily-solar-schedule loc (lambda (rd l) (dawn rd l depression))))

(defmethod event-schedule ((loc astro-location) (event (eql :dusk))
                           &key (depression 6d0))
  (%daily-solar-schedule loc (lambda (rd l) (dusk rd l depression))))

(defmethod event-schedule ((loc astro-location) (event (eql :midday)) &key)
  (%daily-solar-schedule loc
                         (lambda (rd l)
                           (standard-from-universal (midday-ut rd l) l))))

(defmethod event-schedule ((loc astro-location) (event (eql :jewish-sunset)) &key)
  (%daily-solar-schedule loc (lambda (rd l) (jewish-sunset rd :location l))))

(defmethod event-schedule ((loc astro-location) (event (eql :jewish-nightfall))
                           &key (depression +jewish-dusk-vilna+))
  (%daily-solar-schedule loc
                         (lambda (rd l)
                           (jewish-nightfall rd :location l :depression depression))))

(defmethod event-schedule ((loc astro-location) (event (eql :islamic-maghrib)) &key)
  (%daily-solar-schedule loc (lambda (rd l) (islamic-maghrib rd :location l))))

(defmethod event-schedule ((loc astro-location) (event (eql :islamic-fajr))
                           &key (angle +islamic-fajr-angle-mwl+))
  (%daily-solar-schedule loc
                         (lambda (rd l) (islamic-fajr rd :location l :angle angle))))

(defmethod event-schedule ((loc astro-location) (event (eql :islamic-isha))
                           &key (angle +islamic-fajr-angle-mwl+))
  (%daily-solar-schedule loc
                         (lambda (rd l) (islamic-isha rd :location l :angle angle))))

(defmethod event-schedule ((loc astro-location) (event (eql :spring-equinox)) &key)
  (%yearly-date-schedule (lambda (y) (spring-equinox-date y :location loc))))

(defmethod event-schedule ((loc astro-location) (event (eql :autumn-equinox)) &key)
  (%yearly-date-schedule (lambda (y) (autumn-equinox-date y :location loc))))

(defmethod event-schedule ((loc astro-location) (event (eql :qingming)) &key)
  (%yearly-date-schedule (lambda (y) (qingming-date y :location loc))))

(defmethod event-schedule ((loc astro-location) (event (eql :chinese-new-year)) &key)
  (%yearly-date-schedule (lambda (y) (chinese-new-year-date y :location loc))))

;;; --- computus / civil ----------------------------------------------------

(defmethod event-schedule ((source (eql :computus)) (event (eql :easter-western)) &key)
  (%yearly-date-schedule #'easter-western))

(defmethod event-schedule ((source (eql :computus)) (event (eql :easter-orthodox)) &key)
  (%yearly-date-schedule #'easter-orthodox))

(defmethod event-schedule ((source (eql :computus)) (event (eql :passover)) &key)
  (%yearly-date-schedule (lambda (y) (%hebrew-in-gregorian y #'passover))))

(defmethod event-schedule ((source (eql :computus)) (event (eql :rosh-hashanah)) &key)
  (%yearly-date-schedule (lambda (y) (%hebrew-in-gregorian y #'rosh-hashanah))))

(defmethod event-schedule ((source (eql :computus)) (event (eql :eid-al-fitr)) &key)
  (%yearly-date-schedule (lambda (y) (islamic-dates-in-gregorian-year y 10 1))))

(defmethod event-schedule ((source (eql :computus)) (event (eql :eid-al-adha)) &key)
  (%yearly-date-schedule (lambda (y) (islamic-dates-in-gregorian-year y 12 10))))

(defmethod event-schedule ((source (eql :computus)) (event (eql :islamic-new-year)) &key)
  (%yearly-date-schedule (lambda (y) (islamic-dates-in-gregorian-year y 1 1))))

(defmethod event-schedule ((source (eql :computus)) (event (eql :chinese-new-year)) &key)
  (%yearly-date-schedule #'chinese-new-year-date))

(defmethod event-schedule ((source (eql :computus)) (event (eql :new-year)) &key)
  (yearly :on '(:month 1 :day 1)))

(defmethod event-schedule ((source (eql t)) (event (eql :new-year)) &key)
  (yearly :on '(:month 1 :day 1)))

(defmethod event-schedule ((source (eql t)) (event (eql :easter-western)) &key)
  (event-schedule :computus :easter-western))

(defmethod event-schedule ((source (eql t)) (event (eql :weekend)) &key)
  (weekly :on '(:saturday :sunday)))
