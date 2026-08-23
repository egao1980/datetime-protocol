(in-package #:datetime-protocol/tests)

;;;; Lock conversions against data/tests/chrono-gold.sexp (CPython, dateutil,
;;;; pyluach, Kuwaiti JDN, 内閣府, HKO, zoneinfo). Not calendrica.

(defparameter *chrono-gold-path*
  (merge-pathnames "data/tests/chrono-gold.sexp"
                   (asdf:system-source-directory "datetime-protocol")))

(defun load-chrono-gold (&optional (path *chrono-gold-path*))
  (with-open-file (in path) (read in)))

(defparameter *chrono-gold* (load-chrono-gold))

(defun chrono-block (source kind)
  (find-if (lambda (b)
             (and (equal (getf b :source) source)
                  (eq (getf b :kind) kind)))
           (getf *chrono-gold* :blocks)))

(defun %fmt-ymd (y m d)
  (format nil "~4,'0d-~2,'0d-~2,'0d" y m d))

(deftest chrono-gold-gregorian-rd-and-iso
  (let ((rows (getf (chrono-block "cpython" :gregorian) :rows))
        (bad 0))
    (ok (> (length rows) 1000) "cpython gregorian block present")
    (dolist (row rows)
      (destructuring-bind (y m d rd iso-y iso-w iso-wd) row
        (let ((date (make-date y m d)))
          (unless (and (= rd (date-rd date))
                       (equal (list iso-y iso-w iso-wd)
                              (multiple-value-list (date-from-fixed +iso-week+ rd))))
            (incf bad)
            (when (<= bad 8)
              (ok nil (format nil "gregorian ~a rd=~a iso=~a/~a/~a"
                              (%fmt-ymd y m d) rd iso-y iso-w iso-wd)))))))
    (ok (zerop bad) (format nil "cpython mismatches: ~d / ~d" bad (length rows)))))

(deftest chrono-gold-easter
  (dolist (spec '(("dateutil" :easter-western easter-western)
                  ("dateutil" :easter-orthodox easter-orthodox)))
    (destructuring-bind (source kind fn) spec
      (let ((rows (getf (chrono-block source kind) :rows))
            (bad 0))
        (ok (> (length rows) 700) (format nil "~a block" kind))
        (dolist (row rows)
          (destructuring-bind (year y m d) row
            (unless (value= (make-date y m d) (funcall fn year))
              (incf bad)
              (when (<= bad 8)
                (ok nil (format nil "~a ~d -> ~a" kind year (%fmt-ymd y m d)))))))
        (ok (zerop bad) (format nil "~a mismatches: ~d" kind bad))))))

(defun %gold-hebrew-date (hy name)
  (ecase name
    (:rosh-hashanah (rosh-hashanah hy))
    (:yom-kippur (date-from-rd (fixed-from-hebrew-date hy 1 10)))
    (:sukkot (date-from-rd (fixed-from-hebrew-date hy 1 15)))
    (:passover (passover hy))
    (:shavuot (date-from-rd (fixed-from-hebrew-date hy (+ (hebrew-nisan-month hy) 2) 6)))))

(deftest chrono-gold-hebrew
  (let ((rows (getf (chrono-block "pyluach" :hebrew-holiday) :rows))
        (bad 0))
    (ok (> (length rows) 1000) "pyluach hebrew-holiday block")
    (dolist (row rows)
      (destructuring-bind (hy name y m d) row
        (unless (value= (make-date y m d) (%gold-hebrew-date hy name))
          (incf bad)
          (when (<= bad 8)
            (ok nil (format nil "hebrew ~a ~d -> ~a" name hy (%fmt-ymd y m d)))))))
    (ok (zerop bad) (format nil "pyluach holiday mismatches: ~d" bad)))
  (let ((rows (getf (chrono-block "pyluach" :hebrew-year) :rows))
        (bad 0))
    (dolist (row rows)
      (destructuring-bind (hy length leap) row
        (unless (and (= length (hebrew-year-length hy))
                     (eq (and leap t) (and (hebrew-leap-year-p hy) t)))
          (incf bad)
          (when (<= bad 8)
            (ok nil (format nil "hebrew-year ~d len=~a leap=~a" hy length leap))))))
    (ok (zerop bad) (format nil "pyluach year mismatches: ~d" bad))))

(deftest chrono-gold-islamic-civil
  (let ((rows (getf (chrono-block "kuwaiti-jdn" :islamic-civil) :rows))
        (bad 0))
    (ok (> (length rows) 100) "kuwaiti block")
    (dolist (row rows)
      (destructuring-bind (iy im id y m d) row
        (let ((got (multiple-value-list
                    (date-from-fixed +gregorian+ (fixed-from-islamic-date iy im id)))))
          (unless (equal got (list y m d))
            (incf bad)
            (when (<= bad 8)
              (ok nil (format nil "islamic ~d-~d-~d -> ~a (gold ~a)"
                              iy im id got (%fmt-ymd y m d))))))))
    (ok (zerop bad) (format nil "kuwaiti mismatches: ~d" bad))))

(deftest chrono-gold-julian
  (let ((rows (getf (chrono-block "historical-julian" :julian) :rows))
        (bad 0))
    (dolist (row rows)
      (destructuring-bind (jy jm jd rd tag) row
        (declare (ignore tag))
        (unless (= rd (fixed-from-date +julian+ jy jm jd))
          (incf bad)
          (ok nil (format nil "julian ~d-~d-~d rd=~a" jy jm jd rd)))))
    (ok (zerop bad) (format nil "julian mismatches: ~d" bad))))

(deftest chrono-gold-jp-equinox
  (let ((rows (getf (chrono-block "cao-jp" :jp-equinox) :rows))
        (bad 0))
    (ok (> (length rows) 100) "CAO equinox block")
    (dolist (row rows)
      (destructuring-bind (year season y m d) row
        (let ((got (if (eq season :spring)
                       (spring-equinox-date year)
                       (autumn-equinox-date year))))
          (unless (value= (make-date y m d) got)
            (incf bad)
            (when (<= bad 8)
              (ok nil (format nil "jp ~a ~d gold ~a got ~a"
                              season year (%fmt-ymd y m d)
                              (%fmt-ymd (date-year got) (date-month got) (date-day got)))))))))
    (ok (zerop bad) (format nil "CAO equinox mismatches: ~d" bad))))

(defun %gold-chinese-date (year name)
  (ecase name
    (:chinese-new-year (chinese-new-year-date year))
    (:qingming (qingming-date year))
    (:duanwu (duanwu-date year))
    (:zhongqiu (zhongqiu-date year))))

;;; Civil 农历 uses 北京时间 from 1929. Before that, HKO/紫金山 used local
;;; apparent/mean solar time — Meeus + CST can land one day off.
;;; 2033 中秋: 闰十一月 year; a 中气 sits on a Beijing midnight and we skip
;;; an extra lunation (HKO 8/15 = 2033-09-08, we get 10-07).
(defparameter *hko-allow*
  '((2033 :zhongqiu)))

(deftest chrono-gold-hko-chinese
  (let ((rows (getf (chrono-block "hko" :chinese-festival) :rows))
        (bad 0)
        (compared 0))
    (ok (> (length rows) 400) "HKO festival block")
    (dolist (row rows)
      (destructuring-bind (year name y m d) row
        (when (and (>= year 1929)
                   (not (member (list year name) *hko-allow* :test #'equal)))
          (incf compared)
          (let ((got (%gold-chinese-date year name)))
            (unless (and got (value= (make-date y m d) got))
              (incf bad)
              (when (<= bad 12)
                (ok nil (format nil "hko ~a ~d gold ~a got ~a"
                                name year (%fmt-ymd y m d)
                                (if got
                                    (%fmt-ymd (date-year got) (date-month got) (date-day got))
                                    "nil")))))))))
    (ok (zerop bad) (format nil "HKO mismatches: ~d / ~d" bad compared))))

(deftest chrono-gold-tz-offset
  (unless (tzdata-available-p)
    (skip "cl-stack-tzdata not loaded"))
  (when (tzdata-available-p)
    (let ((rows (getf (chrono-block "zoneinfo" :tz-offset) :rows))
          (bad 0))
      (ok (> (length rows) 100) "zoneinfo offset block")
      (dolist (row rows)
        (destructuring-bind (zone unix offset dst) row
          (declare (ignore dst))
          (let ((got (zone-offset-for-instant (resolve-zone-id zone) (make-instant unix))))
            (unless (= got offset)
              (incf bad)
              (when (<= bad 8)
                (ok nil (format nil "~a unix=~a gold=~a got=~a" zone unix offset got)))))))
      (ok (zerop bad) (format nil "tz-offset mismatches: ~d" bad)))))

(deftest chrono-gold-tz-local
  (unless (tzdata-available-p)
    (skip "cl-stack-tzdata not loaded"))
  (when (tzdata-available-p)
    (let ((rows (getf (chrono-block "zoneinfo" :tz-local) :rows))
          (bad 0))
      (dolist (row rows)
        (destructuring-bind (zone y m d h minute kind earlier later) row
          (let* ((z (resolve-zone-id zone))
                 (moment (make-moment (make-date y m d) (make-time-of-day h minute 0))))
            (multiple-value-bind (got-kind got-earlier got-later) (zone-local-info z moment)
              (unless (eq got-kind kind)
                (incf bad)
                (ok nil (format nil "~a ~a kind gold=~a got=~a"
                                zone (%fmt-ymd y m d) kind got-kind)))
              (when (eq kind :normal)
                (unless (= got-earlier earlier)
                  (incf bad)
                  (ok nil (format nil "~a offset gold=~a got=~a" zone earlier got-earlier))))
              (when (member kind '(:gap :overlap))
                (unless (and (= got-earlier earlier) (= got-later later))
                  (incf bad)
                  (ok nil (format nil "~a ~a offsets gold=~a/~a got=~a/~a"
                                  zone kind earlier later got-earlier got-later))))))))
      (ok (zerop bad) (format nil "tz-local mismatches: ~d" bad)))))
