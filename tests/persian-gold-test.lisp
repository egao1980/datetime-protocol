(in-package #:datetime-protocol/tests)

;;;; Solar Hijri 1 Farvardin against the official table frozen in
;;;; data/tests/persian-gold.sexp (scripts/generate_persian_gold.py). A row is
;;;; (SH-YEAR GY GM GD): 1 Farvardin of SH-YEAR is Gregorian GY-GM-GD.

(defparameter *persian-gold*
  (with-open-file (in (asdf:system-relative-pathname "datetime-protocol" "data/tests/persian-gold.sexp")
                      :external-format :utf-8)
    (let ((*read-eval* nil))
      (read in))))

(deftest persian-gold-fixture-shape
  (ok (getf *persian-gold* :sources))
  (ok (> (length (getf *persian-gold* :rows)) 40)))

(deftest persian-new-year-matches-official-table
  "Every 1 Farvardin of the table, both directions."
  (let ((bad '()))
    (dolist (row (getf *persian-gold* :rows))
      (destructuring-bind (sh gy gm gd) row
        (let ((rd (fixed-from-date +gregorian+ gy gm gd)))
          (unless (and (eql rd (fixed-from-persian sh 1 1))
                       (equal (list sh 1 1) (multiple-value-list (persian-from-fixed rd)))
                       (equal (list (cl:1- sh) 12 (persian-month-length (cl:1- sh) 12))
                              (multiple-value-list (persian-from-fixed (cl:1- rd))))
                       (eql rd (date-rd (persian-new-year-date gy))))
            (push row bad)))))
    (ok (null bad) (format nil "1 Farvardin mismatches: ~s" bad))))

(deftest persian-round-trip
  "Every day of SH 1379–1420 converts back to itself."
  (let ((bad 0))
    (loop for rd from (fixed-from-persian 1379 1 1) below (fixed-from-persian 1421 1 1)
          do (multiple-value-bind (y m d) (persian-from-fixed rd)
               (unless (and (<= 1 d (persian-month-length y m))
                            (eql rd (fixed-from-persian y m d)))
                 (incf bad))))
    (ok (zerop bad) (format nil "~d days do not round-trip" bad))))

(deftest persian-dates-in-gregorian-year
  (ok (equal '((2024 3 20)) (mapcar (lambda (d) (list (date-year d) (date-month d) (date-day d)))
                                    (persian-dates-in-gregorian-year 2024 1 1))))
  (ok (equal '((2024 3 19)) (mapcar (lambda (d) (list (date-year d) (date-month d) (date-day d)))
                                    (persian-dates-in-gregorian-year 2024 12 29)))
      "29 Esfand 1402, Oil Nationalization Day"))
