(in-package #:datetime-protocol/tests)

;;;; Ethiopian calendar against data/tests/ethiopic-gold.sexp
;;;; (scripts/generate_ethiopic_gold.py). A row is (EC-YEAR GY GM GD):
;;;; 1 Meskerem of EC-YEAR is Gregorian GY-GM-GD.

(defparameter *ethiopic-gold*
  (with-open-file (in (asdf:system-relative-pathname "datetime-protocol" "data/tests/ethiopic-gold.sexp")
                      :external-format :utf-8)
    (let ((*read-eval* nil))
      (read in))))

(deftest ethiopic-gold-fixture-shape
  (ok (getf *ethiopic-gold* :sources))
  (ok (= 201 (length (getf *ethiopic-gold* :rows)))))

(deftest ethiopic-new-year-matches-gold
  "Every 1 Meskerem 1900–2100 EC, both directions, and the Pagume before it."
  (let ((bad '()))
    (dolist (row (getf *ethiopic-gold* :rows))
      (destructuring-bind (ec gy gm gd) row
        (let ((rd (fixed-from-date +gregorian+ gy gm gd)))
          (unless (and (eql rd (fixed-from-ethiopic ec 1 1))
                       (equal (list ec 1 1) (multiple-value-list (ethiopic-from-fixed rd)))
                       (equal (list (cl:1- ec) 13 (ethiopic-month-length (cl:1- ec) 13))
                              (multiple-value-list (ethiopic-from-fixed (cl:1- rd))))
                       (eql rd (date-rd (ethiopic-new-year-date gy))))
            (push row bad)))))
    (ok (null bad) (format nil "1 Meskerem mismatches: ~s" bad))))

(deftest ethiopic-round-trip
  "Every day of 1990–2100 EC converts back to itself."
  (let ((bad 0))
    (loop for rd from (fixed-from-ethiopic 1990 1 1) below (fixed-from-ethiopic 2101 1 1)
          do (multiple-value-bind (y m d) (ethiopic-from-fixed rd)
               (unless (and (<= 1 d (ethiopic-month-length y m))
                            (eql rd (fixed-from-ethiopic y m d)))
                 (incf bad))))
    (ok (zerop bad) (format nil "~d days do not round-trip" bad))))

(deftest ethiopic-feasts
  "Enkutatash and Meskel move to 12 and 28 September in the Gregorian year
before a leap year; Timkat (11 Ter) is 20 January in a leap year."
  (flet ((iso (d) (list (date-year d) (date-month d) (date-day d))))
    (ok (equal '(2023 9 12) (iso (ethiopic-new-year-date 2023))))
    (ok (equal '(2024 9 11) (iso (ethiopic-new-year-date 2024))))
    (ok (equal '((2023 9 28)) (mapcar #'iso (ethiopic-dates-in-gregorian-year 2023 1 17))))
    (ok (equal '((2024 9 27)) (mapcar #'iso (ethiopic-dates-in-gregorian-year 2024 1 17))))
    (ok (equal '((2024 1 20)) (mapcar #'iso (ethiopic-dates-in-gregorian-year 2024 5 11))))
    (ok (equal '((2025 1 19)) (mapcar #'iso (ethiopic-dates-in-gregorian-year 2025 5 11))))
    (ok (equal '((2023 9 11)) (mapcar #'iso (ethiopic-dates-in-gregorian-year 2023 13 6))))
    (ok (null (ethiopic-dates-in-gregorian-year 2024 13 6)) "no Pagume 6 in 2016 EC")))
