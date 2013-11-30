;; Time interval parsing and generating.
;; Liam Healy Thu May  2 2002 - 16:34
;; Time-stamp: <2010-12-25 17:18:07EST time-interval.lisp>

;; Copyright 2011 Liam M. Healy
;; Distributed under the terms of the GNU General Public License
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; ISO8601:2000(E) Sec. 5.5.4.2 "Representation of time interval by duration only"

(in-package :antik)

(export '(iso8601-time-interval iso8601-parse-time-interval all-time-units))

;;; Output
(defun iso8601-time-interval
    (time
     &optional (format :tud)
     (fractional-seconds-digits *iso8601-fractional-seconds-digits*)
     required-component)
  "Represent the time interval in
   ISO8601:2000(E) Section 5.5.4.2.1 format,
   Representation of time-interval by duration only."
  (with-pq ((time time))
    (let* ((rounded-time
	    ;; Use the time in seconds to the formatted precision
	    ;; to prevent something like
	    ;; (iso8601-time-interval #_35999.9999999998_sec :tud)
	    ;; "PT9h59m60.000s"
	    (read-from-string 
	     (format-float
	      (pqval (abs time) nil *siu*)
	      :significant-figures nil
	      :fracpart-digits fractional-seconds-digits)))
	   (sign (cl:signum (pq-magnitude time)))
	   (tseconds rounded-time))
      (multiple-value-bind
	    (tminutes seconds) (cl:floor tseconds +seconds-per-minute+)
	(multiple-value-bind
	      (thours minutes) (cl:floor tminutes +minutes-per-hour+)
	  (multiple-value-bind
		(tdays hours) (cl:floor thours +hours-per-day+)
	    (multiple-value-bind
		  (tmonths days) (cl:floor tdays +days-per-month+)
	      (multiple-value-bind
		    (years months) (cl:floor tmonths +months-per-year+)
		(mkstr "P"		; interval marker
		       (iso8601-string
			(fn-major-timeparse
			 (make-timeparse-minord
			  seconds minutes hours days months years)
			 (if (minusp sign) #'cl:- #'identity))
			format
			fractional-seconds-digits
			required-component))))))))))

;;; Input
(defun iso8601-parse-time-interval (string)
  "Parse the time interval in
   ISO8601:2000(E) Section 5.5.4.2.1 format,
   Representation of time-interval by duration only."
  (let ((tp (iso8601-parse string)))
    (make-pq
     (cl:+ (timeparse-second tp)
	(cl:* +seconds-per-minute+ (timeparse-minute tp))
	(cl:* +seconds-per-hour+ (timeparse-hour tp))
	(cl:* +seconds-per-day+ (timeparse-day tp))
	(cl:* +seconds-per-month+ (timeparse-month tp))
	(cl:* +seconds-per-year+ (timeparse-year tp)))
     'second)))

(defun parse-time-interval-test ()
  "Test cases for parser."
  (macrolet ((piti (string)
	       `(ignore-errors (iso8601-parse-time-interval ,string))))
    (values 
     (piti "T22")			; 22 hours
     (piti "T22S")			; 22 seconds
     (piti "T14M22S")			; 14 minutes, 22 seconds
     (piti "T14:22")			; 14 hours, 22 minutes
     (piti "T6H14M22S")		; 6 hours, 14 minutes, 22 seconds
     (piti "T06:14:22")		; 6 hours, 14 minutes, 22 seconds
     (piti "16DT6H14M22S")		; 16 days, 6 hours, 14 minutes, 22 seconds
     ;; not permitted yet
     ;;(piti "--16T06:14:22")		; 16 days, 6 hours, 14 minutes, 22 seconds
     (piti "8M16DT6H14M22S")		; 8 months, 16 days, ...
     (piti "8MT6H14M22S")		; 8 months, 0 days, ...
     ;; not permitted yet
     ;;(piti "P-08-16T06:14:22")		; 8 months, 16 days, ...
     (piti "2Y8M16DT6H14M22S")		; 2 years, 8 months, ...
     (piti "0002-08-16T06:14:22"))))	; 2 years, 8 months, ...

(defparameter *all-time-units* '(day hour minute second))

(defun all-time-units (time-interval &optional (stream *standard-output*))
  "Show the time interval in all possible time units."
  (dolist (tu *all-time-units*)
    (new-units time-interval (list tu) stream)
    (fresh-line stream))
  time-interval)
