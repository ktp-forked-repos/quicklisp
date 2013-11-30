;; Datime formats, compatibility with non-iso8601.
;; Liam Healy Fri Oct 12 2001 - 16:27
;; Time-stamp: <2013-02-24 16:20:30EST formats.lisp>

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

(in-package :antik)

(export '(to-yyddd from-yyddd from-jd jd-table))

;;;****************************************************************************
;;; Year and Day of year (doy)
;;;****************************************************************************

(defun doy-year (datime)
  "Return the doy and year for this datime."
  (let* ((tp (timeparse datime))
	 (sy-dtspec (copy-list tp)))
    (setf (timeparse-month sy-dtspec) 1 (timeparse-day sy-dtspec) 1)
    (let ((yearstart (make-dtspec :timeparse sy-dtspec)))
      (values (cl:+ 1 (days- datime yearstart)
		 ;; doy is counted starting from 1, not 0.
		 (pqval (antik:- datime (start-of-day datime)) 'day))
	      (timeparse-year (timeparse datime))))))

(defun to-yyddd (datime)
  "Generate the twoline element form of datime.  Note this
   is not year-2000 compliant!"
  (multiple-value-bind (doy year)
      (doy-year datime)
    (cl:+ (cl:* (mod year 100) 1000)
       (coerce doy 'double-float))))

(defun from-yyddd
    (yy-or-yyddd &key ddd (scale :utc) (hours 0) (minutes 0) (seconds 0))
  "Read datime from yyddd.ddddd or yy, ddd.ddddd format."
  ;; Specifying both a fractional part of the day and nonzero
  ;; hours/minutes/seconds will cause them to be added,
  ;; which doesn't make much sense.
  (let (year doy)
    (if ddd
	(setf year yy-or-yyddd doy ddd)
      (multiple-value-setq (year doy) (floor yy-or-yyddd 1000)))
    (multiple-value-bind (intday fracday) (floor doy)
      (let* ((zerohrtp
	      (timeparse
	       (dtspec-from-julian-day-number
		(cl:+ (julian-day-number
		    (make-dtspec
		     :timeparse
		     (make-timeparse-majord (convert-two-digit-year year) 1 1 0 0 0)))
		   -1/2
		   intday))))
	     (at-hms
	      (make-timepoint :timeparse
			      (make-timeparse-majord
			       (timeparse-year zerohrtp)
			       (timeparse-month zerohrtp)
			       (timeparse-day zerohrtp)
			       hours minutes seconds)
			      :scale scale)))
	(datime+
	 at-hms
	 (make-pq (cl:* fracday (seconds-per-day at-hms)) 'second))))))

(defun from-jd (jd &optional modified (scale :utc))
  "Find the timepoint from the Julian date.
   Assumes a fixed number of seconds per day (86400)."
  (multiple-value-bind (jdint jdfrac)
      (cl:floor jd)
    (datime+
     (make-timepoint
      :dtspec (dtspec-from-julian-day-number jdint modified)
      :scale scale)
     (make-pq jdfrac 'day))))

(defparameter *month-names*
    '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *month-names-full*
    '("January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December"))

(defun jd-table (start-year end-year &optional (format :org))
  "Write out a JD table in TeX, e.g. for class.
   If d is the day of the month, add d+0.5 to the table
   to get the J2000 day number for that day.
   If csv is T, use csv for import into spread sheet.
   IF nil, use TeX."
  (format t (case format (:org "| Month\\Year") (t " Month")))
  (loop for year from start-year to end-year
	do
	   (format t
		   (case format
		     (:csv ", ~d")
		     (:org "| ~d")
		     (:tex "& ~d"))
		   year))
  (when (eq format :org) (format t "|~&|-"))
  (loop for month from 1 to 12
	do
	   (format t
		   (case format
		     (:csv "~& ~a")
		     (:org "~&| ~a")
		     (:tex "\\\\ ~& ~a "))
		   (elt (if (eq format :org) *month-names-full* *month-names*) (1- month)))
	   (loop for year from start-year to end-year
		 do
		    (format t
			    (case format
			      (:csv ", ~d")
			      (:org "| ~d")
			      (:tex " & $~d$"))
			    (- (datime-j2000day
				(make-timepoint
				 :timeparse
				 (make-timeparse-majord year month 1)
				 :scale :utc))
			       3/2)))
	   (when (eq format :org) (princ "|"))))

;;;;****************************************************************************
;;;; Date utilities for plotting
;;;;****************************************************************************

(defparameter *midnight-2000* (read-timepoint "2000-01-01T00:00:00"))

(defun linear-timepoint
    (timepoint &optional (reference-point *midnight-2000*) (unit :year))
  "Compute the timepoint on a linear (number) scale."
  (cl:+ (pqval (antik:- timepoint reference-point) unit)
     (if (eq unit :year)
	 (timeparse-part (timeparse reference-point) unit)
       0)))

