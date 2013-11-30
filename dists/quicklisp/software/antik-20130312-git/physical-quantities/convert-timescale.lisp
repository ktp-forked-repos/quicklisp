;; Convert time scales
;; Liam Healy Sat Feb  4 2006 - 18:00
;; Time-stamp: <2011-03-23 22:53:17EDT convert-timescale.lisp>

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

;;; Used internally.  No user serviceable parts.

(in-package :antik)

(export '*real-ut1-utc*)

;;;****************************************************************************
;;; Conversion between TAI and UTC scales
;;;****************************************************************************

;;; "CLUT" is CL's "universal-time" which is NOT UTC.  It is more
;;; aptly thought of as a map between some linear (isomorphic to the
;;; real line) scale and a uniform (same number of seconds per minute)
;;; compound datime to a count of seconds from some epoch, and left at
;;; that.
(defun clut-to-timeparse (clut)
  "Convert clut, CL's unfortunately-named `universal-time', to timeparse.
   No scale is provided; it is a uniform time."
  (multiple-value-bind (secs frac) (cl:floor clut)
    (let ((tp
	   (subseq (multiple-value-list (decode-universal-time secs 0)) 0 6)))
      (cons (cl:+ frac (timeparse-second tp)) (rest tp)))))

(defun timeparse-to-clut (timeparse)
  "Convert timeparse to clut, CL's unfortunately-named `universal-time'.
   No scale is provided; it is a uniform time."
  (multiple-value-bind (secs frac) (cl:floor (timeparse-second timeparse))
    (cl:+ frac
	  (apply #'encode-universal-time
		 (append (cons secs (rest timeparse)) (list 0))))))

(defun add-uniform-time (dtspec time-interval)
  "Add the time-interval in seconds to dtspec.
   Warning! assumes uniform time, with the same number of seconds per minute
   (like TAI, not UTC)."
  (make-dtspec
   :timeparse
   (clut-to-timeparse
    (cl:+ time-interval (timeparse-to-clut (timeparse dtspec))))))

(defparameter *utc-leap-second-table*
    (list
     (cons (read-dtspec "1972-01-01T00:00:00") 10)
     (cons (read-dtspec "1972-07-01T00:00:00") 11)
     (cons (read-dtspec "1973-01-01T00:00:00") 12)
     (cons (read-dtspec "1974-01-01T00:00:00") 13)
     (cons (read-dtspec "1975-01-01T00:00:00") 14)
     (cons (read-dtspec "1976-01-01T00:00:00") 15)
     (cons (read-dtspec "1977-01-01T00:00:00") 16)
     (cons (read-dtspec "1978-01-01T00:00:00") 17)
     (cons (read-dtspec "1979-01-01T00:00:00") 18)
     (cons (read-dtspec "1980-01-01T00:00:00") 19)
     (cons (read-dtspec "1981-07-01T00:00:00") 20)
     (cons (read-dtspec "1982-07-01T00:00:00") 21)
     (cons (read-dtspec "1983-07-01T00:00:00") 22)
     (cons (read-dtspec "1985-07-01T00:00:00") 23)
     (cons (read-dtspec "1988-01-01T00:00:00") 24)
     (cons (read-dtspec "1990-01-01T00:00:00") 25)
     (cons (read-dtspec "1991-01-01T00:00:00") 26)
     (cons (read-dtspec "1992-07-01T00:00:00") 27)
     (cons (read-dtspec "1993-07-01T00:00:00") 28)
     (cons (read-dtspec "1994-07-01T00:00:00") 29)
     (cons (read-dtspec "1996-01-01T00:00:00") 30)
     (cons (read-dtspec "1997-07-01T00:00:00") 31)
     (cons (read-dtspec "1999-01-01T00:00:00") 32)
     (cons (read-dtspec "2006-01-01T00:00:00") 33)
     (cons (read-dtspec "2009-01-01T00:00:00") 34))
  "Table of UTC leap second insertion times, with the UTC-TAI offset.
    See http://www.boulder.nist.gov/timefreq/pubs/bulletin/leapsecond.htm.")

(defun utc-to-tai-leap-second-table-conversion (utc-table)
  "Generate a tai leap second table from the utc leap second table."
  (loop for (dtspec . offset) in utc-table
      collect
	(cons (add-uniform-time dtspec (1- offset))
	      offset)))

(defparameter *tai-leap-second-table* nil
  "Table of TAI leap second insertion times, with the UTC-TAI offset.")

(defun last-leap-second (dtspec table)
  "Find the last leap second on whatever time scale table."
  (find-if (lambda (dt) (datime>= dtspec dt))
	   table
	   :from-end t
	   :key #'first))

(defun convert-tai-to-utc (tai-dtspec)
  "Given any TAI time, compute the corresponding UTC time."
  (check-type tai-dtspec dtspec)
  (let ((last-leap-second		; TAI time of the last leap second
	 (last-leap-second
	  tai-dtspec
	  (or *tai-leap-second-table*
	      (setf *tai-leap-second-table*
		(utc-to-tai-leap-second-table-conversion
		 *utc-leap-second-table*))))))
    (make-timepoint-noconvert
     :dtspec
     (if (and
	  last-leap-second
	  (datime<= (first last-leap-second) tai-dtspec)
	  (datime< tai-dtspec (add-uniform-time (first last-leap-second) 1)))
	 ;; During the leap second:
	 ;;a funny looking time spec like 1998-12-31T23:59:60
	 (make-dtspec
	  :timeparse
	  (let ((unti
		 (timeparse
		  (add-uniform-time tai-dtspec (cl:- (1+ (rest last-leap-second)))))))
	    (cons (cl:+ 2 (timeparse-second unti)) (rest unti))))
       ;; an ordinary looking time spec
       (add-uniform-time tai-dtspec (cl:- (or (rest last-leap-second) 0))))
     :scale :utc)))

(defun convert-utc-to-tai (utc-dtspec)
  "Given any UTC time, compute the corresponding TAI time."
  (check-type utc-dtspec dtspec)
  (let ((last-leap-second		; UTC time of the last leap second
	 (last-leap-second utc-dtspec *utc-leap-second-table*)))
    (if (cl:>= (timeparse-second (timeparse utc-dtspec)) 60)
	;; During the leap second
	(make-timepoint-noconvert
	 :timeparse
	 (timeparse
	  (add-uniform-time
	   (make-dtspec
	    :timeparse
	    (cons
	     (1- (timeparse-second (timeparse utc-dtspec))) ; knock off a second
	     (rest (timeparse utc-dtspec))))
	   (1+ (rest last-leap-second)))) ; add it back in
	 :scale :tai)		
      ;; an ordinary looking time spec
      (make-timepoint-noconvert
       :dtspec
       (add-uniform-time utc-dtspec (or (rest last-leap-second) 0))
       :scale :tai))))

;;;****************************************************************************
;;; Conversion to/from GPS scale
;;;****************************************************************************

(defparameter *gps-reference-time* (read-timepoint "1980-01-06T00:00:00")
  "Timepoint at which GPS time agreed with UTC;
   see Kaplan, Understanding GPS, Section 2.7.2.")

(defparameter *gps-tai*
  (cl:- (timeparse-to-clut
	 (timeparse
	  (convert-utc-to-tai *gps-reference-time*)))
	(timeparse-to-clut (timeparse *gps-reference-time*)))
  "Time interval in seconds between GPS time and TAI time.
   Add this amount to the GPS time to get TAI.")

(defun convert-gps-to-utc (gps-dtspec)
  "Given any timepoint on the GPS scale, find the UTC time."
  (convert-tai-to-utc (add-uniform-time gps-dtspec *gps-tai*)))

(defun convert-utc-to-gps (utc-dtspec)
  "Given any timepoint on the UTC scale, find the GPS time."
  (make-timepoint-noconvert
   :dtspec
   (add-uniform-time (convert-utc-to-tai utc-dtspec) (cl:- *gps-tai*))
   :scale :gps))

;;;****************************************************************************
;;; UT1
;;;****************************************************************************

(defparameter *ut1-utc-table* nil
  "Hash table of differences UT1-UTC by J2000 day number.
   Use (read-op-file) to load.")

(defparameter *ut1-utc-first-estimated* nil
  "First estimated UT1-UTC value in *ut1-utc-table*.
   This serves as a marker for the age of the eop.dat file.")

(defun read-eop-file ()
  "Read the earth orientation parameters from USNO."
  (warn "Downloading earth orientation data; please wait.")
  (with-input-from-string
      (stream
       (drakma:http-request "http://maia.usno.navy.mil/ser7/finals.all"))
    (loop for line cl:= (read-line stream nil nil)
	  with table cl:= (make-hash-table :test #'eql :size 2000)
	  and estimated and date
	  while line
	  do
       (setf
	 date
	 (make-timepoint
	      :timeparse
	      (list 0 0 0
		    ;; see ftp://maia.usno.navy.mil/ser7/readme.finals
		    ;; for column description
		    (read-from-string line nil nil :start 4 :end 6)
		    (read-from-string line nil nil :start 2 :end 4)
		    (convert-two-digit-year
		     (read-from-string line nil nil :start 0 :end 2)))
	      :scale :utc
	      :day-only t)
	 (gethash (cl:+ 1/2 (datime-j2000day date)) table)
	 ;; see ftp://maia.usno.navy.mil/ser7/readme.finals
	 ;; for column description
	 (read-from-string line nil nil :start 58 :end 68))
       (if (and (not estimated) (equal (subseq line 79 86) "       "))
	   (setf estimated date))
	  finally (setf *ut1-utc-table* table
			*ut1-utc-first-estimated* estimated))
    *ut1-utc-table*))

(defparameter *real-ut1-utc* t "If NIL, UT1 will be taken the same as UTC.")

(defun convert-utc-ut1 (timespec &optional to-ut1)
  "Convert UTC time scale to or from UT1 by adding/subtracting
   the appropriate fractional second."
  (unless *real-ut1-utc* (return-from convert-utc-ut1 timespec))
  (flet ((no-data ()
	   (cerror "Take UT1 and UTC as the same from now on."
		   "Unable to find UT1-UTC table, or to find ~a in it."
		   timespec)
	   (setf *real-ut1-utc* nil)
	   (return-from convert-utc-ut1 timespec)))
    (unless (or *ut1-utc-table* (read-eop-file)) ; Read EOP file if unread
      (no-data))
    (when (and
	   ;; If requested time is too far beyond end of actual data, ...
	   (datime> timespec (antik:+ *ut1-utc-first-estimated* #_4_months))
	   ;; ... there should be some newer data available, so ...
	   (datime> now (antik:+ *ut1-utc-first-estimated* #_2_months)))
      ;; Read a new EOP file.
      (unless (read-eop-file) (no-data)))
    ;; Do the addition/subtraction
    (add-uniform-time
     timespec
     (antik:* (if to-ut1 +1 -1)
	 (or (gethash (cl:+ 1/2 (datime-j2000day timespec)) *ut1-utc-table*)
	     (no-data))))))
;;; Round trip doesn't quite always come out even
;;; (convert-utc-ut1 (convert-utc-ut1 #D2005-01-01T00:00:00 t))
;;; #d2004-12-31T23:59:59.999556064605710
;;; because it is using different days in lookup table.

;;;****************************************************************************
;;; Convert time scales
;;;****************************************************************************

(defun convert-time-scale (timepoint new-scale)
  "Convert time from one scale to another."
  (check-type timepoint timepoint)
  (let ((scale (scale timepoint)))
    (make-timepoint-noconvert
     :dtspec
     (ecase new-scale
       (:tai
	(ecase scale
	  (:tai timepoint)
	  (:utc (convert-utc-to-tai timepoint))
	  (:gps (convert-utc-to-tai (convert-time-scale timepoint :utc)))))
       (:utc
	(case scale
	  (:tai (convert-tai-to-utc timepoint))
	  (:utc timepoint)
	  (:ut1 (convert-utc-ut1 timepoint nil))
	  (:gps (convert-gps-to-utc timepoint))
	  ;; Local timezones can be a name appearing in *timezone-offsets*,
	  ;; or a numerical hour offset from UTC.
	  (otherwise
	   (setf (slot-value timepoint 'scale) :utc)
	   (antik:- timepoint
	       (or (rest (assoc scale *timezone-offsets*)) ; scale can be a named time zone
		   (when (numberp scale) (make-pq scale 'hour)) ; or an hour offset
		   (error "Time scale ~a not known" scale))))))
       (:ut1
	(ecase scale
	  (:utc (convert-utc-ut1 timepoint t))
	  (:ut1 timepoint)))
       (:gps
	(ecase scale
	  (:gps timepoint)
	  (:utc (convert-utc-to-gps timepoint)))))
     :scale new-scale
     :day-only (day-only timepoint))))

;;; (convert-time-scale (make-timepoint-noconvert :iso8601 "1999-01-01T00:00:30" :scale :tai) :utc)
;;; 1998-12-31T23:59:59UTC
;;; (convert-time-scale * :tai)
;;; 1999-01-01T00:00:30TAI
;;; (convert-time-scale (make-timepoint-noconvert :iso8601 "1999-01-01T00:00:31" :scale :tai) :utc)
;;; 1998-12-31T23:59:60UTC
;;; (convert-time-scale * :tai)
;;; 1999-01-01T00:00:31TAI
;;; (convert-time-scale (make-timepoint-noconvert :iso8601 "1999-01-01T00:00:31.5" :scale :tai) :utc)
;;; 1998-12-31T23:59:60.5UTC
;;; (convert-time-scale * :tai)
;;; 1999-01-01T00:00:31.5TAI
;;; (convert-time-scale (make-timepoint-noconvert :iso8601 "1999-01-01T00:00:32" :scale :tai) :utc)
;;; 1999-01-01T00:00:00UTC
;;; (convert-time-scale * :tai)
;;; 1999-01-01T00:00:32TAI
;;; (convert-time-scale (make-timepoint-noconvert :iso8601 "1999-01-01T00:00:33" :scale :tai) :utc)
;;; 1999-01-01T00:00:01UTC
;;; (convert-time-scale * :tai)
;;; 1999-01-01T00:00:33TAI
