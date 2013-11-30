;; Read timepoint specifications.
;; Liam Healy, Sat Feb  4 2006 - 12:17
;; Time-stamp: <2011-09-18 21:26:04EDT read-time.lisp>

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

(export '(read-timepoint
	  read-timepoint-iso8601
	  read-time read-us-date write-us-date
	  *timescales*
	  *prehistoric*))

;;; Read many different kinds of date and time formats and
;;; create the correct timepoint.
;;; There are two alternatives for reading iso8601 strings,
;;; via read-timepoint and via read-timepoint-iso8601.
;;; They each can read strings the other can't,
;;; but the former is what read-time and thus #d macro reader use.

;;; Note: currently will not read specifications with letters.

;;; ISO8601 variants
;;; (read-timepoint "1999-03-30T12:33:45")
;;; (read-timepoint "1999-03-30 12:33:45")
;;; (read-timepoint "1999?03/30    % 12-33-45")
;;; (read-timepoint "1999-03-30T12:33:45EST")
;;; (read-timepoint "1999-03-30T12:33:45-05")
;;; (read-timepoint "1999-03-30")

;;; Others
;;; RINEX files
;;; (read-timepoint "03  6  6  9 59 44.0")
;;; (read-timepoint "03     6  6  9 59 44.0" nil :utc)
;;; OCEAN files (read-integrator-step)
;;; ?
;;; US customary, SSR files
;;; (read-us-date "01/15/2006")

(defun read-timepoint (string &optional pos-ymdhms (scale :utc))
  "Read a timepoint from a string with specification for the position in the string of each component; pos-ymdhms is a list of year, month, day, hour, minute, and second as sequence numbers for the integers in the string.  Scale is the timescale (zone) as a string or symbol.  If pos-ymdhms has only three components, or only a date is provided, the timepoint created will be specifed as day-only.  The default reads an ISO8601 string like 1999-03-30T12:33:45."
  (let* ((pcs				; list of (month day year)
	  (mapcar #'read-from-string
		  (split-sequence:split-sequence-if
		   (complement (lambda (c) (or (digit-char-p c) (eql c #\.))))
		   string :remove-empty-subseqs t)))
	 (lpcs (length pcs))
	 (poss (or pos-ymdhms '(0 1 2 3 4 5))))
    (macrolet
	((ymdhms (n)
	   `(let ((indx (nth ,n poss)))
	      (if (and (numberp indx) (> lpcs ,n))
		  (nth indx pcs)
		  ,(when (cl:< n 3) `(error "Insufficient number of components."))))))
      (if (cl:<= lpcs 2)     ; yymmdd as one number, hhmmss as another
	  (make-timepoint
	   :timeparse
	   (make-timeparse-majord
	    (convert-two-digit-year (floor (ymdhms 0) 10000))  ; year
	    (mod (floor (ymdhms 0) 100) 100)		       ; month
	    (mod (ymdhms 0) 100)			       ; month
	    (if (ymdhms 1) (floor (ymdhms 1) 10000) 0)	       ; hour
	    (if (ymdhms 1) (mod (floor (ymdhms 1) 100) 100) 0) ; minute
	    (if (ymdhms 1) (mod (ymdhms 1) 100) 0)) ; second
	   :scale scale
	   :day-only (= lpcs 1))
	  (let ((timespecp (ymdhms 3)))
	    (make-timepoint
	     :timeparse
	     (make-timeparse-majord
	      (convert-two-digit-year (ymdhms 0)) ; year
	      (ymdhms 1)			  ; month
	      (ymdhms 2)			  ; day
	      (or (ymdhms 3) 0)			  ; hour
	      (or (ymdhms 4) 0)			  ; minute
	      (or (ymdhms 5) 0))		  ; second
	     :scale
	     (or
	      (and timespecp
		   (read-timescale
		    (subseq string
			    (1+ (position-if #'digit-char-p string :from-end t)))))
	      scale)
	     :day-only (or (cl:<= (length poss) 3) (not timespecp))))))))

(defparameter *timezone-offsets*
  '((:acdt . #_10.5_hours)
    (:acst . #_9.5_hours)
    (:act . #_8_hours)
    (:adt . #_-3_hours)
    (:aedt . #_11_hours)
    (:aest . #_10_hours)
    (:aft . #_4.5_hours)
    (:akdt . #_-8_hours)
    (:akst . #_-9_hours)
    (:amst . #_5_hours)
    (:amt . #_4_hours)
    (:art . #_-3_hours)
    (:arab . #_3_hours)
    (:arabian . #_4_hours)
    (:awdt . #_9_hours)
    (:awst . #_8_hours)
    (:azost . #_-1_hours)
    (:bdt . #_8_hours)
    (:biot . #_6_hours)
    (:bit . #_-12_hours)
    (:bot . #_-4_hours)
    (:brt . #_-3_hours)
    (:bangladesh . #_6_hours)
    (:bst . #_1_hours)
    (:btt . #_6_hours)
    (:cat . #_2_hours)
    (:cdt . #_-5_hours)
    (:cedt . #_2_hours)
    (:cest . #_2_hours)
    (:cet . #_1_hours)
    (:chadt . #_13.75_hours)
    (:chast . #_12.75_hours)
    (:cist . #_-8_hours)
    (:ckt . #_-10_hours)
    (:clt . #_-4_hours)
    (:cost . #_4_hours)
    (:cot . #_-5_hours)
    (:cst . #_-6_hours)
    (:china . #_8_hours)
    (:centaust . #_9.5_hours)
    (:ct . #_8_hours)
    (:cvt . #_-1_hours)
    (:cxt . #_7_hours)
    (:chst . #_10_hours)
    (:east . #_-6_hours)
    (:eat . #_3_hours)
    (:ect . #_-4_hours)			; carribean
    (:ecuador . #_-5_hours)
    (:edt . #_-4_hours)
    (:eedt . #_3_hours)
    (:eet . #_2_hours)
    (:est . #_-5_hours)
    (:fkst . #_-3_hours)
    (:fkt . #_-4_hours)
    (:galt . #_-6_hours)
    (:get . #_4_hours)
    (:gft . #_-3_hours)
    (:gilt . #_12_hours)
    (:git . #_-9_hours)
    (:gmt . #_0_hours)
    (:gst . #_-2_hours)
    (:gyt . #_-4_hours)
    (:hadt . #_-9_hours)
    (:hast . #_-10_hours)
    (:hkt . #_8_hours)
    (:hmt . #_5_hours)
    (:hst . #_-10_hours)
    (:ict . #_7_hours)
    (:idt . #_3_hours)
    (:irkt . #_8_hours)
    (:irst . #_3.5_hours)
    (:ist . #_5.5_hours)		; Indian summer time
    (:irish . #_1_hours)		; Irish summer time
    (:israel . #_2_hours)		; Israel standard time
    (:jst . #_9_hours)
    (:krat . #_7_hours)
    (:kst . #_9_hours)
    (:lhst . #_10.5_hours)
    (:lint . #_14_hours)
    (:magt . #_11_hours)
    (:mdt . #_-6_hours)
    (:mit . #_9.5_hours)
    (:msd . #_4_hours)
    (:msk . #_3_hours)
    (:mst . #_-7_hours)
    (:malaysia . #_8_hours)
    (:myanmar . #_6.5_hours)
    (:mut . #_4_hours)
    (:myt . #_8_hours)
    (:ndt . #_-2.5_hours)
    (:nft . #_11.5_hours)
    (:npt . #_5.75_hours)
    (:nst . #_-3.5_hours)
    (:nzdt . #_13_hours)
    (:nzst . #_12_hours)
    (:omst . #_6_hours)
    (:pdt . #_-7_hours)
    (:pett . #_12_hours)
    (:phot . #_13_hours)
    (:pkt . #_5_hours)
    (:pst . #_-8_hours)
    (:phillipine . #_8_hours)
    (:ret . #_4_hours)
    (:samt . #_4_hours)
    (:sast . #_2_hours)
    (:sbt . #_11_hours)
    (:sct . #_4_hours)
    (:sgt . #_8_hours)
    (:slt . #_5.5_hours)
    (:sst . #_-11_hours)
    (:singapore . #_8_hours)
    (:taht . #_-10_hours)
    (:tha . #_7_hours)
    (:uyst . #_-2_hours)
    (:uyt . #_-3_hours)
    (:vet . #_-4.5_hours)
    (:vlat . #_10_hours)
    (:wat . #_1_hours)
    (:wedt . #_1_hours)
    (:west . #_1_hours)
    (:wet . #_0_hours)
    (:wst . #_8_hours)
    (:yakt . #_9_hours)
    (:yekt . #_5_hours))
  ;; from http://en.wikipedia.org/wiki/List_of_time_zone_abbreviations
  "Timezones that may be used to input timepoints.")

(defparameter *timescales*
    (append '("utc" "tai" "gps" "ut1") (mapcar #'first *timezone-offsets*))
  "All known time scales.")

(defun read-timescale (string)
  (let ((ts (first (member string *timescales* :test #'string-equal))))
    (if ts
	;; a named timescale
	(alexandria:make-keyword (string-upcase ts))
	(if (and (cl:>= (length string) 3) ; long enough to include time part
		 (member (cl:aref string 0) '(#\+ #\-))) ; and a signed integer
	    (read-from-string string)))))

(defun read-timepoint-iso8601 (string &optional (scale :utc))
  "Read the timepoint specified as an ISO8601 string.  In contrast
   to #'read-timepoint, this accepts the various rearrangements
   permitted by ISO8601 (see documentation for #'iso8601-parse,
   but does not accept miscellaneous separator symbols."
  ;; Provided for completeness.
  (make-timepoint 
   :dtspec (read-dtspec string)
   :scale scale))

;;; (read-us-date "12/20/2000 3:41:12")
;;; #d2000-12-20T03:41:12.000000000000000
(defun read-us-date (string &optional day-only)
  "Read dates and times in customary US format MM/DD/YYYY; times may
   be included as well if day-only is nil."
  (if day-only
      (read-timepoint string '(2 0 1))
      (read-timepoint string '(2 0 1 3 4 5))))

(defun write-us-date (datime)
  "Write dates and times in customary US format MM/DD/YYYY."
  (let ((tp (timeparse datime)))
    (format nil "~a/~a/~a"
	    (timeparse-month tp)
	    (timeparse-day tp)
	    (timeparse-year tp))))

(defun read-time (string)
  "Parse the datime or time interval string and create a timepoint object."
  (if (char-equal (cl:aref string 0) #\p)
	(iso8601-parse-time-interval (subseq string 1))
	(read-timepoint string)))

#|
(defun numeric-month (string)
  "Find the month number from the English month name."
  (1+
   (find
    string
    *month-names*
    :test
    (lambda (x) (string-equal x "unsigned" :end1 3)))))

;; (split-sequence #\space "Fri Sep  2 14:00:00 2011 EDT" :remove-empty-subseqs t)
;; ("Fri" "Sep" "2" "14:00:00" "2011" "EDT")

;;; use read-timepoint, don't need to split-sequence
;;; map from month name to number in read-timepoint
(defun read-unix-date (string &optional day-only)
  "Read dates and times in UNIX format, e.g. Fri Sep  2 14:00:00 2011 EDT."
  (let ((parsed
	  (split-sequence:split-sequence
	   #\space
	   string :remove-empty-subseqs t)))
    (if day-only
      (read-timepoint string '(2 0 1))
      (read-timepoint string '(2 0 1 3 4 5)))))
|#

;;; Read timepoint with reader macro, e.g.  #d1999-03-30T12:33:45.

;;; Is there any way to get this on additional listeners?
(set-dispatch-macro-character
 #\# #\d
 (lambda (stream subchar arg)
   (declare (ignore subchar arg))
   (let* ((string
	   (stream-to-string
	    stream
	    :terminate-if
	    (lambda (ch)
	      (not (or (alphanumericp ch)
		       (member ch '(#\: #\- #\.) :test #'char-equal)))))))
     (unless *read-suppress*
       (read-time string)))))

(defparameter *prehistoric* (read-timepoint "1900-01-01T00:00:00"))
