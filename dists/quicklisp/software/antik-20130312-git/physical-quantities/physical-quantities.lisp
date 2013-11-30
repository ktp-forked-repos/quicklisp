;; Objects that represent physical measurements.                             
;; Liam Healy Wed Mar  6 2002 - 09:04
;; Time-stamp: <2013-02-17 23:01:31HST physical-quantities.lisp>

;; Copyright 2011, 2012 Liam M. Healy
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

(export '(pqval check-dimension new-units with-pq))

;;;;****************************************************************************
;;;; Parsing unit names
;;;;****************************************************************************

;;; (parse-units "m-kg^2/s^2")
;;; (parse-units 'm-kg^2/s^2)
;;; (parse-units '(/ (* M KG KG) (* S S)))
;;; All return
;;; (1 -2 2 0 0 0 0 0 0)
;;; 1.0
(defun parse-units (unit-expression &optional (sysunits *system-of-units*))
  "Find the dimel representation of the dimensions from a
   string, symbol, or unit sexp."
  (parse-unit-sexp (find-unit-expr (parse-pq-string unit-expression) sysunits)))

;;; (parse-pq-string "m-kg^2/s^2")
;;; (/ (* M KG KG) (* S S))
(defun parse-pq-string (string)
  "Parse a string or symbol specifying units into lists."
  (if (listp string)
      string
      (flet ((symbolize (str)
	       (when (plusp (length str))
		 ;; Empty strings become nil, e.g. "/m" is (/ nil meter)
		 (antik-symbol str))))
	(flet ((lexprod (s)
		 (let ((trms
			(mapcan
			 (lambda (x)
			   (let ((prod (split-sequence:split-sequence #\^ x)))
			     (if (single prod)
				 (list (symbolize (first prod)))
				 (make-list
				  (parse-integer (second prod))
				  :initial-element (symbolize (first prod))))))
			 (split-sequence:split-sequence-if
			  (lambda (x) (member x '(#\- #\*)))
			  s))))
		   (if (single trms) (first trms) (cons '* trms)))))
	  (let ((str (if (symbolp string) (symbol-name string) string)))
	    (destructuring-bind (num &optional den)
		(split-sequence:split-sequence #\/ str)
	      (if den
		  (list '/ (lexprod num) (lexprod den))
		  ;; / with no numerator is reciprocal
		  (if (eq (cl:aref str 0) #\/)
		      (list '/ (lexprod num))
		      (lexprod num)))))))))

(defun check-dimension-or-type (object type)
  "Return T if the object is of the type specified, and, if a physical-quantity, is of the physical-dimension specified in type."
  (or (if (and (typep object 'physical-quantity)
	       (ignore-errors (antik::parse-unit-sexp type)))
	  (check-dimension object type nil))
      (typep object type)))

(setf *parameter-check-function* 'check-dimension-or-type)

(defun check-dimension
    (obj units
     &optional (errorp t) (zeros-have-any-dimension *zero-is-dimensionless*))
  "T if one of these cases hold:
    - obj is a pdq and units have the same dimension as obj,
    - obj is zero and zeros-have-any-dimension is T,
    - obj and units represent a dimensionless quantity,
    - obj and units are grids of the same length, and for 
      each pair of corresponding elements, one of the above is true."
  (if (physical-quantity-p obj)
      (if (and obj (grid:gridp obj) (not (scalar-dimension obj)))
	  (loop for i from 0 below (grid:total-size obj)
		always (equal-dimension (grid:aref* obj i) (grid:aref* obj i)))
	  (equal-dimension obj (parse-units units) errorp zeros-have-any-dimension t))
      (eq units 'dimensionless)))

(defun equal-dimension
    (x y
     &optional
       (errorp t)
       (zeros-have-any-dimension *zero-is-dimensionless*)
       match-dimensionless)
  "If the pdq are equal physical dimension, return that dimel.
   If one quantity is zero and zeros-have-any-dimension, return the
   dimel of the other and disregard whether they're the same physical
   dimension.  If y represents a dimensionless quantity,
   and match-dimensionless is true, return the dimel of x.
   Return two values: the dimel and whether it is scalar dimension."
  (cond ((and zeros-have-any-dimension (zerop x))
	 (values (dimel-or-not y) (scalar-dimension y)))
	((and zeros-have-any-dimension (zerop y))
	 (values (dimel-or-not x) (scalar-dimension x)))
	((equalp (dimel-or-not x) (dimel-or-not y))
	 (values (dimel-or-not x) (scalar-dimension x)))
	;; If one or both has scalar units, break out the individual dimensions.
	((and (grid:gridp x) (grid:gridp y)
	      (or (not (scalar-dimension y)) (not (scalar-dimension x))))
	 ;; All same units, possibly one has scalar dimension
	 (all-same		       ; repurposing 2nd return value 
	  (concatenate
	   'list
	   (if (scalar-dimension x) (list (pq-dimension x)) (pq-dimension x))
	   (if (scalar-dimension y) (list (pq-dimension y)) (pq-dimension y)))
	  :test #'cl:equal))
	((and match-dimensionless (equal (dimel-or-not y) (dimel-or-not nil)))
	 (dimel-or-not x))
	(errorp (error "The quantities ~a and ~a are not both physical ~
               quantities with the same physical dimension." x y))))

(defun dimel-or-not (object)
  "Find the dimel for this object (a physical dimension quantity or a dimel), or
   return the dimensionless dimel."
  (cond ((physical-quantity-p object)
	 (values (pq-dimension object) (scalar-dimension object)))
	((dimelp object) object)
	(t (dimension 'dimensionless))))

(defun equal-dimensions
    (list &optional (zero-is-dimensionless *zero-is-dimensionless*))
  "If all elements of the list have the same physical dimension, return that dimel."
  (let* ((flist (alexandria:flatten list))
	 (short-list (if zero-is-dimensionless (remove-if #'zerop flist) flist)))
    (all-same
     short-list
     :test (alexandria:rcurry #'equal-dimension nil)
     :post-function #'dimel-or-not)))

;;;;****************************************************************************
;;;; Extracting values and printing
;;;;****************************************************************************

(define-parameter nf :degrees
  t t
  "Whether to format angles in degrees and angular rates in Hertz.")

(define-parameter nf :time
  :tud (or null (member :tud :alternate))
  "How to format time intervals:
   :tud = ISO8601 time-unit designator (like P10H7M23.726979085813582S)
   :alternative = ISO8601 alternate (like P10:07:23.726979085813582)
   nil = with the unit for time given by (nf-sysunits)
           (like #_36443.72697908581_S.")

(define-parameter nf :no-units
  nil t
  "Don't print units.")

(defun nf-sysunits ()
  "The system of units used in nf."
  (augment-sysunits
   (when (nf-option degrees) 'degree)
   (when (nf-option degrees) 'hertz)))

(defun pqval-nf (pq)
  (pqval pq nil (nf-sysunits)))

(defmethod nf
  ((pq physical-quantity) &optional (stream *standard-output*))
  (if (and (grid:gridp pq)		
	   (not (and (typep pq 'physical-quantity) (scalar-dimension pq))))
      ;; If a grid that is either not a pdq or is a pdq with non-scalar
      ;; dimensions, call the T method of nf, so that it will call
      ;; nf-grid.
      (call-next-method)
      (multiple-value-bind (val units)
	  (pqval-nf pq)
	(if (and (nf-option time)
		 (not (grid:gridp (pq-magnitude pq)))
		 (check-dimension pq 'time nil nil))
	    (progn
	      (unless texstyle (when (nf-readably) (princ "#d" stream)))
	      (princn (iso8601-time-interval pq (nf-option time))
		      stream))
	    (if (nf-option no-units)
		(nf val stream)		; format the number only
		(format-units val units (scalar-dimension pq) stream))))))

(defmethod print-object ((pq physical-quantity) stream)
   "Print the physical-quantity using nf."
   #+debug
   (print-unreadable-object (pq stream)
     (format stream "Physical dimension quantity [debug] mag ~a"
      (pq-magnitude pq)))
   #-debug
   (if (let ((dim (pq-dimension pq)))
	 (and dim (arrayp dim) (null (row-major-aref dim 0))))
       ;; units not set yet
       (print-unreadable-object (pq stream)
	 (format stream "Physical dimension quantity mag ~a, unspecified units"
		 (pq-magnitude pq)))
       (cl-readable-nf (nf pq stream))))

(defgeneric pqval (pq &rest sysunits-additional-units)
  (:documentation
   "Get the numerical value and the units of the physical dimension quantity.  Returns the magnitude, the units, and whether units are the same for each element of a sequence or grid.")
  (:method (pq &rest sysunits-additional-units)
    (declare (ignore sysunits-additional-units))
    (values pq nil nil)))

(defmethod pqval ((pq physical-quantity) &rest sysunits-additional-units)
  (let ((units
	 (make-unit-sexp
	  (pq-dimension pq)
	  (apply #'augment-sysunits sysunits-additional-units)
	  (scalar-dimension pq))))
    (if (scalar-dimension pq)
	(values
	  (antik:/ (pq-magnitude pq) (nth-value 1 (parse-unit-sexp units)))
	  units
	  t)
	;; Grid dimensions
	;;(funcall (grid:elementwise ))
	(loop with newmag = (grid:copy-to (pq-magnitude pq) (grid:gridp (pq-magnitude pq)))
	      with newdim = (grid:copy-to (pq-dimension pq) 'array)
	      for i from 0 below (grid:total-size pq)
	      do
	   (multiple-value-bind (mag dim)
	       (pqval (grid:aref* pq i))
	     (setf (grid:aref* newmag i) mag
		   (grid:aref* newdim i) dim))
	      finally (return (values newmag newdim nil))))))

(defparameter *pqval-allsame-sequence-collapse* t
  "If a sequence passed to pqval has all the same units,
   collapse to one unit specification.")

(defmethod pqval ((pq sequence) &rest sysunits-additional-units)
  (let ((mat
	 (transpose-list
	  (map 'list
	       (lambda (x)
		 (multiple-value-list
		  (apply #'pqval x sysunits-additional-units)))
	       pq))))
    (multiple-value-bind (allsame-val allsame-same)
	(all-same (second mat) :test #'equal) ; all same units
      (let* ((allsame-nz
	      ;; all non-zero quantities have the same units
	      (when (and *pqval-allsame-sequence-collapse*
			 (not allsame-same)
			 *zero-is-dimensionless*)
		;; Check again if zeros are gone
		(let ((removed-zeros (remove-if 'zerop pq)))
		  (when (< (length removed-zeros) (length pq))
		    (multiple-value-bind (ignore units asnz)
			(apply
			 #'pqval removed-zeros sysunits-additional-units)
		      (declare (ignore ignore))
		      (when asnz units))))))
	     (as (and *pqval-allsame-sequence-collapse*
		      (or allsame-same allsame-nz))))
	(values
	 (if (vectorp pq) (coerce (first mat) 'vector) (first mat))
	 ;; If all units are the same, return them, else return a list of units.
	 (if as
	     (if allsame-same allsame-val allsame-nz)
	     (second mat))
	 as)))))

(defun pqval-seq (pq &rest sysunits-additional-units)
  (let ((*pqval-allsame-sequence-collapse* nil))
    (apply #'pqval pq sysunits-additional-units)))

;;; This should really get a more generic name, as it will
;;; routinely replace nf.
(defun new-units
    (pq &optional (sysunits-additional-units (list *system-of-units*))
	(stream *standard-output*))
  "Format the physical dimension quantity in units different than the default.
   Note that sysunits-additional-units must be a list."
  (with-nf-options
      (time nil degrees nil)
    (let ((*system-of-units*
	   (apply #'augment-sysunits sysunits-additional-units)))
      (nf pq stream))))

;;;;****************************************************************************
;;;; Interface
;;;;****************************************************************************

(defmacro with-pq (physical-quantities &body body)
  "Ensure that the named physical dimension quantities are of the right dimensions, or convert them from plain numbers using the current system of units as specified by *system-of-units*."
  ;; should I have the option to return plain numbers
  ;; in some system of units as well?
  `(let ,(mapcar
	  ;; check that value is non-nil before make-pq
	  (lambda (vu)
	    (destructuring-bind (v u) vu `(,v (and ,v (make-pq ,v ',u)))))
	  physical-quantities)
     ,@body))

(defun read-pq-from-string (string units &optional sysunits)
  "Read the next number and interpret as a pdq, if units is non-nil.
   The special 'pointer gives the starting location to read
   and is updated after the read." 
  (declare (special pointer))
  (multiple-value-bind (mag ptr)
      (read-from-string
       string nil nil :start (if (boundp 'pointer) pointer 0))
    (setf pointer ptr)
    (if units (make-pq mag units sysunits) mag)))

;;; Use #_ for physical dimension quantities, e.g. #_90_degree
(set-dispatch-macro-character
 #\# #\_
 (lambda (stream subchar arg)
   (declare (ignore subchar arg))
   (let ((*zero-is-dimensionless* nil)
	 numerical-value units)
     ;; Tell the reader to read the things
     (setf numerical-value (stream-to-string stream :terminator #\_)
	   units (read stream t nil t))
     ;; Make the PDQ if reading is not suppressed
     (unless *read-suppress*
       (make-pq
	(read-from-string numerical-value)
	units
	nil
	(scalar-units-p units))))))

(defmethod creation-form ((pq physical-quantity))
 (creation-form-readably
   pq
   `(let ((*zero-is-dimensionless* nil))
      (make-pq-object
       ,(creation-form (pq-magnitude pq))
       ,(creation-form (pq-dimension pq))
       ,(scalar-dimension pq)))))

(def-make-load-form physical-quantity)
