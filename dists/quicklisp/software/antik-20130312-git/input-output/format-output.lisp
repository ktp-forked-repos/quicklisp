;; Format numerical output
;; Liam Healy Wed Dec 11 2002 - 16:37
;; Time-stamp: <2013-02-02 10:03:43EST format-output.lisp>

;; Copyright 2011, 2013 Liam M. Healy
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

(export '(nf nf-string nf-option nf-readably set-nf-options
	  with-nf-options texstyle object-as-nf))

;;;;****************************************************************************
;;;; Parameters
;;;;****************************************************************************

(define-parameter-category nf)

(define-parameter nf :style 20
  (or null fixnum (member :tex :short :readable))
    "Style of format, plain (nil), LaTeX (:tex), or plain but
     (possibly) shortened (:short or a number).  If :short a short
     form will be used if available, if a fixnum is supplied, the
     short form will be used if available and the space available is
     less than the space taken by the long form.  If :readable, a form
     that is readable to CL is produced, if :reproducible a readble
     form is produced, and on reading object will be recreated
     exactly.")

;;;;****************************************************************************
;;;; Macros for parameter access
;;;;****************************************************************************

(defmacro nf-option (name)
  "Get/set the nf option named."
  `(parameter-value :nf ,name))

(defun nf-readably ()
  "The format produced will be readable to Lisp."
  (member (nf-option :style) '(:readable)))

(defmacro set-nf-options (&rest name-values)
  "Set the numerical formatting options."
  `(set-parameters :nf ,@name-values))

(defmacro with-nf-options ((&rest name-values) &body body)
  "Set the options for all nf calls within body to inherit."
  `(with-parameters (nf ,@name-values)
    ,@body))

(define-symbol-macro texstyle
  (and (eq (nf-option style) :tex)))

;;;;****************************************************************************
;;;; General nf
;;;;****************************************************************************

(defmacro princn (object stream)
  `(if ,stream (princ ,object ,stream) ,object))

(defgeneric nf (object &optional stream)
  (:documentation "Format output for numerical objects.  If stream is nil, use *standard-output*.")
  (:method ((object t) &optional (stream *standard-output*))
    (if (grid:gridp object)
	(nf-grid object stream)
	;; by default, just print to stream
	(princ object stream))))

(defun nf-string (object)
  "Format output for numerical objects to a new string."
  (with-output-to-string (stream)
    (nf object stream)))

(defmethod nf
    ((object null) &optional (stream *standard-output*))
  (princ "---" stream))

;;;;****************************************************************************
;;;; Numbers nf
;;;;****************************************************************************

(define-parameter nf :significant-figures 4
  (or null fixnum)
  "The number of significant figures formatted for numbers.
   If nil, formatting of numbers is done using :intpart-digits
   and, if a float, :fracpart-digits.")

(define-parameter nf :intpart-digits
  nil (or null fixnum)
  "The minimum space allowed for the whole-number part of numbers
   when :significant-figures is nil.  If this is larger than
   the actual number of digits, pad to the left with spaces.
   If this value is nil, allow enough space to accomodate the number.")

(define-parameter nf :print-sign
  nil t
  "Whether leading `+' is printed.")

(defmethod nf
    ((object integer) &optional (stream *standard-output*))
  (format stream "~vd"
	  (or (nf-option significant-figures)
	      (nf-option intpart-digits))
	  object))

(defmethod nf ((number ratio) &optional (stream *standard-output*))
  (if texstyle
      (format stream "~:[~;-~]\\frac{~d}{~d}"
	      (minusp number)
	      (abs (numerator number))
	      (denominator number))
    (call-next-method)))

;;; More in float.lisp

;;;;****************************************************************************
;;;; Sequences nf
;;;;****************************************************************************

(define-parameter nf :vector-format
  :horizontal (member :horizontal :vertical :coordinate-unit-vectors)
  "Rank-1 grids (vectors) are formatted as rows, columns or as linear
  combination of coordinate unit vectors.")

(define-parameter nf :components
  '("I" "J" "K") list
  "Names of vector components to use :vector-format is :coordinate-unit-vectors.")

(define-parameter nf :tex-element-separator
  (format nil " \\\\~%")  string
  "What to put between vertically separated elements for LaTeX.")

(define-parameter nf :tex-decimal-align
  t boolean
  "Align columns of numbers on decimal point in LaTeX.")

(define-parameter nf :vertical-element-separator
  #\Newline character
  "What to put between vertically separated elements for plain style.")

(define-parameter nf :horizontal-element-separator
  #\space character
  "What to put between horizontally separated elements for plain style.")

(defun vector-component-names (i)
  "The name of the ith vector component."
  (let ((source (nf-option components)))
    (elt source (mod i (length source)))))

(defmethod nf ((object sequence) &optional (stream *standard-output*))
  (when (grid:gridp object)
    (nf-grid object stream)))

   #+(or)
   (format stream "~a~{~a~^ ~})"
	   (if (vectorp object) "#(" "'(")
	   (map 'list (lambda (o) (nf-string o)) object))

(defun nf-grid (object &optional (stream *standard-output*))
  "Format the grid."
  (grid::print-grid-readably
   (when (nf-readably) object)
   (lambda (s)
     (ecase (grid:rank object)
       (1 (with-nf-options (:vector-format (if (nf-readably) :horizontal (nf-option :vector-format)))
	    (if (member (nf-option :vector-format) '(:horizontal :vertical))
		(nf-matrix object s)
		(nf-vector object s))))
       (2 (nf-matrix object s))))
   stream)
  object)

(defun nf-vector (object &optional (stream *standard-output*))
  "Format the one dimensional grid."
  ;; Note: cannot handle lists yet.
  ;; This is called from nf-grid, called only when :vector-format is :coordinate-unit-vectors.
  (dotimes (i (grid:dim0 object))
    ;; iterate formatting each element
    (with-nf-options
	;; If formatting as a linear combination with element labels,
	;; force a + sign except for the first element
	;; to make a mathematical expression.
	(print-sign
	 (and (eq (nf-option :vector-format) :coordinate-unit-vectors)
	      (not (zerop i))))
      ;; format the element itself
      (nf (grid:aref object i) stream)
      ;; append either the element label or the separator
      (princ
       (if (nf-readably)
	   #\space
	   (if texstyle
	       (case (nf-option :vector-format)
		 (:coordinate-unit-vectors (format nil "\\unitvec{~a}" (vector-component-names i)))
		 (:vertical "\\\\")
		 (:horizontal " "))
	       (case (nf-option :vector-format)
		 (:coordinate-unit-vectors (vector-component-names i))
		 (:vertical #\newline)
		 (:horizontal #\space))))
       stream))))

(defun parenstyle (outer)
  "Parentheses should be used in this case."
  (and outer
       (or (member (nf-option style) '(:readable))
	   (numberp (nf-option style)))))

(defun nf-matrix (object &optional (stream *standard-output*))
  "Format the two dimensional grid, or the one dimensional grid horizontally or vertically."
  (flet ((element (i j)
	   (if (grid:dim1 object)
	       (grid:aref object i j)
	       (case (nf-option :vector-format)
		 (:vertical (grid:aref object i))
		 (:horizontal (grid:aref object j))))))
    (let ((dim1 (or (grid:dim1 object)
		    (if (eq (nf-option :vector-format) :vertical)
			1
			(grid:dim0 object))))
	  (dim0 (if (or (grid:dim1 object) (eq (nf-option :vector-format) :vertical))
		    (grid:dim0 object)
		    1)))
      (when texstyle
	(format stream "\\left[\\begin{array}{~a}~&"
		(if (nf-option :tex-decimal-align)
		    (reduce (alexandria:curry #'concatenate 'string)
			    (make-list dim1 :initial-element "r@{.}l"))
		    (make-string dim1 :initial-element #\r))))
      (when (parenstyle t) (princ #\( stream)) ; open paren for whole array
      (dotimes (i dim0)
	(when (parenstyle (grid:dim1 object)) (princ #\( stream)) ; open paren for a row
	(dotimes (j dim1)
	  (if (and texstyle (nf-option :tex-decimal-align))
	      (princ
	       (substitute #\& #\. (nf-string (element i j)))
	       stream)
	      (nf (element i j) stream))
	  (when (< j (1- dim1))
	    (if texstyle
		(princ " & " stream)
		(princ (nf-option :horizontal-element-separator) stream))))
	(when (parenstyle (grid:dim1 object)) (princ #\) stream)) ; close paren for a row
	(when (< i (1- dim0))
	  (if texstyle
	      (princ (nf-option :tex-element-separator) stream)
	      (princ (nf-option :vertical-element-separator) stream))))
      ;; close out TeX array
      (when texstyle (format stream "~&\\end{array}\\right]"))
      (when (parenstyle t) (princ #\) stream)))) ; close paren for whole array
  nil)

(defun object-as-nf (object)
  "Define a new object exactly as the current nf options print an existing one.  This function can be used for example to define a number exactly as the rounded print format of another one."
  (read-from-string
   (with-nf-options (style :readable) (nf-string object))))

(defmacro cl-readable-nf (&body body)
  "For use in #'print-object methods that rely on nf methods for
   printing: they should print readably if cl:*print-escape* or
   cl:*print-readably* say to."
  `(with-nf-options
       (:style (if (readably) :readable (nf-option :style))
	       :full-precision (readably))
     ,@body))
