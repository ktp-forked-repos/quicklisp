;; Global/local parameters to pass to functions
;; Liam Healy Sat Apr 30 2005 - 21:12
;; Time-stamp: <2013-02-17 23:00:49HST parameters.lisp>

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

(export '(define-parameter-category
	  parameter-value parameter-value*
	  set-parameter-value set-parameters
	  with-parameters define-parameter parameter-help
	  parameters-set reset-parameters
	  make-parameters-from-table))

#||
;;; Parameters are values that are bound by the user or by functions,
;;; and are read by functions.

;;; This system allows:
;;; - grouping by functionality
;;; - restriction on type
;;; - default values
;;; - easily accessible documentation
;;; - dynamic scope

;;; This is useful when needed visibility of bindings is spread thin
;;; and wide, making arglists clumsy, or if there are many parameters,
;;; making specials too numerous.

;;; Examples
;;; First create the category
(define-parameter-category kepler)

;;; Then define some parameters
(define-parameter kepler foo 122 fixnum "A fixnum parameter of kepler.")
(define-parameter kepler bar "hi" string "A string parameter of kepler.")

;;; Get their values
(parameter-value kepler bar)
"hi"
(parameter-value kepler foo)
122

;;; Dynamic binding
(defun show-foo-bar ()
  (format t "~&foo: ~a, bar: ~s"
	  (parameter-value kepler foo)
	  (parameter-value kepler bar)))

(show-foo-bar)
foo: 122, bar: "hi"
NIL

;;; Locally change values
(with-parameters (kepler foo 143 bar "bye")
  (show-foo-bar))
foo: 143, bar: "bye"
NIL

(show-foo-bar)
foo: 122, bar: "hi"
NIL

;;; Make a mistake
(with-parameters (kepler foo 143 bar -44)
  (show-foo-bar))
Error: Value -44 is of type FIXNUM, not of the required type STRING.

;;; Globally change values
(set-parameter-value kepler bar "a new value")
foo: 122, bar: "a new value"

;;; Set multple values
(set-parameters kepler bar "xyz" foo 1)

;;; Get information about the categories and parameters
(parameter-help)
Parameter categories: KEPLER.

(parameter-help :kepler)
Parameters in KEPLER: BAR and FOO.

(parameter-help :kepler :bar)
BAR: A string parameter of kepler.
Type is: STRING, default value is "hi".

(parameter-value* kepler (mkstr "FO" "O"))

||#

;;; Wishlist:
;;; multiple defaults: "parameter sets"
;;; Improvement of parameter-help to get current value.
;;; Parameters-used list generation.

;;;;****************************************************************************
;;;; Internal definitions, not seen by applications
;;;;****************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *parameters* nil)
)

;;; (defconstant +no-value+ (make-symbol "NO-VALUE"))

(defun special-symbol-lookup (category)
  (or (first (getf *parameters* (alexandria:make-keyword category)))
      (error "Parameter category ~a is unknown" category)))

(defmacro parameter-access ((category parameter-symbol) &body body)
  "Give access to the parameter special, for use in other parameter macros."
  `(let ((,parameter-symbol (special-symbol-lookup ,category)))
     `(locally (declare (special ,,parameter-symbol))
	,,@body)))

;;;;****************************************************************************
;;;; Make category, get and set parameter values
;;;;****************************************************************************

(defmacro define-parameter-category (name)
  "Define a new category for parameters."
  ;; Can't make-symbol because definition and use may happen in
  ;; separate files.  Still want non-conflict with any user symbols
  ;; however.
  (let ((symbol
	 (intern (format nil "/~a-PARAMETERS/" (string-upcase (string name))))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (locally (declare (special ,symbol))
	 (setf (getf *parameters* ,(alexandria:make-keyword name))
	       (list ',symbol)
	       ,symbol nil)))))

(defmacro parameter-value
    (category name &optional (default (default-value category name)))
  "Get or set the parameter value."
  (parameter-access (category symbol)
    `(getf ,symbol ,(alexandria:make-keyword name) ',default)))

;;; Could replace parameter-value with this?
(defmacro parameter-value*
    (category name &optional default)
  "Get the parameter value dynamically."
  (let ((nsym (gensym)))
    (parameter-access (category symbol)
      `(let ((,nsym ,name))
	(getf ,symbol ,nsym
	 (or ,default (default-value ',category ,nsym)))))))

(defmacro set-parameter-value* (category name value)
  "Set the parameter value without checking."
  (parameter-access (category symbol)
    `(setf (getf ,symbol ,(alexandria:make-keyword name)) ,value)))

(defmacro with-parameters ((category &rest name-values) &body body)
  "Provide local scope for parameter values, and possibly
   bind new values."
  (let ((special-symbol (special-symbol-lookup category)))
    `(locally (declare (special ,special-symbol))
       (let ((,special-symbol (copy-list ,special-symbol)))
	 (declare (special ,special-symbol))
	 ,@(when name-values
	     `((setf
		,@(loop for (name value) on name-values by #'cddr
		     append
		     `((parameter-value ,category ,(alexandria:make-keyword name))
		       ,value)))))
	 ,@body))))

;;; Except for the lack of a definition of #'default-value, the above
;;; four macros function fine without any of the definitions below.

;;;;****************************************************************************
;;;; Registration of parameters
;;;;****************************************************************************

;;;  Parameters may be registered in advance with #'define-parameter.

(defstruct (parameter
	     (:constructor make-par-int
			   (category name default type description)))
  ;; 'attribute is used later, for attributes
  category name default type description attribute)

(defun define-parameter-int (category name default type description)
  (check-parameter-type default type t category name)
  (push (make-par-int
	 category name default type description)
	(rest (or (getf *parameters* category)
		  ;; Could create a category?
		  (error "No such category: ~a" category)))))

(defmacro define-parameter (category name default type description)
  "Define the parameter name in category."
  (let ((kcat (alexandria:make-keyword category))
	(kname (alexandria:make-keyword name)))
    ;; The default must be of the correct type.
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-parameter-int ,kcat ,kname ,default ',type ,description))))

(defun all-parameters (category)
  "All the parameters for a category."
  (rest (getf *parameters* (alexandria:make-keyword category))))

(defun find-parameter (category name)
  (or
   (find (alexandria:make-keyword name) (all-parameters category) :key #'parameter-name)
   (error "No parameter ~a known in ~a." name category)))

(defparameter *parameter-check-function* 'typep)

(defun check-parameter-type (value type error category name)
  (unless (funcall *parameter-check-function* value type)
    (if error 
	(error "Parameter ~a/~a value ~a is not the required type or dimension ~a"
	       category name value type))))

(defun check-parameter (category name value &optional error)
  "Check that the value is appropriate for this parameter.
   If the parameter is unknown, an error is signalled."
  (let ((parinfo (find-parameter category name)))
    (when parinfo
      (check-parameter-type
       value (parameter-type parinfo) error category name))))

(defun format-list (list stream)
  (apply #'format stream
	 ;;; Shamelessly cribbed from the Hyperspec.
	 "~#[none~;~a~;~a and ~a~:;~@{~#[~; and ~]~a~^, ~}~]."
	 (sort list #'string-lessp)))

(defun parameter-help (&optional category name (stream t))
  "Print all information known about the parameter.  If category is nil (default), names of all categories are printed.  If name is nil, all defined parameters in that category are printed."
  ;; If T passed for name, only parameters set to non-default values
  ;; will be printed?
  (if category
      (if name
	  (let ((parinfo (find-parameter category name)))
	    (if (typep parinfo 'parameter)
		(format stream "~&~a: ~a~&Type is ~s, ~
                        default value is ~s, ~&current value is ~s."
			(parameter-name parinfo)
			(parameter-description parinfo)
			(parameter-type parinfo)
			(parameter-default parinfo)
			(eval `(parameter-value ,category ,name)))
		(format stream "~&No information on ~a." name)))
	  (format stream "~&Parameters in ~a: ~a"
		  category
		  (format-list
		   (mapcar #'parameter-name (all-parameters category))
		   nil)))
      (format stream "~&Parameter categories: ~a"
	      (format-list
	       (loop for name on *parameters* by #'cddr collect (first name))
	       nil))))

(defmacro set-parameter-value (category name value)
  "Set the parameter value, checking that the name is legitimate and
   the value is of the correct type."
  `(progn
     (check-parameter ,(alexandria:make-keyword category) ,(alexandria:make-keyword name) ,value t)
     (set-parameter-value* ,category ,name ,value)))

(defsetf parameter-value set-parameter-value)

(defmacro set-parameters (category &rest names-values)
  "Set the values of the parameters."
  `(setf 
    ,@(loop for (name value) on names-values by #'cddr
	 append `((parameter-value ,category ,name) ,value))))

;;;;****************************************************************************
;;;; Defaults
;;;;****************************************************************************

(defun default-value (category name)
  "The default value of a parameter."
  (parameter-default (find-parameter category name)))

(defmacro parameters-set (category)
  "A list of parameters that are set."
  (parameter-access (category symbol)
    `(loop for (key value) on ,symbol by #'cddr
	collect (list key value))))

(defmacro reset-parameters (category &rest names)
  "Reset the parameter(s) to the default value; if none are specified,
   all in the category are reset."
  (parameter-access (category symbol)
    (if names
	`(dolist (name ',names)
	  (remf ,symbol (alexandria:make-keyword name)))
	`(setf ,symbol nil))))

;;;;****************************************************************************
;;;; Make parameters
;;;;****************************************************************************

;;; This function is designed to read an org-mode table that is parsed and passed to CL by org-babel.
;;; See the file input-output/org-mode.lisp for documentation and examples
(defun make-parameters-from-table
    (table
     &key (headerp t) (category (first *parameters*)) (type grid::*default-element-type*) (prefix :par))
  "From the list of lists, define the parameters.  The optional header should have column names.  These column names include 'category 'name 'default 'value 'type 'description ('value and 'default mean the same thing).  Any column names not specified will receive a default value.  Any values in the cells that are empty will receive a default value.  Any columns given with a header not on the list will be ignored.  Category must already exist."
  (let* ((header (when headerp (first table)))
	 (tbl (if headerp (rest table) table))
	 (catpos (position :category header :test 'string-equal))
	 (nampos (position :name header :test 'string-equal))
	 (valpos (or (position :default header :test 'string-equal)
		     (position :value header :test 'string-equal)))
	 (typepos (position :type header :test 'string-equal))
	 (descpos (position :description header :test 'string-equal))
	 (counter 0))
    ;; Should the type be 
    (dolist (row tbl)
      (incf counter)
      (let ((valtype
	      (find-symbol
	       (string-upcase
		(if (and typepos (stringp (nth typepos row)) (plusp (length (nth typepos row))))
		    (nth typepos row)
		    type)))))
	(define-parameter-int
	    ;; Category
	    (alexandria:make-keyword
	     (string-upcase
	      (if (and catpos (stringp (nth catpos row)) (plusp (length (nth catpos row))))
		  (nth catpos row)
		  category)))
	    ;; Name
	    (alexandria:make-keyword
	     (if (and nampos (stringp (nth nampos row)) (plusp (length (nth nampos row))))
		 (string-upcase (nth nampos row))
		 (alexandria:symbolicate prefix (format nil "~4,'0,d" counter))))
	  ;; Value (default, or initial)
	  (let ((val (when (and valpos (not (equal (nth valpos row) ""))) (nth valpos row))))
	    (etypecase val
	      (number val)
	      (string (read-from-string val))
	      (null (antik:coerce grid::*default-numerical-value* valtype))))
	  ;; Type
	  valtype
	  ;; Description
	  (if (and descpos (plusp (length (nth descpos row))))
	      (nth descpos row)
	      "No description."))))))
