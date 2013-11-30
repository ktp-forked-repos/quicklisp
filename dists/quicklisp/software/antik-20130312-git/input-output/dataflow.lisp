;; Record the data as it is transformed by functions, for research, pedagogical, or debugging.
;; Liam Healy 2013-02-17 11:02:49HST dataflow.lisp
;; Time-stamp: <2013-02-17 17:54:23HST dataflow.lisp>

;;; Record values or location in the calculation.  This is somewhat like reproducible research without the literate programming part.

(in-package :antik)

(export '(tags get-recorded-data clear-recorded-data record-value computation))

;;;;****************************************************************************
;;;; Tags: Keyword symbols added or removed in source code to identify context
;;;;****************************************************************************

(defparameter *context-tags* nil
  "A list of tags that indicate the context of the calculation.")

;;; Before compiling, remove :recording from *features* in order to maximize speed
;; #-recording
;; (defmacro tags (add-tags remove-tags &body body)
;;   (declare (ignore add-tags remove-tags))
;;   `(progn ,@body))

;;;#+recording
(defmacro tags ((add-tags &optional remove-tags) &body body)
  "Add or remove tags.  This macro is used in source code to identify context for data recording."
  `(let ((*context-tags*
	   (append ,add-tags (set-difference *context-tags* ,remove-tags))))
     ,@body))

;;; Tag expression similar to feature expressions (file:///usr/share/doc/hyperspec/Body/24_aba.htm).
;;; mapcar `(member ,atom *context-tags*) onto atoms, so that
;;; (and (or (member cat *context-tags*) (member dog *context-tags*)) (member horse *context-tags*))
;;; Maybe eventually check values, e.g. if an iteration count exceeds some value.
;;; (recorded-context-p '(or (and cat dog) horse) '(cat fish)) ;;  => NIL
;;; (recorded-context-p '(or (and cat dog) horse) '(dog cat)) ;; => (DOG CAT)
(defun recorded-context-p (tag-expression tags)
  "Read tag expression and check that tags match"
  (alexandria:once-only ((tags `',tags))
    (labels ((in-taglist (sexp)
	       (etypecase sexp
		 (list (cons (first sexp) (mapcar #'in-taglist (rest sexp))))
		 (atom `(member ',sexp ,tags)))))
      (in-taglist tag-expression))))

;;;;****************************************************************************
;;;; Data table: global hash table with one entry for each computation sequence.
;;;;****************************************************************************

;;; Each value is saved as (value metadata ...), with metadata a plist.  For ease of retrieval, metadata should include a :name, which should be unique within the calculation sequence.

(defvar *current-recorded-data* nil "The data currently being recorded.")
(defvar *recorded-data* (make-hash-table)
  "The table of data recorded, indexed by computation identifier.")

(defun save-value (value metadata)
  "Save the recorded value and metadata for later playback; return the value."
  (push (cons value metadata) *current-recorded-data*)
  value)

(defmacro get-recorded-data (identifier)
  `(gethash ,identifier *recorded-data*))

(defun clear-recorded-data ()
  (clrhash *recorded-data*))

(defvar *last-identifier* nil)
(defun get-value (name &optional (identifier *last-identifier*))
  (find name (get-recorded-data identifier) :key (lambda (x) (getf (rest x) :name))))

;;;;****************************************************************************
;;;; Data recording: record value and metadata, define computation sequence
;;;;****************************************************************************

;;; (record-value (/ (+ r1 r2) 2) "The semimajor axis of the transfer ellipse in the Hohmann transfer" :plain "atrans" :tex-subscript "t" :attribute 'semimajor-axis)
(defmacro record-value (value &optional metadata)
  "If dynamically surrounded by #'tags and the tag-expression is satisfied, record value and metadata (a plist).  This form is used in code to record values.  It returns the value."
  (alexandria:once-only (value)
    `(if (recorded-context-p *tag-expression* *context-tags*)
	 (save-value ,value ,metadata)
	 ,value)))

;;; Every entrance (thread, recursion) will need to use this macro?
(defmacro computation
    ((tag-expression &optional (identifier (get-universal-time)) continue-from-identifier)
     &body body)
  "Compute the form(s) while saving input, intermediate and result data.  A tag-expression is specified to indicate which values should be recorded.  This form is used at runtime to identify a single set of calculations for recording."
  `(let ((*current-recorded-data*
	   (if ,continue-from-identifier
	       (copy-list (gethash ,continue-from-identifier *recorded-data*))
	       (list)))
	 (*tag-expression* ',tag-expression))
     (setf *last-identifier* ,identifier)
     (unwind-protect
	  ,@body
       (setf (get-recorded-data ,identifier) *current-recorded-data*)
       ,identifier)))
