;; Utility definitions
;; Liam Healy 2010-12-25 12:14:58EST utility.lisp
;; Time-stamp: <2011-08-24 17:06:36EDT utility.lisp>

;; Copyright 2011 Liam M. Healy, Paul Graham
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

(export '(mkstr symb single all-same stream-to-string transpose-list))
(export '(pythagorean-sum pythagorean-complement))

(defun mkstr (&rest args)     ; From "On Lisp", copied with permission
  "Make a string out of the printed representations of the arguments." ; LMH
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)      ; From "On Lisp", copied with permission
  "Make a symbol out of the printed representations of the arguments." ; LMH
  (values (intern (apply #'mkstr args))))

(defun antik-symbol (symbol)
  (alexandria:ensure-symbol
   (if (stringp symbol) (string-upcase symbol) symbol)
   :antik))

(defun single (lst)	      ; From "On Lisp", copied with permission
  "Test list for one element."		; LMH
  (and (consp lst) (not (cdr lst))))

(defun all-same (list &key (test #'eql) post-function)
  "If all elements are the same, that value and T are returned; if
   not, nil and nil are returned.  The post-function is applied to the
   common value before returning the answer if there is one."
  (if (or (single list) (every (lambda (x) (funcall test (first list) x)) list))
      (values
	(if post-function
	    (funcall post-function (first list))
	    (first list))
	t)
      (values nil nil)))

(defun stream-to-string (stream &key terminator terminate-if)
  "Fill a string with the contents of the stream.  Stop when an EOF
   is encountered, or when the terminating character :terminator,
   which is discarded, is reached, or the function :terminate-if
   is satisfied on the character, which is returned to the stream.
   Only one of :terminator and :terminate-if should be specified."
  ;; Both terminator and terminate-if can be specified,
  ;; but the unread-char might not work right.
  (with-output-to-string
    (out)
    (loop for ch = (read-char stream nil nil)
	  until (or (null ch)
		    (and terminator (eq ch terminator))
		    (and terminate-if (funcall terminate-if ch)))
	  do (write-char ch out)
	  finally
	  (when terminate-if (unread-char ch stream)))))

;; Someday grid:transpose should perform this function.
(defun transpose-list (lists)
  (assert (and lists (listp lists)) (lists) "Argument must be a list of lists.")
  (cond ((some #'null lists) '())
	(t (cons (mapcar #'car lists)
		 (transpose-list (mapcar #'cdr lists))))))

(defparameter *type-equality*
    `((sequence . ,#'equal) (string . ,#'string-equal) (t . ,#'eql)))

(defmacro pythagorean-sum (&rest args)
  "Sum the squares of the arguments."
  `(+ ,@(mapcar #'(lambda (x) `(expt ,x 2)) args)))

(defmacro pythagorean-complement (x)
  "sqrt(1-x^2)"
  `(sqrt (- 1 (expt ,x 2))))

;;; It is necessary to shadow these symbols in order to define types.
;;; These macros are provided to make them transparently available as
;;; their function/macro bindings.
(defmacro length (&rest args) `(cl:length ,@args))
(defmacro time (&rest args) `(cl:time ,@args))

(defun map-leaf (function tree)
  "Map onto the leaves (non-list) of the tree."
  (if (listp tree)
      (mapcar (alexandria:curry 'map-leaf function) tree)
      (funcall function tree)))
