;; Methods for lisp-unit functions
;; Liam Healy 2010-08-22 19:10:26EDT lisp-unit-extension.lisp
;; Time-stamp: <2011-01-09 22:15:51EST lisp-unit-extension.lisp>

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

(in-package :lisp-unit)

(defmethod norm-equal
    ((data1 grid:foreign-array) (data2 grid:foreign-array)
     &optional (epsilon *epsilon*) (measure *measure*))
  (norm-equal (grid:copy-to data1) (grid:copy-to data2)
	      epsilon measure))

(defmethod numerical-equal
    ((result1 grid:foreign-array) (result2 grid:foreign-array)
     &key (test #'number-equal))
  "Return true if the arrays are equal in dimension and each element
is equal according to :TEST."
  (and (equal (grid:dimensions result1) (grid:dimensions result2))
       (numerical-equal (grid:copy-to result1 'array)
			(grid:copy-to result2 'array)
			:test test)))
