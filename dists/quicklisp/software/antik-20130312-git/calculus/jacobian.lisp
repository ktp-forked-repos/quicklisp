;; Find the Jacobian matrix of a multivariate function
;; Liam Healy 2011-10-15 12:40:05EDT jacobian.lisp
;; Time-stamp: <2011-10-15 15:37:37EDT jacobian.lisp>

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

;;; This does not use GSLL's central-difference, because that is scalar only and it would be inefficient to repeatedly re-evaluate the function to get the various components.

(in-package :antik)

(defun jacobian-matrix
    (function point step
     &optional (matrix-or-nrows (grid:dim0 (funcall function point))))
  "The matrix of partial derivatives of the function.  If the number
of rows nrows is not supplied, it is determined by evaluating the
function at the point; supplying this information avoids that
calculation.  Step must be a scalar."
  ;; This cannot currently use pq.
  (let ((matrix (if (grid:gridp matrix-or-nrows)
		    matrix-or-nrows
		    (grid:make-simple-grid
		     :dimensions (list (grid:dim0 point) matrix-or-nrows)
		     :grid-type (grid:gridp point)))))
    (iter (for col from 0 below (grid:dim1 matrix))
      (let ((pos (grid:copy-to point))
	    (neg (grid:copy-to point)))
	(incf (grid:aref pos col) (/ step 2))
	(decf (grid:aref neg col) (/ step 2))
	(setf (grid:column matrix col)
	      (/ (- (funcall function pos) (funcall function neg)) step))))
    matrix))
