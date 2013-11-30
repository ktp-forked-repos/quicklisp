;; Functions on cartesian space
;; Liam Healy 2010-12-26 20:58:14EST cartesian.lisp
;; Time-stamp: <2012-06-03 22:34:26EDT cartesian.lisp>

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

(export '(vector-angle right-angle
	  coplanar distance
	  first-3vector second-3vector coordinate-unit-vector))

(defun vector-angle (a b)
  "The short angle between a and b, and the rotation vector."
  (values
    (acos (/ (grid:inner a b) (* (grid:norm a) (grid:norm b))))
    (grid:normalize (grid:cross a b))))

(defun right-angle (vector)
  "Find an arbitrary right angle to the vector."
  (let* ((vec (pqval vector))
	 (bigind
	   ;; Find which component is the largest
	   (iterate (for i vector-element-index vector)
	     (finding i maximizing (abs (grid:aref vec i)))))
	 (project (grid:copy-to vec))
	 (return (grid:copy-to vec)))
    ;; Find vector projected into the plane perpendicular
    (setf (grid:aref project bigind) 0.0
	  ;; The return vector is the original vector with a different
	  ;; value in what was the largest component.
	  (grid:aref return bigind)
	  (cl:- (cl:/ (grid:inner project project) (grid:aref vec bigind))))
    (grid:normalize return)))

(defun coplanar (vect1-or-matrix &optional vect2 vect3)
  "The cosine of the angle between vect1 and the cross product of vect2 and vect3.
   This should be near zero if the vectors are coplanar."
  (if (cl:> (grid:rank vect1-or-matrix) 1)
      (coplanar (grid:column vect1-or-matrix 0)
		(grid:column vect1-or-matrix 1)
		(grid:column vect1-or-matrix 2))
      (let ((x23 (grid:cross vect2 vect3)))
	(cos (vector-angle vect1-or-matrix x23)))))

(defun distance (a b &optional (combination-function '-))
  "The length of the vector and the vector difference a-b."
  (let ((diff
	  (grid:map-n-grids
	   :sources (list (list a nil nil) (list b nil nil))
	   :combination-function combination-function
	   :combine-destination nil)))
    (values (grid:norm diff) diff)))

(defun first-3vector (vec)
  "Extract the first 3-vector."
  (grid:subgrid vec '(3) '(0)))

(defun second-3vector (vec)
  "Extract the second 3-vector."
  (grid:subgrid vec '(3) '(3)))

(defun coordinate-unit-vector (i &optional (length 3))
  "A coordinate unit vector."
  (let ((vec (grid:make-simple-grid :dimensions length :initial-element 0.0d0)))
    (setf (grid:aref vec i) 1.0d0)
    vec))
