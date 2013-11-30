;; Tests of physical quantities
;; Liam Healy 2011-01-09 17:38:41EST tests.lisp
;; Time-stamp: <2011-06-23 13:41:18EDT physical-quantities.lisp>

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

(defmethod lisp-unit::numerical-equal
    ((result1 physical-quantity) (result2 physical-quantity) &rest keys &key test)
  (declare (ignorable test))
  (multiple-value-bind (mag2 units2)
      (pqval result2)
    (and
     (apply 'lisp-unit::numerical-equal (pq-magnitude result1) mag2 keys)
     (check-dimension result1 units2 nil))))

(defun check-pq (object value units)
  "Check that the object is a physical-quantity and that its magnitude
   and units are as given."
  (and (physical-quantity-p object)
       (lisp-unit::numerical-equal value (pq-magnitude object))
       (check-dimension object units nil)))

(lisp-unit:define-test units
  (lisp-unit:assert-true (check-pq #_1_m 1.0d0 'm))
  (lisp-unit:assert-false (check-pq #_1_m 1.0d0 's))
  (lisp-unit:assert-numerical-equal #_2.0_m (antik:+ #_1_m #_1_m))
  (lisp-unit:assert-error 'simple-error (antik:+ #_1_m #_1_s))
  (lisp-unit:assert-numerical-equal #_0.5_m/s (/ #_1_m #_2_s))
  (lisp-unit:assert-numerical-equal #_6.0_m^2 (* #_3_m #_2_m))
  (lisp-unit:assert-numerical-equal #_0.3048_m #_1_foot)
  (lisp-unit:assert-numerical-equal #_0.45359237_kg #_1_pound)
  (lisp-unit:assert-numerical-equal #_3600_s #_1_hour))

;;; These are all scalar units, i.e., the same units apply to each
;;; element of the array.
(lisp-unit:define-test grid-scalar-units
  (lisp-unit:assert-true
   ;; An array with pqs as elements
   (let ((grid #(#_1.0d0_m #_5.0d0_m)))
     (and (arrayp grid)
	  (equal (grid:dimensions grid) '(2))
	  (check-pq (grid:aref grid 0) 1.0d0 'meter)
	  (check-pq (grid:aref grid 1) 5.0d0 'meter))))
  (lisp-unit:assert-true
   ;; An array with pqs as elements
   (let ((grid (make-array '(2) :initial-contents '(#_1.0d0_m #_5.0d0_m))))
     (and (arrayp grid)
	  (equal (grid:dimensions grid) '(2))
	  (check-pq (grid:aref grid 0) 1.0d0 'meter)
	  (check-pq (grid:aref grid 1) 5.0d0 'meter))))
  (lisp-unit:assert-true
   ;; A physical-quantity with a foreign-array as magnitude
   (check-pq #m(#_1.0d0_m #_5.0d0_m) #m(1.0d0 5.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity with an array as magnitude
   (check-pq
    (grid:make-simple-grid
     :grid-type 'array
     :initial-contents (list #_1.0d0_m #_5.0d0_m))
    #(1.0d0 5.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity with a foreign-array as magnitude
   (check-pq
    (grid:make-foreign-array
     'double-float :dimensions '(2) :initial-contents '(#_1.0d0_m #_5.0d0_m))
    #m(1.0d0 5.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity with an array as magnitude
   (check-pq #_#(1.0d0 2.0d0)_m #(1.0d0 2.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity with an array as magnitude
   (check-pq (make-pq #(1.0d0 2.0d0) 'meter) #(1.0d0 2.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity with a foreign-array as magnitude
   (check-pq #_#m(1.0d0 5.0d0)_m #m(1.0d0 5.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity with a foreign-array as magnitude
   (check-pq (make-pq #m(1.0d0 5.0d0) 'meter) #m(1.0d0 5.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity using make-grid with :initial-element
   (check-pq
    (grid:make-grid '((grid:foreign-array 3) double-float)
		    :initial-element #_23.0_m)
    #m(23.0d0 23.0d0 23.0d0) 'meter))
  (lisp-unit:assert-true
   ;; A physical-quantity using make-grid with :initial-contents
   (let ((pq
	  (grid:make-grid '((grid:foreign-array 3) double-float)
			  :initial-contents '(#_1_km #_2_km #_3_km))))
     (and (scalar-dimension pq)
	  (check-pq pq #m(1.0d3 2.0d3 3.0d3) 'meter)))))

(lisp-unit:define-test grid-array-units
  (lisp-unit:assert-true
   ;; An array with pqs as elements
   (let ((grid #(#_1.0d0_m #_5.0d0_s)))
     (and (arrayp grid)
	  (equal (grid:dimensions grid) '(2))
	  (check-pq (grid:aref grid 0) 1.0d0 'meter)
	  (check-pq (grid:aref grid 1) 5.0d0 'second))))
  (lisp-unit:assert-true
   ;; An array with pqs as elements
   (let ((grid (make-array '(2) :initial-contents '(#_1.0d0_m #_5.0d0_s))))
     (and (arrayp grid)
	  (equal (grid:dimensions grid) '(2))
	  (check-pq (grid:aref grid 0) 1.0d0 'meter)
	  (check-pq (grid:aref grid 1) 5.0d0 'second))))
  (lisp-unit:assert-true
   ;; A physical-quantity with a foreign-array as magnitude
   (check-pq #m(#_1.0d0_m #_5.0d0_s) #m(1.0d0 5.0d0) #(meter second)))
  ;; This should work but it doesn't like #(meter) for units:
  ;;;(lisp-unit:assert-true
   ;; A physical-quantity with a foreign-array as magnitude, single
  ;;;   (check-pq #m(#_1.0d0_m) #m(1.0d0) #(meter)))
  (lisp-unit:assert-true
   ;; A vector with foreign-array magnitude and :initial-contents
   (check-pq
    (grid:make-grid '((grid:foreign-array 3) double-float)
		    :initial-contents '(#_1_km #_2_km #_3_sec))
    #m(1.0d3 2.0d3 3.0d0)
    #(meter meter second)))
  (lisp-unit:assert-true
   ;; A 2D foreign-array magnitude and :initial-contents
   (check-pq
    (grid:make-grid '((grid:foreign-array 3 2) double-float)
		    :initial-contents '((#_1_km #_2_km #_3_sec) (#_1_m #_2_m #_88_sec)))
    #m((1000.0d0 2000.0d0 3.0d0) (1.0d0 2.0d0 88.0d0))
    #2A((meter meter second) (meter meter second))))
  (lisp-unit:assert-true
   ;; Concatenate grids, array
   (check-pq (grid:concatenate-grids #_#(1.0d0 2.0d0)_m #_#(3.0d0 4.0d0)_s)
	     #(1.0d0 2.0d0 3.0d0 4.0d0)
	     #(meter meter second second)))
  (lisp-unit:assert-true
   ;; Concatenate grids, foreign-array
   (check-pq (grid:concatenate-grids #_#m(1.0d0 2.0d0)_m #_#m(3.0d0 4.0d0)_s)
	     #m(1.0d0 2.0d0 3.0d0 4.0d0)
	     #(meter meter second second))))

#|
;;; Things that should be made to work.
;;; Multiplication of dimensionless grid by dimensioned grid
(* #((1.0 0.0) (0.0 1.0)) #(#_1_km #_2_s))  ; works
(* #m((1.0 0.0) (0.0 1.0)) #m(#_1_km #_2_s))  ; does not work

;;; Add dimensioned
(+ #m(#_1_km #_2_s) #m(#_1_km #_2_s))

;;; Multiply dimensioned
(* #_#m((1.0 0.0) (0.0 1.0))_km #m(#_1_km #_2_s))
|#
