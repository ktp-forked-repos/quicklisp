;; Test functions of arithmetic functions of grids
;; Liam Healy 2012-12-02 22:58:35EST linear-algebra-tests.lisp
;; Time-stamp: <2012-12-02 23:49:03EST linear-algebra-tests.lisp>

;; Copyright 2012 Liam M. Healy
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

(in-package :grid)

(lisp-unit:define-test linear-algebra
  (lisp-unit:assert-numerical-equal
   '(3.00d0 3.00d0 3.00d0 3.00d0)
   (contents
    (antik:+ (test-grid-double-float 'foreign-array '(4))
	     (gsl:vector-reverse (test-grid-double-float 'foreign-array '(4))))))
  )
