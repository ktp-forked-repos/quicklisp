;; Definitions of systems of units
;; Liam Healy Tue Mar 19 2002 - 18:07
;; Time-stamp: <2011-06-05 23:37:14EDT sysunit-definitions.lisp>

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

(export '(si *siu* *englishu* *cgsu*))

;;; The file unit-definitions must be compiled and loaded before this can
;;; be compiled.

(define-sysunits *siu*
    (meter second kilogram kelvin
	   ampere mole candela radian
	   newton watt joule pascal coulomb volt
	   farad ohm siemens tesla weber henry)
  nil
  "Systeme internationale system of units.")

(define-sysunits *cgsu*
    (centimeter radian gram second dyne watt erg dynes-per-square-centimeter)
  nil
  "The CGS system of units.")

(define-sysunits *englishu*
    (foot radian slug second pound-force
	  horsepower foot-pound pounds-per-square-inch)
  nil
  "The English system of units.")

(setf *system-of-units* *siu*)		; Default default system of units is SI
(setsys si *siu*)			; so 'si will set si as default system
