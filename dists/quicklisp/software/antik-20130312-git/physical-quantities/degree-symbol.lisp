;; Load the degree symbol from cl-unicode
;; Liam Healy 2011-01-30 18:33:55EST degree-symbol.lisp
;; Time-stamp: <2011-02-11 22:33:11EST degree-symbol.lisp>

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *degree-symbol*
	(princ-to-string (cl-unicode:character-named "degree sign"))))

(define-units 'angle
    `((degree (/ ,pi 180)
       (,(antik-symbol *degree-symbol*) deg degrees) (,*degree-symbol* "\\dgr"))))
