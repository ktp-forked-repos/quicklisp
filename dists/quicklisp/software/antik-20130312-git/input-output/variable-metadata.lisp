;; Standard metadata to attach to variables.
;; Liam Healy 2013-02-16 20:51:14HST variable-metadata.lisp
;; Time-stamp: <2013-02-16 22:12:48HST variable-metadata.lisp>

;; Copyright 2013 Liam M. Healy
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


;;; Properties: default type description attribute latex latex-supplement

(defvar *variable-properties*
  '(default type documentation attribute symbol))

;;; symbol will be both plain, latex or latex-supplement if attribute
;;; (subscript 1)
;;; (plain "sma" latex "a")

(defun set-variable-metadata (symbol property value)
  (if (member property *variable-properties*)
      (setf (get symbol property) value)
      (error "Property ~a is not one of ~a." property *variable-properties*)))

;;; For globals
;;; Proclaim the type.
;;; Proclaim the variable special.
;;; Bind the documentation for the variable.

;;; Non-top-level
;;; (proclaim '(special a))
;;; (proclaim '(type velocity a)) ; will be checked at least by SBCL.
