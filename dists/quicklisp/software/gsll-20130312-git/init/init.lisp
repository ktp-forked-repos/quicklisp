;; Load GSL
;; Liam Healy Sat Mar  4 2006 - 18:53
;; Time-stamp: <2012-02-19 10:35:34EST init.lisp>
;;
;; Copyright 2006, 2007, 2008, 2009, 2010, 2011 Liam M. Healy
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

(defpackage gsll
    (:nicknames :gsl)
  (:use :common-lisp :cffi)
  (:import-from :grid #:dim0 #:dim1 #:^ #:copy)
  (:export #:dim0 #:dim1 #:copy)
  ;; No actual conflict due to different usage of symbols:
  ;; antik:psi means "pounds per square inch" vs. function #'gsl:psi
  ;; antik:knots means "nautical miles per hour" vs. function #'gsl:knots
  ;; antik:acceleration refers to the time derivative of velocity vs. object 'gsl:acceleration.
  ;; si units symbol-macro vs. GSLL's sine integral.
  (:shadowing-import-from :antik #:psi #:knots #:si))

(setf
 antik::*antik-user-shadow-symbols*
 (append antik::*antik-user-shadow-symbols*
	 ;; Where there is a symbol conflict between GSLL and other packages,
	 '(
	   ;; take from the other package
	   grid:row			; GSLL alternate is equivalent
	   grid:column			; GSLL alternate is equivalent
	   iterate:sum ; GSLL histogram function, both pretty obscure
	   iterate:multiply		; GSLL function duplicates '*
	   antik:polar-to-rectangular	; GSLL's doesn't use vectors
	   antik:rectangular-to-polar	; GSLL's doesn't use vectors
	   antik:acceleration
	   ;; taken from GSLL
	   gsll::iterate ; conflict with iterate:iterate, but iterate:iter is a synonym
	   ))
 antik::*antik-user-use-packages*
 (cons '#:gsll antik::*antik-user-use-packages*))

(antik:make-user-package :antik-user)	; Add the new use package and shadow symbols to :antik-user

(in-package :gsl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gsl-config (arg)
    "A wrapper for tool `gsl-config'."
    (with-input-from-string
        (s (with-output-to-string (asdf::*verbose-out*)
             (asdf:run-shell-command "gsl-config ~s" arg)))
      (read-line s)
      (read-line s)))
  #+unix
  (defun gsl-config-pathname (pn)
    (merge-pathnames pn (pathname (format nil "~a/" (gsl-config "--prefix"))))))

(cffi:define-foreign-library libgslcblas
  (:darwin #+ccl #.(ccl:native-translated-namestring
		    (gsl-config-pathname "libgslcblas.dylib"))
           #-ccl #.(gsl-config-pathname "libgslcblas.dylib"))
  (:cygwin "cyggslcblas-0.dll")
  (:unix (:or "libgslcblas.so.0" "libgslcblas.so"))
  (t (:default "libgslcblas")))
   
(cffi:use-foreign-library libgslcblas)

;; When calling libgsl from emacs built for windows and slime, and
;; using clisp built for cygwin, we have to load lapack/cygblas.dll
;; before loading cyggsl-0.dll
#+(and clisp cygwin)
(cffi:load-foreign-library "/lib/lapack/cygblas.dll")

(cffi:define-foreign-library libgsl
  (:darwin #+ccl #.(ccl:native-translated-namestring
                     (gsl-config-pathname "libgsl.dylib"))
           #-ccl #.(gsl-config-pathname "libgsl.dylib"))
  (:cygwin "cyggsl-0.dll")
  (:unix (:or "libgsl.so.0" "libgsl.so"))
  (t (:default "libgsl")))
   
(cffi:use-foreign-library libgsl)
