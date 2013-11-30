;; Minimize or maximize the function in one variable.
;; Liam Healy 2011-01-14 12:18:15EST minimize.lisp
;; Time-stamp: <2011-04-02 16:57:34EDT one-dim.lisp>

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

;;; This requires GSLL

(in-package :antik)

(export '(root-1d minimize-1d maximize-1d))

(defun root-1d (function initial derivative
		&optional
		(absolute-error 0.0001d0)
		(relative-error 0.0001d0)
		(method gsl:+newton-fdfsolver+))
  "Find the root of a real function of a real variable.  Returns the
   argument value of the root and the computed value of the function at
   that value.  The argument and return values may be physical quantities."
  (flet ((root-1d-nopq			; If argument is a number
	     (function initial derivative absolute-error relative-error method)
	   (let ((solver (gsl:make-one-dimensional-root-solver-fdf
			  method
			  function derivative
			  (lambda (x) (values (funcall function x) (funcall derivative x)))
			  initial)))
	     (iter (for oldroot first initial then root)
		   (gsl:iterate solver)
		   (for root = (gsl:solution solver))
		   (until (and
			   oldroot
			   (gsl:root-test-delta root oldroot absolute-error relative-error))))
	     (values (gsl:solution solver) (funcall function (gsl:solution solver))))))
    (if (physical-quantity-p initial)
	(multiple-value-bind (initial-num initial-units)
	    (pqval initial)
	  (multiple-value-bind (x f)
	      (root-1d-nopq
	       (lambda (x) (pqval (funcall function (make-pq x initial-units))))
	       initial-num
	       (lambda (x) (pqval (funcall derivative (make-pq x initial-units))))
	       absolute-error
	       relative-error
	       method)
	    (values (make-pq x initial-units) f)))
	(root-1d-nopq function initial derivative absolute-error relative-error method))))

(defun minimize-1d
    (function x-lower x-upper
     &optional
     (x-minimum (cl:/ (cl:+ x-lower x-upper) 2))
     (absolute-error 0.0001d0)
     (relative-error 0.0001d0)
     (method gsl:+brent-fminimizer+))
  "Find the minimum of the function of one variable (x) between x-lower
   and x-upper. The optional argument x-minimum is a guess of x for the
   minimum value of the function.  Returns the argument and value at the minimum."
  (let ((minimizer (gsl:make-one-dimensional-minimizer method function x-minimum x-lower x-upper)))
    (iter (until (gsl:min-test-interval
		  (gsl:fminimizer-x-lower minimizer)
		  (gsl:fminimizer-x-upper minimizer)
		  absolute-error
		  relative-error))
	  (gsl:iterate minimizer))
    (values (gsl:solution minimizer) (gsl:function-value minimizer))))

(defun maximize-1d
    (function x-lower x-upper
     &optional
     (x-maximum (cl:/ (cl:+ x-lower x-upper) 2))
     (absolute-error 0.0001d0)
     (relative-error 0.0001d0)
     (method gsl:+brent-fminimizer+))
  "Find the maximum of the function of one variable (x) between x-lower
   and x-upper. The optional argument x-maximum is a guess of x for the
   maximum value of the function.  Returns the argument and value at the maximum."
  (multiple-value-bind (x min-val)
      (minimize-1d
       (alexandria:compose 'cl:- function)
       x-lower
       x-upper
       x-maximum
       absolute-error
       relative-error
       method)
    (values x (cl:- min-val))))
