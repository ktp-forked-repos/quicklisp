;; Sample sets of values from a low-discrepancy (quasi-random) sequence
;; Liam Healy 2011-06-06 09:42:22EDT low-discrepancy-sequence.lisp
;; Time-stamp: <2011-07-29 14:35:19EDT low-discrepancy-sequence.lisp>

(in-package :antik)

(export '(low-discrepancy-sample apply-to-arguments))

(defun val-from-range (val min max)
  (+ min (* val (- max min))))

(defun quasi-random-values
    (rank count &optional (rng-type gsl:+sobol+) (grid-type grid:*default-grid-type*))
  "Generate a set of count values evenly distributed across the range [0,1] for each dimension."
  (let ((gen (gsl:make-quasi-random-number-generator rng-type rank))
	(vec (grid:make-foreign-array 'double-float :dimensions rank)))
    (loop repeat count
	  do (gsl:qrng-get gen vec)
	  collect (grid:copy-to vec grid-type))))

(defun low-discrepancy-sample (count make-function &rest parameters)
  "Call the make function to generate a set of objects with the
   parameters sampled according to a low-discrepancy sequence.  Each
   parameter will be either a fixed value, specified as a list of key
   and value, or a range from which the sample is taken, specified as
   a list of key, lower value, and upper value.  For example,
@example
   (low-discrepancy-sample
      5
      'make-orbit
      '(:semimajor-axis #_7000_km #_9000_km) 
      '(:eccentricity 0.0)
      '(:inclination #_95_deg #_105_deg))
@end example"
  (let* ((fixed-parameters
	  (apply 'append
		 (remove-if-not (lambda (x) (= (length x) 2)) parameters)))
	 (sweep-parameters
	  (remove-if-not (lambda (x) (= (length x) 3)) parameters))
	 (vals (quasi-random-values (length sweep-parameters) count)))
    (iter:iter
      (iter:for v iter:in vals)
      (iter:collect
	  (apply
	   make-function
	   (append
	    fixed-parameters
	    (mapcan
	     (lambda (x seq)
	       (list (first x)
		     (val-from-range
		      (grid:gref v seq) (second x) (third x))))
	     sweep-parameters
	     (loop for i from 0 below (length sweep-parameters) collect i))))))))

(defun apply-to-arguments (function argument-order argument-list)
  "Apply the function to the arguments given, where the argument list
   is a list of (argument-name argument-value)."
  (apply function
	 (loop for arg in argument-order
	       collect (getf argument-list arg))))
