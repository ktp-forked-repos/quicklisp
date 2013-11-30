;;; scratch stuff for matrix messin'

(in-package :clem-test)

(defparameter m1 (make-instance 'clem::double-float-matrix :rows 512 :cols 512))
(time (clem::sum m1))

(defun enable-bracket-reader-macro ()
 (set-macro-character #\] (get-macro-character #\) nil))
 (set-macro-character #\[ #'(lambda (in c)
                              (declare (ignore c))
                              (let ((sym (read in))
                                    (indices (read-delimited-list #\] in)))
                                (apply #'clem::mref (symbol-value sym) indices)))))
