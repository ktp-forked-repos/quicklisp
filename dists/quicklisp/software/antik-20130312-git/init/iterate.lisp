;; Extend the iterate macro
;; Liam Healy 2008-12-22 16:02:33EST iterate.lisp
;; Time-stamp: <2011-10-18 19:34:31EDT iterate.lisp>

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

(in-package :iterate)

(defclause-driver (for var-spec &sequence)
  "Numbers"
  (top-level-check)
  (if with-index
      (clause-error "WITH-INDEX should not be specified for this clause"))
  (check-sequence-keywords from upfrom downfrom to downto above below nil)
  (make-default-binding var-spec :type 'number)
  (let* ((var (extract-var var-spec))
	 (initial (or from upfrom downfrom 0))
	 (limit (or to downto above below))
	 (step-func (if (or downfrom downto above)
			'antik:-
			'antik:+))
	 (test-func (cond
		     (to 'antik:>)
		     (downto 'antik:<)
		     (below 'antik:>=)
		     (above 'antik:<=)))
	 (limit-var (if (and limit (not (constant? limit)))
			(make-var-and-default-binding 
			 'limit
			 :using-type-of (if (expression-type limit)
					    limit
					    var))))
	 (step-var (if (not (constantp by)) 
		       (make-var-and-default-binding 'step
						     :using-type-of by)))
	 (step-thing (or step-var by))
	 (limit-code (or limit-var limit))
	 (init-val (eval-const-expr
		    (list (if (eq step-func 'antik:+) 'antik:- 'antik:+) initial step-thing)))
	 (test (if limit
		   (progn (setq *loop-end-used?* t)
			  `((if (,test-func ,var ,limit-code) 
				(go ,*loop-end*))))
		   nil))
	 (next `((setq ,var (,step-func ,var ,step-thing)) .,test)))
    (return-driver-code :initial `(,.(if limit-var
					 `((setq ,limit-var ,limit)))
				   ,.(if step-var
					 `((setq ,step-var ,by)))
				   (setq ,var ,init-val))
			:next next
			:variable var)))

;;; Shamelessly cribbed from the iterate manual

(defmacro-clause (MULTIPLY expr &optional INTO var)
    `(reducing ,expr by #'antik::*i into ,var initial-value 1))

(defmacro-clause (SUM expr &optional INTO var)
    `(reducing ,expr by #'antik::+i into ,var initial-value 0))

(defclause (maximize expr &optional into var)
  "Maximize value of an expression"
  (return-extremum-code expr var 'antik:max))

#|
;;; This does not work; can't use 'leave
(defmacro-clause (FINDING expr MAXIMIZING func &optional INTO var)
  (alexandria:with-gensyms (max-val temp1 temp2)
    (let ((winner (or var *result-var*)))
      `(progn
         (with ,max-val = nil)
         (with ,winner = nil)
         (cond
	   ((null ,max-val)
	    (setq ,winner ,expr)
	    (setq ,max-val (funcall ,func ,winner))
	    (t
	     (let* ((,temp1 ,expr)
		    (,temp2 (funcall ,func ,temp1)))
	       (when (antik:> ,temp2 ,max-val)
		 (setq ,max-val ,temp2)
		 (setq ,winner ,temp1))))))
         (finally (leave ,winner))))))
|#

(defclause (minimize expr &optional into var)
  "Minimize value of an expression"
  (return-extremum-code expr var 'antik:min))

#|
;;; This does not work
(defmacro-clause (FINDING expr MINIMIZING func &optional INTO var)
  (alexandria:with-gensyms (max-val temp1 temp2)
    (let ((winner (or var *result-var*)))
      `(progn
         (with ,max-val = nil)
         (with ,winner = nil)
         (cond
	   ((null ,max-val)
	    (setq ,winner ,expr)
	    (setq ,max-val (funcall ,func ,winner))
	    (t
	     (let* ((,temp1 ,expr)
		    (,temp2 (funcall ,func ,temp1)))
	       (when (antik:< ,temp2 ,max-val)
		 (setq ,max-val ,temp2)
		 (setq ,winner ,temp1))))))
         (finally (leave ,winner))))))
|#
