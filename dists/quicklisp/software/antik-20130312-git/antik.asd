;; Antik system definition
;; Liam Healy 2010-12-24 09:43:28EST antik.asd
;; Time-stamp: <2013-01-21 18:24:57EST antik.asd>

;; Copyright 2010, 2011, 2012, 2013 Liam M. Healy
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

;;; Antik is a system for doing scientific, engineering, and computational mathematics.  
;;; The following systems are required:
;;;   cffi, alexandria, split-sequence, cl-ppcre, trivial-garbage, drakma
;;; The following are optional:
;;;   static-vectors, lisp-unit, gsll, cl-unicode

#+(or allegro ccl ecl lispworks sbcl)
(when (asdf:find-system :static-vectors nil)
  (pushnew :static-vectors *features*))

(asdf:defsystem #:antik
  :name "Antik"
  :description "A library providing a foundation for computational mathematics, science, and engineering."
  :author "Liam M. Healy"
  :license "GPL v3"
  :serial t
  :defsystem-depends-on (#:asdf-system-connections)
  :depends-on (#:iterate #:alexandria #:split-sequence #:cl-ppcre
		#:cffi #:trivial-garbage
		#+static-vectors static-vectors
		#:drakma)
  :components
  ((:module init
    :serial t
    :components
    ((:file "package")
     (:file "utility")
     (:file "conditions")
     (:file "object")
     (:file "iterate")
     (:file "intermediate")		; temporary
     (:file "generic")))
   (:module grid
    :depends-on (init)
    :components
    ((:file "util")
     (:module affi			; Affine indexing
      :components
      ((:file "package")
       (:file "utility" :depends-on ("package"))
       (:file "affi" :depends-on ("utility"))
       (:file "transformers" :depends-on ("affi"))))
     (:file "functions" :depends-on ("util" affi))
     (:file "specification" :depends-on ("util" "functions" affi))
     (:file "array" :depends-on ("functions" affi))
     (:file "map" :depends-on (affi "functions"))
     (:file "compose" :depends-on ("functions" affi))
     (:file "slice" :depends-on (affi))
     (:file "norm-vector-product" :depends-on (affi "functions"))
     (:file "copy")
     (:file "iterate" :depends-on ("compose"))
     (:file "mathematics" :depends-on ("map"))
     (:module tests
      :components
      ((:file "grids")))))
   (:module input-output
    :serial t
    :components
    ((:file "parameters")
     (:file "format-output")
     (:file "org-mode")
     (:file "float")
     (:file "matlab")
     (:file "read")			; cl-ppcre
     (:file "indexed")))
   (:module foreign-array
    :depends-on (grid input-output)
    :components
    ((:file "types")
     (:file "complex-types" :depends-on ("types"))
     (:file "element-types" :depends-on ("types" "complex-types"))
     (:file "symbol-type")
     (:file "number-conversion"
      :depends-on ("complex-types" "symbol-type"))
     (:file "subclass" :depends-on ("element-types"))
     (:file "foreign-array" :depends-on ("types" "element-types"))
     (:file "vector-matrix" :depends-on ("element-types" "subclass"))
     (:file "methods" :depends-on ("foreign-array" "vector-matrix"))))
   (:module physical-quantities
    :depends-on (init input-output)
    :components
    ((:file "units")
     (:file "scalar" :depends-on ("units"))
     (:file "grid" :depends-on ("scalar"))
     (:file "funcall")
     (:file "unit-definitions" :depends-on ("units" "grid" "funcall"))
     (:file "sysunit-definitions"
      :depends-on ("units" "unit-definitions"))
     (:file "physical-quantities" :depends-on ("units" "scalar"))
     (:file "undimension" :depends-on ("physical-quantities"))
     (:file "arithmetic" :depends-on ("units" "undimension"))
     (:file "iso8601")
     (:file "dtspec" :depends-on ("iso8601"))
     (:file "time-interval"
      :depends-on
      ("sysunit-definitions" "physical-quantities" "iso8601" "dtspec"))
     (:file "angle"
      :depends-on
	    ;; Dependency to force loading of everything needed
	    ;; to put #_1_unit into source code.
      ("physical-quantities"		; needs split-sequence
       "arithmetic"
       "undimension"
       "unit-definitions"
       "sysunit-definitions" "dtspec" "time-interval"))
     (:file "angle-component"
      :depends-on
      ("physical-quantities"
       "undimension"
       "unit-definitions"
       "sysunit-definitions"))
     (:file "state" :depends-on ("units"))
     (:file "timepoint"
      :depends-on
      ("unit-definitions"
       "sysunit-definitions"
       "physical-quantities" "arithmetic"
       "dtspec" "iso8601" "time-interval"))
     (:file "relative-time" :depends-on ("timepoint"))
     (:file "read-time" :depends-on ("timepoint"))
     (:file "formats" :depends-on ("read-time"))
     (:file "convert-timescale"
	    ;; Dependency to force loading of everything needed ;
	    ;; to put #_1_unit into source code. ;
      :depends-on
      ("physical-quantities"		; needs split-sequence ;
       "arithmetic" "undimension" "unit-definitions"
       "sysunit-definitions" "time-interval" "timepoint" "read-time"))
     (:file "dtmath"
      :depends-on ("dtspec" "read-time" "timepoint" "convert-timescale"))))
   (:module mathematics
    :serial t
    :components
    ((:file "trigonometry")
     (:file "integers")))
   (:module cartesian
    :serial t
    :components
    ((:file "cartesian")
     (:file "polar")
     (:file "rotation")))
   (:module tests
    :serial t
    :components
    ((:file "format-grid")))))

(asdf:defsystem-connection #:degree-symbol
  :serial t
  :requires (#:antik #:cl-unicode)
  :components
  ((:module physical-quantities
	    :components
	    ((:file "degree-symbol")))))

(asdf:defsystem-connection #:antik-tests
    :serial t
  :requires (#:antik #:lisp-unit)
  :components
  ((:module tests
	    :components
	    ((:file "numbers")
	     (:file "physical-quantities")))
   (:module grid
	    :components
	    ((:module tests
		      :components
		      ((:file "augment")
		       (:file "compose")
		       (:file "map")
		       (:file "slice")
		       (:file "norm-vector-product")))))
   (:module foreign-array
	    :components
	    ((:module tests
		      :components
		      ((:file "aref")
		       (:file "lisp-unit-extension")
		       (:file "compose")))))))

;;; High level mathematics built on libraries.
(asdf:defsystem-connection #:math-high
  :serial t
  :requires (#:antik #:gsll)
  :components
  ((:module optimize
    :components
    ((:file "one-dim")))
   (:module linear-algebra
    :components
    ((:file "linear-algebra")))
   (:module sample
    :components
    ((:file "low-discrepancy-sequence")))))

(asdf:defsystem-connection #:math-high-tests
  :serial t
  :requires (#:math-high #:lisp-unit)
  :components
  ((:module linear-algebra
    :components
    ((:file "linear-algebra-tests")))))
