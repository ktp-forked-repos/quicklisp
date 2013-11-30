
(require 'asdf)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (asdf:operate 'asdf:load-op 'clem)
 (asdf:operate 'asdf:load-op 'clem-doc)
 (asdf:operate 'asdf:load-op 'tinaa))

(cl:defpackage :clem-tinaa
  (:use :cl))

(cl:in-package :clem-tinaa)

(defun clem-doc-system::make-tinaa-docs ()
  (asdf:operate 'asdf:load-op 'tinaa)
  (let ((tinaa::*short-documentation-length* 512))
    (tinaa:document-system
     'package 'clem (asdf:component-pathname
                     (asdf:find-component
                      (asdf:find-component
                       (asdf:find-system 'clem-doc)
                       "doc")
                      "tinaa")))))
  
(clem-doc-system::make-tinaa-docs)
