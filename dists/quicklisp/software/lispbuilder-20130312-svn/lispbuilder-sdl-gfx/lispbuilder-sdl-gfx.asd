;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-gfx-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-gfx-system)

(defsystem lispbuilder-sdl-gfx
  :description "lispbuilder-sdl-gfx: SDL_gfx v2.0.16 library wrapper and tools"
  :long-description
  "lispbuilder-sdl-gfx is a wrapper for the SDL_gfx v2.0.16 library. 
    The SDL_gfx library extends the base functionality implemented by SDL
    and provides Graphic Primitives, Rotozoomimg, Framerate control, and 
    MMX image filters. The wrapper is implemeted using CFFI to be highly 
    compatible across lisp implementations."
  :version "0.8.0"
  :author "Luke Crook <luke@balooga.com>"
  :maintainer "Application Builder <application-builder@lispniks.com>"
  :licence "MIT"
  :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-gfx-cffi)
  :perform (load-op :after (op lispbuilder-sdl-gfx)
		    (pushnew :lispbuilder-sdl-gfx *features*))
  :components
  ((:module "sdl-gfx"
	    :components
	    ((:file "package")
	     (:file "font" :depends-on ("package"))))
   (:module "documentation"
	    :components
	    ((:html-file "lispbuilder-sdl-gfx")
	     (:html-file "header")
	     (:html-file "footer")
	     (:static-file "sdl-gfx-examples_width-height.png")
	     (:doc-file "README")))))

