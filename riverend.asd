;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :riverend
  :name "riverend"
  :version "0.0.1"
  :author "Tom Dailey"
  :licence "MIT"
  :description "Riverend rendering and animation system"
  :long-description "A light (but slow) animation framework, which originally grew out of Paul Graham's Ray tracing program in his book /ANSI Common Lisp/."
  :serial t
  :components ((:file "src/package")
	       (:file "src/utilities")
	       (:file "src/tracer")
	       (:file "src/ball")
	       (:file "src/tri")
	       (:file "src/env")
	       (:file "import"))
  :depends-on ("zpng"
	       "3d-vectors"
	       "3d-matrices"))

;; Organizational notes:
;; Tracer contains the ray-tracing code, and definitions
;; for the 3d types that it handles. Methods for these are in
;; the file for the relavant object.
