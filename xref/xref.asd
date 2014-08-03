;; ASDF system definition file for XREF
;; so that XREF can be loaded with ql:quickload

(defpackage #:xref-asd
  (:use :cl :asdf))

(in-package :xref-asd)

(defsystem #:xref
  :serial t
  :components ((:file "xref")
               (:file "xref-patterns-for-macl")))
