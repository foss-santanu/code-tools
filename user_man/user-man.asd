;; ASDF system definition file for USER-MAN
;; so that XREF can be loaded with ql:quickload

(defpackage #:user-man-sys
  (:use :cl :asdf))

(in-package :user-man-sys)

(defsystem #:user-man
  :serial t
  :components ((:file "user-manual")))