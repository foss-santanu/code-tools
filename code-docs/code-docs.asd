;; ASDF system definition file for CODE-DOCS
;; so that XREF can be loaded with ql:quickload

(defpackage #:code-docs-sys
  (:use :cl :asdf))

(in-package :code-docs-sys)

(defsystem #:code-docs
  :serial t
  :description "Documentation generator for a Lisp codebase"
  :author "Santanu Chakrabarti <santanu.chakrabarti@gmail.com>"
  :version "0.1"
  :license "GPL"
  :depends-on (:xref :user-man)
  :components ((:file "documentation-builder"))
  )