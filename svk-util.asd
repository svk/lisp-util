(defpackage #:svk-util-system)
(in-package :svk-util-system)

(asdf:defsystem svk-util
  :description "A haven for various utility functions and macros used across svk's Lisp projects"
  :author "Steinar V. Kaldager <steinarvk@gmail.com>"
  :depends-on ("cl-utilities")
  :licence "Simplified BSD (see COPYING)"
  :serial t
  :version "0.1.0"
  :components ((:file "svk-util")))
