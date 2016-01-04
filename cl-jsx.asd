;; file: cl-jsx.asd

(in-package :cl-user)

(defpackage cl-jsx-asd
  (:use :cl :asdf))

(in-package :cl-jsx-asd)

(defsystem cl-jsx
  :serial t
  :license "MIT"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "JSX Compiler for Common Lisp"
  :version "0.0.1"
  :depends-on (:cl-who
               :esrap
               :named-readtables)
  :components ((:file "packages")
               (:file "parser")
               (:file "who")
               (:file "cl-jsx")))

;; EOF
