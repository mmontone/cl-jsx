(in-package :cl-user)

(defpackage cl-jsx-test-asd
  (:use :cl :asdf))

(in-package :cl-jsx-test-asd)

(defsystem cl-jsx-test
  :serial t
  :version "0.0.1"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "Test suite for CL-JSX"
  :license "MIT"
  :depends-on (:cl-jsx
               :prove)
  :components ((:module "t"
                        :serial t
                        :components ((:file "cl-jsx"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
