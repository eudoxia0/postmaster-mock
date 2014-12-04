(in-package :cl-user)
(defpackage postmaster-mock-test-asd
  (:use :cl :asdf))
(in-package :postmaster-mock-test-asd)

(defsystem postmaster-mock-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:postmaster-mock
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "postmaster-mock")))))
