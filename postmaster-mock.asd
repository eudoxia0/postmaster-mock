(in-package :cl-user)
(defpackage postmaster-mock-asd
  (:use :cl :asdf))
(in-package :postmaster-mock-asd)

(defsystem postmaster-mock
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:postmaster
               :trivial-types
               :anaphora)
  :components ((:module "src"
                :components
                ((:file "postmaster-mock"))))
  :description "Set up a fake SMTP/IMAP server for testing Postmaster-based applications."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op postmaster-mock-test))))
