(in-package :cl-user)
(defpackage postmaster-mock-test
  (:use :cl :postmaster-mock :fiveam))
(in-package :postmaster-mock-test)

;;; Test data

(defparameter *service*
  (make-instance '<mock-service>)
  "The mock service we'll use throughout the tests.")

(defparameter *invalid-account*
  (make-instance '<mock-account>
                 :service *service*
                 :username "john@example.com"
                 :password "pass")
  "This account is invalid, because it won't be added to the list of accounts in
  the service.")

(defparameter *authorized-account*
  (make-instance '<mock-account>
                 :service *service*
                 :username "jane@example.com"
                 :password "pass")
  "This account will be registered with *service*, so it should work.")

(defparameter *unauthorized-account*
  (make-instance '<mock-account>
                 :service *service*
                 :username "jane@example.com"
                 :password "pass2")
  "This account is a copy of *unauthorized-account*, but with a different password,
  so authentication will fail.")



(defparameter *email*
  (make-instance 'postmaster.email:<email>
                 :from "me@example.com"
                 :to "friend@example.com"
                 :subject "Subject"
                 :body "This is test data. Who cares?")
  "Sample email we'll try to send.")

;;; Tests

(def-suite general)
(in-suite general)

(test account-functions
  (finishes
    (register-account *service* *invalid-account*))
  (is-true (has-account *service*
                        (postmaster.services:username *invalid-account*)))
  (is (equal (registered-users *service*) 1))
  (finishes (remove-account *service*
                            (postmaster.services:username *invalid-account*)))
  (is (equal (registered-users *service*) 0))
  (is-false (has-account *service*
                         (postmaster.services:username *invalid-account*)))
  (finishes
    ;; Register these two for future use
    (register-account *service* *authorized-account*)))

(test send-email
  (is (equal (email-count *service*) 0))
  (is-false
   ;; This should fail, because *invalid-account* is not registered in *service*
   (postmaster.smtp:send *invalid-account* *email*))
  (is-false
   ;; This should fail, because *unauthorized-account* is has the wrong password
   (postmaster.smtp:send *unauthorized-account* *email*))
  (finishes
   ;; This will work
   (postmaster.smtp:send *authorized-account* *email*))
  (is (equal (email-count *service*) 1)))

(run! 'general)
