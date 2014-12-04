(in-package :cl-user)
(defpackage postmaster-mock
  (:use :cl :trivial-types :anaphora)
  (:import-from :postmaster.services
                :<account>
                :<service>
                :service
                :username
                :password)
  (:import-from :postmaster.email
                :<email>)
  (:export :<mock-account>
           :<mock-service>
           :accounts
           :email
           :register-account
           :registered-users
           :has-account
           :remove-account
           :email-count))
(in-package :postmaster-mock)

(defclass <mock-account> (<account>)
  ()
  (:documentation "Represents an account on the server side. When sending an
  email, username/password details are actually verified."))

(defclass <mock-service> (<service>)
  ((accounts :accessor accounts :type hash-table
             :initform (make-hash-table :test #'equal))
   (emails :accessor emails :initarg :email :type (proper-list <email>)
           :initform (list)))
  (:documentation "A service that stores all incoming mail in a stack."))

(defmethod register-account ((service <mock-service>) (account <mock-account>))
  "Add a mock account to a mock service."
  (setf (gethash (username account) (accounts service)) account))

(defmethod registered-users ((service <mock-service>))
  "Return the number of accounts registered to the service."
  (hash-table-count (accounts service)))

(defmethod has-account ((service <mock-service>) username)
  "Check whether there's an account in the service with that username."
  (gethash username (accounts service)))

(defmethod remove-account ((service <mock-service>) username)
  "Remove the account with that username from the service."
  (remhash username (accounts service)))

(defmethod email-count ((service <mock-service>))
  "Return the number of emails received by the service."
  (length (emails service)))

(defmethod postmaster.smtp:send ((account <mock-account>)
                                 (email postmaster.email:<email>))
  (aif (has-account (service account) (username account))
       ;; This account is registered to the service. Do the passwords match?
       (if (equal (password account) (password it))
           (push email (emails (postmaster.services:service account))))))
