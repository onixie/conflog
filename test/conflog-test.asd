;;;; conflog-test.asd

(asdf:defsystem #:conflog-test
  :serial t
  :description "Conflog Test"
  :author "Nixie onixie@gmail.com"
  :license "LGPL"
  :depends-on (#:conflog #:rt)
  :components ((:file "package")
               (:file "conflog-test")))

