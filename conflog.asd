;;;; conflog.asd

(asdf:defsystem #:conflog
  :serial t
  :description "Conflict Addressing"
  :author "Nixie <onixie@gmail.com>"
  :license "LGPL"
  :depends-on (#:paiprolog #:alexandria #:anaphora)
  :components ((:file "package")
               (:file "conflog")))

