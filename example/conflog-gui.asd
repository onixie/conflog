;;;; conflog-gui.asd

(asdf:defsystem #:conflog-gui
  :serial t
  :description "Conflict Addressing for GUI"
  :author "Nixie <onixie@gmail.com>"
  :license "LGPL"
  :depends-on (#:mcclim #:conflog)
  :components ((:file "conflog-gui")))

