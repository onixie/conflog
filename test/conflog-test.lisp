;;;; conflog-test.lisp

(in-package #:conflog-test)

;;; "conflog-test" goes here. Hacks and glory await!

(clear-rules)

(:- (A ?what) (B ?what))
(:- (B ?what) (C ?what))
(:- (C ?what) (D on) (= ?what off))
(:- (C ?what) (D off) (= ?what on))
(:- (D ?what) (status D ?what))
