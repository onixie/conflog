;;;; package.lisp

(defpackage #:conflog
  (:use #:cl
	#:paiprolog
	#:alexandria
	#:anaphora)
  (:shadowing-import-from #:paiprolog
			  #:deref
			  #:unify! 
			  #:call/1 
			  #:unbound-var-p
			  #:*trail*
			  #:add-clause
			  #:def-prolog-compiler-macro
			  #:compile-body
			  #:args
			  #:predicate))

