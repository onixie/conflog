;;;; package.lisp

(defpackage #:conflog
  (:use #:cl
	#:paiprolog
	#:alexandria)
  (:shadowing-import-from #:paiprolog
			  #:*trail*
			  #:add-clause
			  #:args
			  #:call/1 
			  #:compile-body
			  #:def-prolog-compiler-macro
			  #:deref
			  #:predicate
			  #:unbound-var-p
			  #:unify! 
			  #:variable-p))

