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
			  #:ignore
			  #:predicate
			  #:unbound-var-p
			  #:unify! 
			  #:variable-p
			  #:prolog-compiler-macro
			  #:clear-db
			  #:*db-predicates*
			  #:make-predicate
			  #:relation-arity
			  #:get-clauses)
  (:export #:|:-| #:?-
	   #:clear-rules
	   #:clear-status
	   #:setup-status
	   #:setup-init-status))

