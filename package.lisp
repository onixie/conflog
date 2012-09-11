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
			  #:get-clauses
			  #:clear-predicate
			  #:run-prolog
			  #:deref-exp
			  #:replace-?-vars
			  #:variables-in
			  #:top-level-query/0
			  #:set-binding!
			  #:var-p)
  (:export #:|:-| #:?-
	   #:clear-rules
	   #:clear-status
	   #:setup-status
	   #:setup-init-status

	   #:with-rules
	   #:with-status

	   #:get-status
	   #:refresh-status
	   #:refresh-status/resolve
	   #:set-init-status
	   #:set-status
	   #:set-status/resolve
	   #:resolve
	   #:regist))

