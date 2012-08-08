(in-package :conflog)
;;; "conflog" goes here. Hacks and glory await!

;; Primitives

(defmacro define-primitive (name (&rest args) &body body)
  (let ((pname (intern (symbol-name name) :paiprolog)))
    `(progn
       (defun ,pname (,@args) ,@body)
       (shadowing-import ',pname))))

(define-primitive apply/1 (?g cont)
  (call/1 ?g cont))

(let ((init-status (make-hash-table))
      (status (make-hash-table))
      (prev-status (make-hash-table))
      (in-cycle (make-hash-table))
      (N/A (gensym)))

  (defun setup-status (alist &key (for :current))
    (case for
      (:default (setf init-status (alist-hash-table alist)))
      (:previous (setf prev-status (alist-hash-table alist)))
      (t (setf status (alist-hash-table alist)))))
  
  (defun clear-status ()
    (setf status (make-hash-table)))

  (defun clear-prev-status ()
    (setf prev-status (make-hash-table)))
  
  (labels  ((found? (pred status)
	      (awhen (gethash pred status N/A)
		(and (not (eq it N/A)) it)))
	    (in-cycle (goal)
	      (setf (gethash goal in-cycle) goal))
	    (in-cycle? (goal)
	      (prog1 (found? goal in-cycle)
		(setf (gethash goal in-cycle) N/A))))

    (define-primitive init-status/2 (pred var cont)
      (awhen (and init-status
		  (not (unbound-var-p pred))
		  (found? (deref pred) init-status))
	(when (unify! var it)
	  (funcall cont))))

    (define-primitive status/2 (pred var cont)
      (when (not (unbound-var-p pred))
	(aif (found? (deref pred) status)
	     (when (unify! var it)
	       (funcall cont))
	     (apply/1 `(and (,pred ?X) (= ,var ?X)) cont))))
    
    (define-primitive prev-status/2 (pred var cont)
      (awhen (and (not (unbound-var-p pred))
		  (found? (deref pred) prev-status))
	(when (unify! var it)
	  (funcall cont))))

    (define-primitive sstatus/2 (pred val cont)
      (when (and (not (unbound-var-p pred))
		 (not (unbound-var-p val)))
	(awhen (found? (deref pred) status)
	  (setf (gethash (deref pred) prev-status) it))
	(setf (gethash (deref pred) status) val)
	(funcall cont)))

    (define-primitive dump-status/0 (cont)
      (declare (ignorable cont))
      (dolist (key (hash-table-keys status))
	(format t "~&~A:" key)
	(awhen (found? key status)
	  (format t " ~A" it))
	(awhen (found? key prev-status)
	  (format t ", ~A" it)))
      (finish-output))

    (defmacro :- (head &body body)
      `(progn
	 (let ((*package* (find-package :paiprolog)))
	   (<- ,head ,@body)
	   (def-prolog-compiler-macro ,(first head) 
	       (goal body cont bindings)
	     (if (funcall ,#'in-cycle? (predicate goal))
		 :pass
		 (progn
		   (funcall ,#'in-cycle (predicate goal))
		   `(funcall ,#'status/2 ',(predicate goal) ,@(args goal)
			     (lambda ()
			       ,(compile-body body cont bindings)))))))
	 ',(predicate head)))))

