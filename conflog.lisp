;;; "conflog" goes here. Hacks and glory await!
(in-package :conflog)

;;; Utils
(defmacro define-alias (alias name &key (type :macro))
  (ecase type
    (:function `(setf (symbol-function ',alias) (symbol-function ',name)))
    (:macro `(setf (macro-function ',alias) (macro-function ',name)))))

(eval-when (:compile-toplevel)
  (defconstant +N/A+ (gensym)))

(defmacro found? (val)
  `(not (eq ,val +N/A+)))

;;; Primitives
(defmacro define-primitive (name (&rest args) &body body)
  (let ((pname (intern (symbol-name name) :paiprolog)))
    `(progn
       (defun ,pname (,@args) 
	 #+conflog-debug
	 (format t "~&(~A ~{~A~^ ~})~%" ',pname (butlast (list ,@args)))
	 ,@body)
       (eval-when (:compile-toplevel)
	 (shadowing-import ',pname)))))

(defmacro define-status/primitive (name)
  (let ((v (gensym)))
    `(progn
       (defvar ,v (make-hash-table))
       (defmacro ,name (var)
	 `(gethash ,var ,',v +N/A+))
       (defun ,(symbolicate 'setup- name) (alist)
	 (setf ,v (alist-hash-table alist)))
       (defun ,(symbolicate 'clear- name) ()
	 (setf ,v (make-hash-table)))
       (defun ,(symbolicate name '-variables) ()
	 (hash-table-keys ,v))
       (defun ,(symbolicate name '-alist) ()
	 (hash-table-alist ,v))
       
       (define-primitive ,(symbolicate name '/2) (pred var cont)
	 (when (not (unbound-var-p pred))
	   (let ((s (,name (deref pred))))
	     (when (and (found? s) (unify! var s))
	       (funcall cont))))))))

(define-status/primitive status)
(define-status/primitive init-status)
(define-status/primitive prev-status)

(defun set-status (pred val cont &optional (schedule-propogate nil))
  (when (and (not (unbound-var-p pred))
	     (not (unbound-var-p val)))
    (let* ((pred (deref pred))
	   (val (deref val))
	   (pval (status pred)))
      (when (not (eq pval val))
	(setf (status pred) val)
	(when (found? pval)
	  (setf (prev-status pred) pval)
	  (when schedule-propogate
	    (funcall schedule-propogate pred))))
      (funcall cont))))

(define-primitive sstatus/2 (pred val cont)
  (set-status pred val cont))

(define-primitive propogate-sstatus/2 (pred val cont)
  (set-status pred val cont #'schedule-propogate))

(define-primitive dump-status/0 (cont)
  (dolist (key (status-variables))
    (format t "~&~A:" key)
    (let ((s (status key)))
      (when (found? (status key))
	(format t " current ~A" s)))
    (let ((ps (prev-status key)))
      (when (found? ps)
	(format t ", previous ~A" ps)))
    (let ((ds (init-status key)))
      (when (found? ds)
	(format t ", default ~A" ds))))
  (finish-output)
  (funcall cont))

(macrolet ((define-apply-utils ()
	     (let ((in (gensym "in-apply")))
	       `(progn
		  (defvar ,in (make-hash-table))
		  (defmacro with-apply ((g &optional (kind :in)) &body body)
		    (let ((old (gensym)))
		      `(let ((,old (gethash ,g ,',in nil)))
			 (unwind-protect (progn (setf (gethash ,g ,',in)
						      ,(case kind (:out nil) (t t)))
						,@body)
			   (setf (gethash ,g ,',in) ,old)))))
		  (defun in-apply? (g) (gethash g ,in nil))))))
  (define-apply-utils))

(define-primitive apply/1 (goal cont)
  (let ((pred (predicate goal))
	(var (first (args goal))))
    (with-apply (pred :in)
      (call/1 goal (lambda ()
		     (with-apply (pred :out)
		       (propogate-sstatus/2 pred var cont)))))))

(define-primitive lookup/2 (pred var cont)
  (when (not (unbound-var-p pred))
    (let ((s (status (deref pred))))
      (unless (found? s)
	(apply/1 `(,(deref pred) ,(?)) #'ignore))
      (status/2 pred var cont))))

(define-primitive member/2 (item list cont)
  (when (some (lambda (v) (unify! item v)) list)
    (funcall cont)))

;;; Conflict Rule
(defvar *total-rules* 0)
(defvar *relation* (make-hash-table))
(defmacro define-rule (head &body body)
  (let ((rule-str (format nil "[~d] (:- ~A ~{~A~^ ~})" (incf *total-rules*) head body)))
    (declare (ignorable rule-str))
    `(progn
       #+conflog-debug
       (<- ,head (lisp (format t "~&~A~%" ,rule-str)) ,@body)
       #-conflog-debug
       (<- ,head ,@body)

       (mapc (lambda (rhs)
	       (let ((old (gethash rhs *relation* nil)))
		 (unless (member ',(car head) old)
		   (setf (gethash rhs *relation*)
			 (append old (list ',(predicate head)))))))
	     ',(mapcar #'predicate body))

       ,(unless (get (first head) 'prolog-compiler-macro)
	  `(def-prolog-compiler-macro ,(first head)
	     (goal body cont bindings)
	     (if (in-apply? (predicate goal))
		 :pass
	       (compile-body (cons `(lookup ,(predicate goal) ,@(args goal)) body) cont bindings))))

       ',(predicate head))))

(defun clear-rules ()
  (dolist (pred *db-predicates*)
    (mapc (lambda (clause)
	    (fmakunbound (intern (symbol-name (make-predicate pred (relation-arity clause))) :paiprolog))
	    (unintern (make-predicate pred (relation-arity clause)) :paiprolog))
	  (get-clauses pred)))
  (clear-db)
  (setf *db-predicates* nil)
  (setf *total-rules* 0)
  (setf *relation* (make-hash-table)))

(define-alias :- define-rule)

;;; Query Rule
(define-primitive return/2 (var-names vars cont)
  (throw :ret
	 (or (null vars)
	     (mapcar (lambda (var-name var)
		       (cons (intern var-name) (deref-exp var)))
		     var-names vars))))

(defun query (goals)
  (clear-predicate 'top-level-query)
  (let ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (return ,(mapcar #'symbol-name vars) ,vars))))
  (catch :ret
    (run-prolog 'top-level-query/0 #'ignore)))

(defmacro ?- (&rest goals)
  `(prog1
       (query ',(replace-?-vars goals))
     (run-propogate)))

;;; Propogation
(defvar *propogating* nil)
(defvar *in-propogate* nil)

(defun propogate-ask (&optional (preds nil))
  (macrolet ((ask (pred)
	       `(let ((pred ,pred))
		  #+conflog-debug
		  (format t "~&  --> ~A ~%" pred)
		  (query ,(replace-?-vars ``((apply (,pred ?))))))))
    (when preds
      (let ((*in-propogate* (first preds)))
	(ask (first preds)))
      (propogate-ask (rest preds)))))

(defun schedule-propogate (pred)
  #+conflog-debug
  (format t "~&SCHEDULE PROPOGATION OF ~A FOR [ ~{~A~^, ~} ]~%" pred (gethash pred *relation*))
  (setf *propogating* 
	(append *propogating* 
		(list (lambda ()
			#+conflog-debug
			(format t "~&PROPOGATING ~A:~%" pred)
			(propogate-ask (gethash pred *relation*)))))))

(defun run-propogate ()
  (loop for p in *propogating* do (funcall p))
  (setf *propogating* nil))

(define-primitive propogate/1 (pred cont)
  (schedule-propogate pred)
  (funcall cont))

(define-primitive ^/1 (pred cont)
  (when (eq pred *in-propogate*)
    (funcall cont)))

;;; Low-level Interface
(defmacro with-rules ((&rest rules) &rest queries)
  (labels ((do-query (queries)
		     (cond ((null queries) nil)
			   (t `((progn (clear-status) ,(car queries))
				,@(do-query (cdr queries)))))))
    `(progn (clear-rules)
	    ,@rules
	    (list ,@(do-query queries)))))

(defmacro with-status ((&rest cons) &body body)
  `(unwind-protect
       (progn (setup-status ',cons)
	      ,@body)
     (clear-status)))