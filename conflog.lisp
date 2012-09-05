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

(eval-when (:compile-toplevel)
  (defconstant +indent-amount+ 4))
(defvar *indent-level* 0)
(defmacro indent-format-line (&rest args)
  `(progn
     (format t "~&")
     (loop for i from *indent-level* above 0 do (princ #\Space))
     (format t ,@args)
     (terpri)))

(defmacro with-indent (() &body body)
  `(let ((*indent-level* (+ *indent-level* +indent-amount+)))
     ,@body))

;;; Primitives
(defmacro define-primitive (name (&rest args) &body body)
  (let ((pname (intern (symbol-name name) :paiprolog)))
    `(progn
       (defun ,pname (,@args)
	 #+conflog-debug
	 (indent-format-line "CALL (~A ~{~A~^ ~})" ',pname (butlast (list ,@args)))
	 (unwind-protect (with-indent () ,@body)
	   #+conflog-debug
	   (indent-format-line "RETURN (~A ~{~A~^ ~})" ',pname (butlast (list ,@args)))))
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

(defvar *status-hook* (make-hash-table))
(defmacro add-hook (pred hook)
  `(setf (gethash ,pred *status-hook*) ,hook))
(defmacro remove-hook (pred)
  `(setf (gethash ,pred *status-hook*) +N/A+))
(defmacro call-hook (pred prev-status status)
  `(let ((hook (gethash ,pred *status-hook* +N/A+)))
     (when (found? hook)
       (funcall hook ,pred ,prev-status ,status))))

(defun sstatus (pred val cont &optional (schedule-propogate nil))
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
	    (funcall schedule-propogate pred)))
	(call-hook pred (found? pval) val))
      (funcall cont))))

(define-primitive sstatus/2 (pred val cont)
  (sstatus pred val cont))

(define-primitive maybe-propogate-sstatus/2 (pred val cont)
  (sstatus pred val cont #'schedule-propogate))

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
		       (maybe-propogate-sstatus/2 pred var cont)))))))

(define-primitive lookup/2 (pred var cont)
  (when (not (unbound-var-p pred))
    (let ((s (status (deref pred))))
      (if (found? s)
	  (status/2 pred var cont)
	  (apply/1 `(,(deref pred) ,(?)) (lambda () (status/2 pred var cont)))))))

(define-primitive member/2 (item list cont)
  (when (some (lambda (v) (unify! item v)) (deref list))
    (funcall cont)))

(define-primitive not/1 (goal cont)
  (unless (call/1 goal (lambda () t))
    (funcall cont)))

;;; Conflict Rule
(defvar *total-rules* 0)
(defvar *relation* (make-hash-table))
(defun add-relation (head body)
  (cond ((null body) nil)
	(t (let ((pred (predicate (first body))))
	     (case pred
	       ((or and not) (add-relation head (args (first body))))
	       (t (let ((old (gethash pred *relation* nil)))
		    (unless (member (predicate head) old)
		      (setf (gethash pred *relation*)
			    (append old (list (predicate head)))))))))
	   (add-relation head (rest body)))))

(defmacro conflict-rule (head &body body)
  (let ((rule-str (format nil "[~d] (:- ~A ~{~A~^ ~})" (incf *total-rules*) head body)))
    (declare (ignorable rule-str))
    `(progn
       (<- ,head
	   #+conflog-debug (lisp (indent-format-line "~A" ,rule-str))
	   ,@body)

       (add-relation ',head ',body)

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

(define-alias :- conflict-rule)

;;; Query Rule
(define-primitive return/2 (var-names vars cont)
  (throw :ret
    (or (null vars)
	(mapcar (lambda (var-name var)
		  (cons (intern var-name) (deref-exp var)))
		var-names vars))))

(defun query (goals)
  (clear-predicate 'top-level-query)
  (let* ((goals (replace-?-vars goals))
	 (vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (return ,(mapcar #'symbol-name vars) ,vars))))
  (catch :ret
    (run-prolog 'top-level-query/0 #'ignore)))

(defun query/run-propogate (goals)
  (prog1 (query goals)
    (run-propogate)))

(defmacro query-rule (&rest goals)
  `(query/run-propogate ',goals))

(define-alias ?- query-rule)

;;; Propogation
(defvar *propogating* nil)
(defvar *in-propogate* nil)

(defun propogate-refresh (&optional (preds nil))
  (flet ((refresh (pred)
	   #+conflog-debug
	   (format t "~&--> ~A ~%" pred)
	   (query `((apply (,pred ?))))))
    (when preds
      (refresh (first preds))
      (propogate-refresh (rest preds)))))

(defun schedule-propogate (pred)
  (let ((preds (gethash pred *relation* nil)))
    (when preds
      #+conflog-debug
      (indent-format-line "SCHEDULE PROPOGATION OF ~A FOR [ ~{~A~^, ~} ]" pred preds)
      (setf *propogating* 
	    (append *propogating* 
		    (list (lambda ()
			    #+conflog-debug
			    (format t "~&PROPOGATING ~A TO:~%" pred)
			    (let ((*in-propogate* pred))
			      (propogate-refresh preds)))))))))

(defun run-propogate ()
  (labels ((the-loop ()
	     (when *propogating*
	       (funcall (pop *propogating*))
	       (the-loop))))
    (the-loop)))

(defun clear-propogate ()
  (setf *propogating* nil))

(define-primitive propogate/1 (pred cont)
  (schedule-propogate pred)
  (funcall cont))

(define-primitive ^/1 (pred cont)
  (when (eq (deref pred) *in-propogate*)
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

;;; High-level Interface
(declaim (inline get-status refresh-status refresh-status/resolve set-status set-status/resolve resolve regist))

(defun get-status (pred)
  (let ((res (first (query `((,pred ?what))))))
    (if (consp res)
	(cdr res)
	res)))

(defun refresh-status (pred)
  (query `((apply (,pred ?))))
  (clear-propogate)
  (get-status pred))

(defun refresh-status/resolve (pred)
  (query/run-propogate `((apply (,pred ?))))
  (get-status pred))

(defun set-status (pred status)
  (query `((sstatus ,pred ,status))))

(defun set-status/resolve (pred status)
  (query/run-propogate `((maybe-propogate-sstatus ,pred ,status))))

(defun resolve (pred)
  (query/run-propogate `((propogate ,pred))))

(defun regist (pred hook)
  (add-hook pred hook))