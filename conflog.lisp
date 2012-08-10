(in-package :conflog)
;;; "conflog" goes here. Hacks and glory await!

;;; Utils
(eval-when (:compile-toplevel)
  (defconstant +N/A+ (gensym)))

(defmacro found? (val)
  `(not (eq ,val +N/A+)))

(defmacro define-primitive (name (&rest args) &body body)
  (let ((pname (intern (symbol-name name) :paiprolog)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,pname (,@args) ,@body)
       (shadowing-import ',pname))))

;;; Primitives

(defmacro define-status/primitive (name)
  (let ((v (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
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

(define-primitive sstatus/2 (pred val cont)
  (when (and (not (unbound-var-p pred))
	     (not (unbound-var-p val)))
    (let* ((pred (deref pred))
	   (pval (status pred)))
      (when (found? pval)
	(setf (prev-status pred) pval))
      (setf (status pred) val)
      (funcall cont))))

(define-primitive dump-status/0 (cont)
  (declare (ignorable cont))
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
  (finish-output))

(macrolet ((define-apply-utils ()
	     (let ((in (gensym "in-apply")))
	       `(eval-when (:compile-toplevel :load-toplevel :execute)
		  (defvar ,in (make-hash-table))
		  (defun in-apply! (g) (setf (gethash g ,in) t))
		  (defun in-apply? (g) (prog1 (gethash g ,in nil) (setf (gethash g ,in) nil)))))))
  (define-apply-utils))

(define-primitive apply/1 (goal cont)
  (in-apply! (predicate goal))
  (call/1 goal (lambda () (sstatus/2 (predicate goal) (first (args goal)) cont))))

(define-primitive lookup/2 (pred var cont)
  (when (not (unbound-var-p pred))
    (let ((s (status (deref pred))))
      (unless (found? s)
	(apply/1 `(,(deref pred) ,(?)) #'ignore))
      (status/2 pred var cont))))

;;; Conflict Rule
(defmacro :- (head &body body)
  `(progn
     (<- ,head ,@body)
     (def-prolog-compiler-macro ,(first head)
	 (goal body cont bindings)
       (if (in-apply? (predicate goal))
	   :pass
	   (compile-body (cons `(lookup ,(predicate goal) ,@(args goal)) body) cont bindings)))
     ',(predicate head)))