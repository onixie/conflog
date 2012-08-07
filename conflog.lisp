;;;; conflog.lisp

(in-package #:conflog)
;;; "conflog" goes here. Hacks and glory await!

;; Primitives

(let ((status (make-hash-table))
      (prev-status (make-hash-table))
      (N/A (gensym)))
  (labels  ((found? (v) (not (eq v N/A)))
	    (lookup (var val db cont)
	      (let ((s (gethash var db N/A)))
		(when (found? s)
		  (paiprolog::unify! val s)
		  (funcall cont)))))
    
    (defun paiprolog::status/2 (var val cont)
      (lookup var val status cont))
    
    (defun paiprolog::prev-status/2 (var val cont)
      (lookup var val prev-status cont))

    (defun paiprolog::sstatus/2 (var val cont)
      (let ((prev (gethash var status (gensym))))
	(when (symbol-package prev)
	  (setf (gethash var prev-status) prev)))
      (setf (gethash var status) val)
      (funcall cont))

    (defun paiprolog::dump-status/0 (cont)
      (declare (ignorable cont))
      (princ (hash-table-plist status))
      (finish-output))))