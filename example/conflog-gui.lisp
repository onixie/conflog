(in-package #:cl)

(defpackage #:conflog-gui
  (:use #:clim-lisp #:clim #:conflog))

(in-package #:conflog-gui)

;;; Compiler Utils
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-id/label (name)
    (if (listp name)
	`(:id ',(first name) :label ,(second name))
	`(:id ',name :label ,(symbol-name name))))

  (defmacro t/nil<->on/off (thing)
    (ecase thing
      ((t) ''on)
      ((nil) ''off)
      ((on) t)
      ((off) nil)))

  (defun default-callback (g v)
    (declare (ignore g v))
    nil)

  (defun exit-frame (&rest rest)
    (declare (ignore rest))
    (frame-exit *application-frame*)))


;;; Conflog register
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *pred* nil))

(defvar *id/gadget* (make-hash-table))

(defmacro conflog-regist (gadget init-status &body body)
  `(progn
     (set-init-status (gadget-id ,gadget) ,init-status)
     (set-status (gadget-id ,gadget) ,init-status)
     (setf (gethash (gadget-id ,gadget) *id/gadget*) ,gadget)
     (regist (gadget-id ,gadget)
	     (lambda (id prev-status status)
	       (declare (ignorable id prev-status status))
	       ,@body))
     ,gadget))

(defun set-on/off (gadget status)
  (case status
    ((disable) (deactivate-gadget gadget))
    ((enable) (activate-gadget gadget))))

(defun conflog-regist-on/off-status (gadget init-status)
  (set-on/off gadget init-status)
  (conflog-regist gadget init-status
    (set-on/off gadget status)))

(defun set-selections (gadget status)
  (when status
    (if (atom status)
	(when (gethash status *id/gadget* nil)
	  (setf (gadget-value gadget) (gethash status *id/gadget*)))
	(setf (gadget-value gadget) (remove-if #'null (mapcar (lambda (id) (gethash id *id/gadget* nil)) status))))))

(defun conflog-regist-selection-status (gadget init-status)
  (set-selections gadget init-status)
  (conflog-regist gadget init-status
    (set-selections gadget status)))

(defun set-range (gadget status)
  (if (numberp status)
      (setf (gadget-value gadget) status)
      (set-on/off gadget status)))

(defun conflog-regist-range-status (gadget init-status)
  (set-range gadget init-status)
  (conflog-regist gadget init-status
    (set-range gadget status)))

(defun set-editable (gadget status)
  (if (stringp status)
      (setf (gadget-value gadget) status)
      (set-on/off gadget status)))

(defun conflog-regist-text-status (gadget init-status)
  (set-editable gadget init-status)
  (conflog-regist gadget init-status
    (set-editable gadget status)))

(defun conflog-refresh-all ()
  (loop for id being each hash-key in *id/gadget* using (hash-value gadget)
     do (progn (refresh-status/resolve id))))

(defmacro conflog-compensate-rules ()
  `(progn ,@(append (loop for pred in (reverse conflog::*head-pred*)
		       collect `(:- (,pred ?what) (status ,pred ?what)))
		    (loop for pred in (reverse *pred*)
		       unless (member pred conflog::*head-pred*)
		       collect `(:- (,pred ?what) (status ,pred ?what))))))
;;; Conflog-featured Clim Gadgets

(defmacro make-button ((name &key (callback #'default-callback) (init-status nil)) &rest pane-options)
  (pushnew name *pred*)
  `(conflog-regist-on/off-status
    (make-pane 'push-button
	       ,@(generate-id/label name)
	       :activate-callback ,callback ,@pane-options)
    ,init-status))

(defmacro make-selection (type (name &key (callback #'default-callback) (init-status nil)) selections &rest pane-options)
  (pushnew name *pred*)
  (loop for selection in selections do (pushnew (if (atom selection) selection (first selection)) *pred*))
  `(let* ((choices (list ,@(mapcar (lambda (selection)
				     `(conflog-regist-on/off-status
				       (make-pane 'toggle-button
						  ,@(generate-id/label selection)
						  :indicator-type ,(if (eq type 'radio-box) :one-of :some-of))
				       'enable))
				   selections)))
	  (current ,(if (eq type 'radio-box)
			`(or (gethash ',init-status *id/gadget* nil) (first choices))
			(when init-status
			  `(remove-if #'null (mapcar (lambda (s) (gethash s *id/gadget* nil)) ',init-status))))))
     (conflog-regist-selection-status
      (make-pane ',type ,@(generate-id/label name)
		 :choices choices
		 :selection current
		 :value-changed-callback (lambda (gadget value)
					   (set-status/resolve (gadget-id gadget)
							       ,(if (eq type 'radio-box)
								    `(gadget-id value)
								    `(sort (mapcar #'gadget-id value) #'string<)))
					   (funcall ,callback gadget value))
		 ,@pane-options)
      ,(if (eq type 'radio-box)
	   `(or ',init-status (gadget-id current))
	   `',init-status))))

(defmacro make-radiobox (&rest args)
  `(make-selection radio-box ,@args))

(defmacro make-checkbox (&rest args)
  `(make-selection check-box ,@args))

(defmacro make-slider ((name &key (callback #'default-callback) (init-status 0) (min 0) (max 100)) &rest pane-options)
  (pushnew name *pred*)
  `(conflog-regist-range-status
    (make-pane 'slider :id ',name
	       :min-value ,min :max-value ,max 
	       :value ,min :number-of-quanta ,max
	       :orientation :horizontal
	       :value-changed-callback (lambda (gadget value)
					 (set-status/resolve (gadget-id gadget) value)
					 (funcall ,callback gadget value))
	       ,@pane-options)
    ,init-status))

(defmacro make-editbox ((name &key (callback #'default-callback) (init-status "")) &rest pane-options)
  (pushnew name *pred*)
  `(conflog-regist-text-status
    (make-pane 'text-field :id ',name
	       :value-changed-callback (lambda (gadget value)
					 (set-status/resolve (gadget-id gadget) value)
					 (funcall ,callback gadget value))
	       ,@pane-options)
    ,init-status))

(defmacro make-dialog (name
		       (&rest control-list)
		       (&rest control-layout)
		       &rest conflog-rules)
  (let ((conflog-debug (gensym)))
    `(progn
       (define-application-frame ,name ()
	 ()
	 (:panes
	  #+conflog-debug
	  (,conflog-debug :application :width 800 :scroll-bars :both)
	  ,@control-list)
	 (:layouts
	  (default
	      #+conflog-debug
	      (horizontally ()
		(1/4 ,control-layout)
		(3/4 (labelling (:label "Conflog Log"),conflog-debug)))
	    #-conflog-debug
	    ,control-layout)))
       (prog1 (make-instance ',name)
	 ,@conflog-rules
	 (conflog-compensate-rules)))))

(defun run-dialog (dialog &rest options)
  (clim-sys:make-process #'(lambda ()
			     (apply #'run-frame-top-level dialog options))
			 :name (format nil "~A" (frame-name dialog)))
  (sleep 0.5)
  (conflog-refresh-all))