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

  (defun default-callback (&rest args)
    (declare (ignore args))
    nil)
  
  (defun symb (&rest args)
    (intern (format nil "~{~A~}" args))))


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

(defun conflog-regist-no-status (gadget)
  (conflog-regist gadget (gensym)))

(defun set-on/off (gadget status)
  (setf (gadget-value gadget)
	(case status
	  ((on) t)
	  ((off) nil))))

(defun conflog-regist-on/off-status (gadget init-status)
  (set-on/off gadget init-status)
  (conflog-regist gadget init-status
    (set-on/off gadget status)))

(defun set-selections (gadget status)
  (when status
    (if (atom status)
	(let ((g (gethash status *id/gadget* nil)))
	  (when g
	    (setf (gadget-value gadget) g)))
	(setf (gadget-value gadget) (remove-if #'null (mapcar (lambda (id) (gethash id *id/gadget* nil)) status))))))

(defun conflog-regist-selection-status (gadget init-status)
  (set-selections gadget init-status)
  (conflog-regist gadget init-status
    (set-selections gadget status)))

(defun set-range (gadget status)
  (when (numberp status)
    (setf (gadget-value gadget) status)))

(defun conflog-regist-range-status (gadget init-status)
  (set-range gadget init-status)
  (conflog-regist gadget init-status
    (set-range gadget status)))

(defun set-string (gadget status)
  (when (stringp status)
    (setf (gadget-value gadget) status)))

(defun conflog-regist-text-status (gadget init-status)
  (set-string gadget init-status)
  (conflog-regist gadget init-status
    (set-string gadget status)))

(defun conflog-regist-combo-status (gadget init-status)
  (setf (gadget-value gadget) init-status)
  (conflog-regist gadget init-status
    (setf (gadget-value gadget) status)))

(defun conflog-refresh-all ()
  (loop for id being each hash-key in *id/gadget* using (hash-value gadget)
     do (progn (refresh-status/resolve id))))

(defmacro conflog-gen-prologue-rules ()
  `(progn ,@(loop for pred in (reverse *pred*)
	       collect `(:- (,pred ?value) (or (^ ,(symb pred '.value)) (^ ,(symb pred '.status)))
			    (,(symb pred '.value) ?value)
			    (,(symb pred '.status) ?status) (or (and (== ?status disable) (show ,pred on) (enable ,pred off))
								(or (and (== ?status hide) (show ,pred off))
								    (and (show ,pred on) (enable ,pred on))))))))

(defmacro conflog-gen-epilogue-rules ()
  `(progn ,@(loop for pred in (reverse *pred*)
	       append `((:- (,(symb pred '.value) ?value) (,pred ?value))
			(:- (,(symb pred '.status) ?status) (or (and (show ,pred ?off) (== ?off off) (= ?status hide))
								(or (and (enable ,pred ?off) (== ?off off) (= ?status disable))
								    (= ?status on))))))))

(defmacro conflog-gen-toggle-selections-linkage-rules ()
  `(progn ,@(loop for pred in (reverse *pred*)
	       and gadget = (gethash pred *id/gadget* nil)
	       when (and gadget (or (typep gadget 'radio-box) (typep gadget 'check-box)))
	       append (let ((box-id (gadget-id gadget))
			     (check-ids (mapcar #'gadget-id (sheet-children gadget))))
			 (if (typep gadget 'radio-box)
			     (loop for check-id in check-ids
				append `((:- (,box-id ,check-id) (^ ,check-id) (,check-id on))
					 (:- (,check-id on) (^ ,box-id) (,box-id ,check-id))
					 (:- (,check-id off) (^ ,box-id))))
			     (append
			      `((:- (,box-id ?new)
				  ,@(mapcan (lambda (check-id) `((^ ,check-id) (,check-id ?))) check-ids)
				  (is ?new (sort (remove-if #'null (mapcar (lambda (check-id) 
									     (when (gadget-value (gethash check-id *id/gadget*)) 
									       check-id))
									   ',check-ids))
						 #'string<))))
			      (loop for check-id in check-ids
				 append `((:- (,check-id on) (^ ,box-id) (,box-id ?checks) (member ,check-id ?checks))
					  (:- (,check-id off) (^ ,box-id))))))))))

(defmacro conflog-gen-compensate-rules ()
  `(progn ,@(append (loop for pred in (reverse (remove-duplicates (mapcar #'first conflog::*head*)))
		       collect `(:- (,pred ?value) (status ,pred ?value)))
		    (loop for pred in (reverse *pred*)
		       unless (member pred (mapcar #'first conflog::*head*))
		       collect `(:- (,pred ?value) (status ,pred ?value)))
		    (loop for pred in (reverse *pred*)
		       append `((:- (,(symb pred '.value) ?value) (status ,(symb pred '.value) ?value))
				(:- (,(symb pred '.status) ?status) (status ,(symb pred '.status) ?status)))))))

;;; Conflog-featured Clim Gadgets
(defmacro make-button ((name &key (callback #'default-callback)) &rest pane-options)
  (pushnew name *pred*)
  `(conflog-regist-no-status
    (make-pane 'push-button
	       ,@(generate-id/label name)
	       :activate-callback (lambda (gadget)
				    (set-status/resolve (gadget-id gadget) (gensym))
				    (funcall ,callback gadget)) ,@pane-options)))

(defmacro make-selection (type (name &key (callback #'default-callback) (init-status nil)) selections &rest pane-options)
  (pushnew name *pred*)
  (loop for selection in selections do (pushnew (if (atom selection) selection (first selection)) *pred*))
  `(let* ((choices (list ,@(mapcar (lambda (selection)
				     `(conflog-regist-on/off-status
				       (make-pane 'toggle-button
						  ,@(generate-id/label selection)
						  :indicator-type ,(if (eq type 'radio-box) :one-of :some-of)
						  :value-changed-callback (lambda (gadget value)
									    (set-status/resolve (gadget-id gadget) (if value 'on 'off))))
				       'off))
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

(defmacro make-combo ((name &key (callback #'default-callback) (init-status nil)) selections &rest pane-options)
  (pushnew name *pred*)
  `(conflog-regist-combo-status
    (make-pane 'option-pane :id ',name :items ',selections :value ,init-status 
	       :value-changed-callback (lambda (gadget value)
					 (set-status/resolve (gadget-id gadget) value)
					 (funcall ,callback gadget value)))
    ,init-status))


;;; Dialog
(defvar *dialog* nil)
(defmacro make-dialog (name
		       (&rest control-list)
		       (&rest control-layout)
		       &rest conflog-rules)
  (let ((conflog-debug (gensym)))
    (declare (ignorable conflog-debug))
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
       (prog1 (setf *dialog* (make-instance ',name))
	 (clear-rules)
	 (clear-status)
	 (conflog-gen-prologue-rules)
	 (conflog-gen-toggle-selections-linkage-rules)
	 ,@conflog-rules
	 (conflog-gen-epilogue-rules)
	 (conflog-gen-compensate-rules)))))

(defun run-dialog (dialog &rest options)
  (clim-sys:make-process #'(lambda ()
			     (apply #'run-frame-top-level dialog options))
			 :name (format nil "~A" (frame-name dialog)))
  (loop until (not (eq (frame-state dialog) :disowned)))
  (conflog-refresh-all)
  dialog)

;;; primitives for gui operation
(conflog::define-primitive enable/2 (pred status cont)
  (let ((g (gethash pred *id/gadget* nil)))
    (when g
      (if (conflog::unbound-var-p status)
	  (conflog::unify! status (if (gadget-active-p g) 'on 'off))
	  (case (intern (symbol-name (conflog::deref status)) :keyword)
	    (:off (deactivate-gadget g))
	    (:on (activate-gadget g))))))
  (funcall cont))

(conflog::define-primitive show/2 (pred status cont)
  (let ((g (gethash pred *id/gadget* nil)))
    (when g
      (if (conflog::unbound-var-p status)
	  (conflog::unify! status (if (gadget-active-p g) 'on 'off))
	  (case (intern (symbol-name (conflog::deref status)) :keyword)
	    (:on (setf (sheet-enabled-p g) t))
	    (:off (setf (sheet-enabled-p g) nil))))))
  (funcall cont))

(conflog::define-primitive enable-element/2 (pred-lst status-lst cont)
  (mapc (lambda (pred status) (enable/2 pred status (lambda () t))) pred-lst status-lst)
  (funcall cont))

(conflog::define-primitive show-element/2 (pred-lst status-lst cont)
  (mapc (lambda (pred status) (show/2 pred status (lambda () t))) pred-lst status-lst)
  (funcall cont))

(conflog::define-primitive message/3 (title text style cont)
  (notify-user *dialog* (format nil "~5:i~A~5i" text) :title title :style style)
  (funcall cont))

(conflog::define-primitive message/2 (title text cont)
  (message/3 "Info" text :inform cont))

(conflog::define-primitive message/1 (text cont)
  (message/3 "Info" text :inform cont))

(conflog::define-primitive inform/1 (text cont)
  (message/3 "Info" text :inform cont))

(conflog::define-primitive warning/1 (text cont)
  (message/3 "Warning" text :warning cont))

(conflog::define-primitive error/1 (text cont)
  (message/3 "Error" text :error cont))