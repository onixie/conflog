;;;; conflog-test.lisp

(in-package #:conflog-test)

;;; "conflog-test" goes here. Hacks and glory await!

;;; Define
(defmacro define-conflog-test (name (&rest rules) &rest asserts)
  (let ((index 0))
    (declare (ignorable index))
    `(deftest ,name
	 (with-rules (,@rules) ,@(mapcar (lambda (assert)
					   `(progn
					      #+conflog-debug
					      (format t "~&Test ~A.~D~&----------~&Rules:~{~&  ~S~}~&Ask:~&  ~A~&Expected:~&  ~A~&Log:~%" ',name ,(incf index) ',rules ',(first assert) ',(second assert))
					      ,(first assert))) asserts))
       ,(mapcar #'second asserts))))

;;; Tests
(define-conflog-test T1 ((:- (A on) true))
  ((?- (A on)) t)
  ((?- (A ?what)) ((?what . on))))

(define-conflog-test T2 ((:- (A on) fail))
  ((?- (A on)) nil))

(define-conflog-test T3 ((:- (A ?what) (B ?what))
			 (:- (B ?what) (status B ?what)))
  ((with-status ((B . on))
     (?- (A on)))
   t)
  ((with-status ((B . off))
     (?- (A ?what)))
   ((?what . off))))

(define-conflog-test T4 ((:- (A ?what) (B ?what))
			 (:- (B ?what) (C ?what))
			 (:- (C ?what) (D on) (= ?what off))
			 (:- (C ?what) (D off) (= ?what on))
			 (:- (D ?what) (status D ?what)))
  ((with-status ((D . on))
     (?- (A off) (B off) (C off))) 
   t)
  ((with-status ((D . off))
     (?- (A ?whatA) (B ?whatB) (C ?whatC))) 
   ((?whatA . on) (?whatB . on) (?whatC . on))))

(define-conflog-test T5 ((:- (A on) (status B ?what) (member ?what (A B C D))))
  ((?- (sstatus B A) (A on)) t)
  ((?- (sstatus B B) (A on)) t)
  ((?- (sstatus B D) (A on)) t)
  ((?- (sstatus B W) (A on)) nil))

(define-conflog-test T6 ((:- (A ?what) (status A ?what)))
  ((?- (sstatus A on) (A ?what)) ((?what . on))))

(define-conflog-test T7 ((:- (A ?what) (B ?what))
			 (:- (B ?what) (status B ?what)))
  ((?- (sstatus B on) (A ?what)) ((?what . on)))
  ((?- (sstatus A off) (sstatus B on) (A ?what)) ((?what . off)))
  ((?- (sstatus A off) (sstatus B on)
       (status A ?what1)
       (A ?what2)
       (apply (A ?what3))
       (status A ?what4)
       (A ?what5))
   ((?what1 . off)
    (?what2 . off)
    (?what3 . on)
    (?what4 . on)
    (?what5 . on))))

(define-conflog-test T8 ((:- (A ?what) (init-status A ?what)))
  ((progn (setup-init-status '((A . on)))
	  (?- (A ?what1)
	      (sstatus A off)
	      (status A ?what2)
	      (prev-status A ?what3)))
   ((?what1 . on) (?what2 . off) (?what3 . on))))

(define-conflog-test T9 ((:- (A on) (B off))
			 (:- (A off) (B on))
			 (:- (B ?what) (init-status B ?what)))
  ((progn (setup-init-status '((B . on)))
	  (setup-status '((B . off)))
	  (?- (apply (B ?what)))
	  (?- (A ?what)))
   ((?what . off))))

(define-conflog-test T10 ((:- (A on) (B off))
			  (:- (A off) (B on))
			  (:- (B ?what) (status B ?what)))
  ((progn (?- (sstatus B on) (propogate-sstatus B off))
	  (?- (A ?what)))
   ((?what . on))))


(define-conflog-test T11 ((:- (A on) (B off))
			  (:- (A off) (B on))
			  (:- (C on) (A on))
			  (:- (B ?what) (status B ?what))
			  (:- (C off) true))
  ((with-status ((B . on) (A . off))
     (?- (propogate-sstatus B off))
     (?- (apply (C ?what))))
   ((?what . on)))
  ((with-status ((B . on))
     (?- (propogate-sstatus B off)
	 (propogate-sstatus B on))
     (?- (apply (C ?what))))
   ((?what . off))))