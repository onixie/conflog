(in-package #:conflog-gui)
;(climi::use-pixie)

;;; Nominee Pad
(defun exit-frame (&rest rest)
    (declare (ignore rest))
    (frame-exit *application-frame*))

(run-dialog (make-dialog
	     ;; Name
	     Nominee-Pad
	     
	     ;; Controls
	     ((Nominee (make-radiobox (Nominee) (Obama
						 Romney
						 Johnson
						 Stein)))
	      (Property (make-listbox (Property :init-status "1") ("1" "2" "3")))
	      (Rate (make-slider (Rate :init-status 0 :min 0 :max 3)))
	      (Good-Nature (make-checkbox (Good-Nature) ((A "Understand the constitution of the United States of America")
							 (B "Have a good knowledge of American History")
							 (C "Understand the various types of political, social and economic systems")
							 (D "Understand the freedoms embodied in the Bill of Rights")
							 (E "Understand cultures that are different from the society")
							 (F "Understand that he is chief amoung the servants of the People")
							 (G "Have \"vision\" and purpose"))))
	      ;(Comment (make-editbox (Comment) :editable-p t))
	      (Quit (make-button (Quit :callback #'exit-frame))))

	     ;; Layouts
	     (vertically ()
	       Nominee
	       Property
	       Rate
	       Good-Nature
	       ;Comment
	       Quit)

	     ;; Rules
	     ;; (:- (Quit enable) (not (or (Comment "") (Comment disable))))
	     ;; (:- (Good-Nature (A B D)) (Nominee Obama))
	     ;; (:- (E disable) (Nominee Obama))
	     ;; (:- (Good-Nature (C D E)) (Nominee Romney))
	     ;; (:- (Good-Nature (G)) (Nominee Johnson))
	     ;; (:- (Good-Nature (E F)) (Nominee Stein))
	     ;; (:- (E enable) true)
	     ;; (:- (Quit disable) true)
	     ;; (:- (Nominee ?what) (status Nominee ?what))
	     ;; (:- (Comment ?what) (status Comment ?what))
	     
	     ;; 
	     
	     (:- (Good-Nature (A D E)) (Nominee Obama))
	     (:- (Good-Nature (C F)) (Nominee Romney))
	     (:- (Good-Nature.Status disable) (Nominee Johnson))
	     (:- (Good-Nature.Status hide) (Nominee Stein) (message "Washed out Nominee"))
	     (:- (Good-Nature.Status on))

	     (:- (Obama on) (Rate 0))
	     (:- (Obama off))
	     
	     (:- (Nominee Romney) (Rate 1))
	     (:- (Nominee Johnson) (Rate 2))
	     (:- (Nominee Stein) (Rate 3))

	     (:- (Property.Status disable) (Nominee Stein))
	     (:- (Property.Status on))
	     
	     (:- ($$ ?) (Quit ?any) (inform "You are closing the pad."))
	     (:- ($$ ?) (error "You should not see this"))))