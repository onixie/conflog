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
	      (Rate (make-slider (Rate :init-status 0 :min 0 :max 5)))
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
	     (:- (Nominee ?value) ($VAL_Nominee ?value) ($FNS_Nominee ?status)
		 (or (and (== ?status disable) (show Nominee on) (enable Nominee off))
		     (or (and (== ?status hide) (show Nominee off))
			 (and (enable Nominee on) (show Nominee on)))))
	     
	     (:- ($VAL_Nominee ?value) (Nominee ?value))

	     (:- ($FNS_Nominee ?status)
	       (or (and (show Nominee ?off) (== ?off off) (= ?status hide))
		   (or (and (enable Nominee ?off) (== ?off off) (= ?status disable))
		       (= ?status on))))
	     ;; 
	     (:- (Good-Nature ?value) ($VAL_Good-Nature ?value) ($FNS_Good-Nature ?status)
		 (or (and (== ?status disable) (show Good-Nature on) (enable Good-Nature off))
		     (or (and (== ?status hide) (show Good-Nature off))
			 (and (enable Good-Nature on) (show Good-Nature on)))))

	     (:- ($VAL_Good-Nature (A D E)) ($VAL_Nominee Obama))
	     (:- ($VAL_Good-Nature (C F)) ($VAL_Nominee Romney))
	     (:- ($FNS_Good-Nature disable) ($VAL_Nominee Johnson))
	     (:- ($FNS_Good-Nature hide) ($VAL_Nominee Stein) (message "Washed out Nominee"))
	     
	     (:- ($VAL_Good-Nature ?value) (Good-Nature ?value))
	     (:- ($FNS_Good-Nature ?status)
	       (or (and (enable Good-Nature ?off) (== ?off off) (= ?status disable))
		   (or (and (show Good-Nature ?off) (== ?off off) (= ?status hide))
		       (= ?status on))))
	     
	     (:- ($$ ?) (Quit ?any) (inform "You are closing the pad."))
	     (:- ($$ ?) (error "You should not see this"))))