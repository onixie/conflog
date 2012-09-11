(in-package #:conflog-gui)
(climi::use-pixie)

;;; Nominee Pad

(defparameter *Nominee-Pad*
  (make-dialog Nominee-Pad
	       ((Nominee (make-radiobox (Nominee)
					(Obama
					 Romney
					 Johnson
					 Stein)))
		(Rate (make-slider (Rate :init-status 0 :min 0 :max 5)))
		(Good-Nature (make-checkbox (Good-Nature :init-status (G))
					    ((A "Understand the constitution of the United States of America")
					     (B "Have a good knowledge of American History")
					     (C "Understand the various types of political, social and economic systems")
					     (D "Understand the freedoms embodied in the Bill of Rights")
					     (E "Understand cultures that are different from the society")
					     (F "Understand that he is chief amoung the servants of the People")
					     (G "Have \"vision\" and purpose"))))
		(Comment (make-editbox (Comment :init-status 'disable)))
		(Quit (make-button (Quit :init-status 'enable :callback #'exit-frame))))
	       (vertically ()
		 Nominee
		 Rate
		 Good-Nature
		 Comment
		 Quit)
	       (:- (Quit enable) (not (or (Comment "") (Comment disable))))
	       (:- (Good-Nature (A B D)) (Nominee Obama))
	       (:- (E disable) (Nominee Obama))
	       (:- (Good-Nature (C D E)) (Nominee Romney))
	       (:- (Good-Nature (G)) (Nominee Johnson))
	       (:- (Good-Nature (E F)) (Nominee Stein))
	       (:- (D enable) true)
	       (:- (Quit disable) true)
	       (:- (Nominee ?what) (status Nominee ?what))
	       (:- (Comment ?what) (status Comment ?what))))

(run-dialog *Nominee-Pad*)