(uiop:define-package :wo-ledger/widgets/account
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session

	  #:wo-ledger/logic/account
	  #:ledger)
  (:export
   #:render-account))

(in-package :wo-ledger/widgets/account)


(defun render-account (account &optional selected)
  (with-html
    (:span :style (format nil "color:~A" (if selected "red" "green"))
	   (account-display-name account)
#+nil	   (if (account-parent account)
	       (account-fullname account)
	       "<ROOT>"))))
