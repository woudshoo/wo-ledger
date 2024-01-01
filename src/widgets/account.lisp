(uiop:define-package :wo-ledger/widgets/account
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session
	  #:ledger)
  (:export
   #:render-account))

(in-package :wo-ledger/widgets/account)


(defun render-account (account &optional selected)
  (with-html
    (:span :style (format nil "color:~A" (if selected "red" "green"))
	   (if (account-parent account)
	       (account-name account)
	       "<ROOT>"))))
