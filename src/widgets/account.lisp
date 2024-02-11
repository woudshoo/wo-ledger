(uiop:define-package :wo-ledger/widgets/account
    (:use #:cl
#+nil	  #:alexandria
	  #:reblocks/html
#+nil	  #:reblocks/widget
#+nil	  #:reblocks/session

	  #:wo-ledger/logic/account
#+nil	  #:ledger)
  (:export
   #:render-account))

(in-package :wo-ledger/widgets/account)


(defun render-account (account &optional selected)
  (with-html
    (:span :style (format nil "color:~A" (if selected "red" "green"))
	   (account-display-name account))))
