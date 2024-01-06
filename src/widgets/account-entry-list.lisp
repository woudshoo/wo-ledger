(uiop:define-package :wo-ledger/widgets/account-entry-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session

	  #:wo-ledger/app/session
	  #:wo-ledger/widgets/entry

	  #:wo-ledger/logic/account
	  #:ledger)
  (:export
   #:render-account-entry-list))

(in-package :wo-ledger/widgets/account-entry-list)


(defun render-account-entry-list (account)
  (with-html
    (:h2  (account-display-name account)  " [" (account-display-value account) "]")
    (:table
     (:thead
      (render-entry-header))
     (:tbody
      (loop :with iter = (entries-iterator (session-binder))
	    :for entry = (funcall iter)
	    :while entry
	    :do
	    (render-entry entry account))))))
