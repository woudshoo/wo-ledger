(uiop:define-package :wo-ledger/widgets/account-entry-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session

	  #:wo-ledger/app/session
	  #:wo-ledger/widgets/entry

	  #:wo-ledger/logic/entry
	  #:wo-ledger/logic/account
	  #:ledger)
  (:export
   #:render-account-entry-list
   #:account-entry-list))

(in-package :wo-ledger/widgets/account-entry-list)

(defwidget account-entry-list ()
  ((account :accessor account :initarg :account)))

(defun render-account-entry-list-internal (account)
  (with-html
    (:h2  (account-display-name account)  " [" (account-display-value account) "]")
    (:al-table
     (render-entry-header)
     (loop :with iter = (entries-iterator (session-binder))
	   :for entry = (funcall iter)
	   :while entry
	   :when (entry-relevant-for-account entry account)
	   :do
	      (render-entry entry account)))))

(defmethod render ((ael account-entry-list))
  (render-account-entry-list-internal (account ael)))


(defun render-account-entry-list (account)
  (render (make-instance 'account-entry-list :account account)))

