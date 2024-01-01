(uiop:define-package :wo-ledger/widgets/entry
    (:use #:cl
	  #:reblocks/html
	  #:reblocks/widget

	  #:wo-ledger/logic/entry
	  #:ledger)
  (:export
   #:render-entry
   #:render-entry-header))

(in-package :wo-ledger/widgets/entry)



(defun render-entry-header ()
  (with-html
    (:tr
     (:th "Date")
     (:th "Payee")
     (:th "Status")
     (:th "Code")
     (:th "Entry Note")
     (:th "Account/Fund")
     (:th "Amount")
     (:th "Virtual")
     (:th "Tran Note"))))

(defun render-entry (entry &optional account)
  "Renders the entry to the output stream.

Could be part of the render pipeline of an entry widger,
or used raw.


The optional account does some tricky filtering:

1. It will show the entry if at least one of the transactions involve account.
2. It will not show the transactions for account.
"

  (let* ((transactions (transactions-relevant-for-account entry account))
	 (current-transaction (pop transactions)))
    (when current-transaction
      
      (with-html
	(:tr
	 (:td  (periods:strftime (entry-actual-date entry)))
	 (:td  (entry-payee entry))
	 (:td  (entry-status entry))
	 (:td  (entry-code entry))
	 (:td  (entry-note entry))
	 (:td  (account-fullname (xact-account current-transaction)))
	 (:td  (cambl:print-value (xact-amount current-transaction)
				  :output-stream *stream*))
	 (:td (xact-virtualp current-transaction))
	 (:td  (xact-note current-transaction)))
	(loop
	  :for current-transaction = (pop transactions)
	  :while current-transaction
	  :do
	     (with-html
	       (:tr
		(:td) (:td) (:td) (:td) (:td)
		(:td  (account-fullname (xact-account current-transaction)))
		(:td  (cambl:print-value (xact-amount current-transaction)
					 :output-stream *stream*))
		(:td (xact-virtualp current-transaction))
		(:td (xact-note current-transaction)))))))))
