(uiop:define-package :wo-ledger/widgets/entry
    (:use #:cl
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/actions
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
     (:th "Account/Fund")
     (:th "Amount")
     (:th "Target")
     (:th "Status"))))


(defun render-entry-status (entry)
  (let ((action (make-js-action
		 (lambda (&rest r)
		   (setf (entry-status entry) :cleared)))))
    (case (entry-status entry)
      (:uncleared  (with-html (:wo-uncleared :onclick action "🆄")))
      (:cleared (with-html (:wo-cleared "🅒")))
      (:pending (with-html (:wo-pending :onclick action "🅟")))
      (t (with-html  (:wo-unknown-status :onclick action "U"))))))

(defun render-entry (entry &optional account)
  "Renders the entry to the output stream.

Could be part of the render pipeline of an entry widger,
or used raw.


The optional account does some tricky filtering:

1. It will show the entry if at least one of the transactions involve account.
2. It will not show the transactions for account.

What it will show:

- Funds column:   The virtual entry that is NOT a special entry
- Target:         The non-virtual entry that is not equal to account
"

  (when (entry-relevant-for-account entry account)
    (with-html
      (:tr
       (:td  (periods:strftime (entry-actual-date entry)))
       (:td  (entry-payee entry))
       (:td  (entry-budget-name entry account))
       (:td  :class "wo-amount" (:wo-amount (entry-value-for-account entry account)))
       (:td  (entry-expense-name entry account))
       (:td  (render-entry-status entry))))))
