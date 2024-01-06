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

(defwidget entry-row ()
  ((entry :accessor entry :initarg :entry)
   (account :accessor account :initarg :account)))

(defun render-entry-header ()
  (with-html
    (:al-table-header
     (:al-cell "Date")
     (:al-cell "Payee")
     (:al-cell "Account/Fund")
     (:al-cell :class "wo-amount" "Amount")
     (:al-cell "Target")
     (:al-cell :class "wo-status" "Status"))))


(defmethod get-html-tag ((er entry-row))
  :al-table-row)

(defmethod render ((er entry-row))
  (let ((entry (entry er))
	(account (account er)))
    (with-html
      (:al-cell  (periods:strftime (entry-actual-date entry)))
      (:al-cell  (entry-payee entry))
      (:al-cell  (entry-budget-name entry account))
      (:al-cell  :class "wo-amount" (:wo-amount (entry-value-for-account entry account)))
      (:al-cell  (entry-expense-name entry account))
      (:al-cell  :class "wo-status" (render-entry-status er)))))

(defun render-entry-status (er)
  (let ((action (make-js-action
		 (lambda (&rest r)
		   (setf (entry-status (entry er)) :cleared)
		   (update er)))))
    (case (entry-status (entry er))
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
  (render (make-instance 'entry-row :entry entry :account account)))
