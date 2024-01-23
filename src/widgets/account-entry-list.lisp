(uiop:define-package :wo-ledger/widgets/account-entry-list
    (:documentation "This renders a list of transactions for an account.

So it deals with real transactions.   It will uses the entry widget to render
the rows of the transaction table.

It also manages the editing and creating new transactions.")
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session
	  #:reblocks/actions
	  #:reblocks-ui/form
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
  ((account :accessor account :initarg :account)
   (entry-row-in-edit :accessor entry-row-in-edit :initarg :entry-row-in-edit))
  (:default-initargs :entry-row-in-edit nil))


(defmethod entry-row-in-edit-has-entry-p ((ael account-entry-list) (entry entry))
  (let ((e-r-i-e (entry-row-in-edit ael)))
    (and e-r-i-e  (eq (entry e-r-i-e) entry))))

(defmethod clear-row-in-edit ((ael account-entry-list))
  (let ((e-r-i-e (entry-row-in-edit ael)))
    (when e-r-i-e
      (setf (in-edit e-r-i-e) nil)
      (setf (entry-row-in-edit ael) nil)
      (update e-r-i-e))))

(defmethod set-row-in-edit ((ael account-entry-list) (er entry-row))
  (clear-row-in-edit ael)
  (setf (entry-row-in-edit ael) er)
  (setf (in-edit er) t)
  (update er))

(defun render-account-entry-list-internal (ael)
  (let ((account (account ael)))
    (with-html
      (:h2  (account-display-name account)  " [" (account-display-value account) "]")
      (:account-sub-header (number-of-transactions (session-binder) account)))

    
    (with-html-form (:post (lambda (&key date payee account amount expense submit &allow-other-keys)
			     (let ((entry (entry (entry-row-in-edit ael))))
			       (update-entry entry (account ael) date payee account amount expense)
#+nil			       (setf (xact-amount entry) (cambl:amount amount))
			       (setf (entry-payee entry) payee))
			     (clear-row-in-edit ael)
			     (format t "-S:-~A-~A--~A-----~%" submit date payee)))
      (:al-table
       (render-entry-header)
       (loop :with iter = (entries-iterator (session-binder))
	     :for entry = (funcall iter)
	     :while entry
	     :when (entry-relevant-for-account entry account)
	       :do
		  (let ((er (make-instance 'entry-row :entry entry
						      :account account
						      :in-edit (entry-row-in-edit-has-entry-p ael entry)
						      :on-edit (lambda (er) (set-row-in-edit ael er)))))
		    (render er)))))))

(defmethod render ((ael account-entry-list))
  (render-account-entry-list-internal ael))


(defun render-account-entry-list (account)
  (render (make-instance 'account-entry-list :account account)))

