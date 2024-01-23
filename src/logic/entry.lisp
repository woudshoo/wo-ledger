(uiop:define-package :wo-ledger/logic/entry
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:reblocks/session  ;; to get global app
	  #:series)
  (:import-from #:wo-ledger/logic/transaction
		#:xact-part-of-account)
  (:import-from #:wo-ledger/logic/account
		#:account-display-name
		#:account-leafs)
  (:import-from #:wo-ledger/logic/app
		#:br-account)
  (:import-from #:local-time
		#:parse-timestring)
  (:import-from #:cambl
		#:*default-display-precision*
		#:format-value)
  (:import-from #:wo-ledger/logic/app
		#:ledger)
  (:export
   #:transactions-for-account
   #:transactions-with-filter
   #:transactions-for-account-or-children
   #:transactions-with-filter
   #:transactions-without-account-or-children
   #:transactions-relevant-for-account
   #:entry-relevant-for-account
   #:entry-budget-name
   #:entry-value-for-account
   #:entry-expense-name
   #:entry-status-for-display
   #:possible-budget-names
   #:update-entry))

(in-package :wo-ledger/logic/entry)


(defmethod transactions-for-account ((entry entry) (account (eql nil)))
  (transactions-with-filter entry (constantly t)))

(defmethod transactions-for-account ((entry entry) (account account))
  "Returns a list of transactions in entry that touch account."
  (transactions-with-filter entry
			   #'(lambda (xact) (eq account (xact-account xact)))))

(defmethod transactions-for-account-or-children ((entry entry) account)
  (flet ((account-or-parent (xact)
	   (xact-part-of-account xact account)))
    (transactions-with-filter entry #'account-or-parent)))

;;; can merge with above

(defmethod transactions-for-account-or-children ((entry entry) (account (eql nil)))
    (transactions-with-filter entry (constantly t)))

(defmethod transactions-without-account-or-children ((entry entry) (account account))
  (flet ((account-or-parent (xact)
	   (not (xact-part-of-account xact account))))
    (transactions-with-filter entry #'account-or-parent)))

(defmethod transactions-without-account-or-children ((entry entry) (account (eql nil)))
  nil)

(defmethod transactions-with-filter ((entry entry) (filter function))
  (loop :with iter = (transactions-iterator entry)
	:for xact = (funcall iter)
	:while xact
	:when (funcall filter xact) :collect xact))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logical= (a b)
  (or (and a b)
      (and (not a) (not b))))


(defun entry-relevant-for-account (entry account)
  (transactions-for-account-or-children entry account))



(defun transaction-for-account (entry account)
  (remove account (transactions-for-account-or-children entry "Assets:Budget") :key #'xact-account))

(defun entry-budget-name (entry account)
  (let ((tr (transaction-for-account entry account)))
    (case (length tr)
      (1 
       (account-display-name tr))
      (0 nil)
      (t (account-display-name tr)))))

(defun entry-expense-name (entry account)
  (let ((tr (remove account (transactions-with-filter entry (complement #'xact-virtualp)) :Key #'xact-account)))
    (case (length tr)
      (1 
       (account-display-name tr))
      (0 nil)
      (t (account-display-name tr)))))

(defun entry-value-for-account (entry account)
  (let ((tr (transactions-for-account-or-children entry account))
	(*default-display-precision* 2))
    (case (length tr)
      (1
	 (format-value (xact-amount (first tr))))
      (0 "<<No Transaction>>")
      (t "<<Multiple>>"))))


(defun entry-status-for-display (entry)
  (case (entry-status entry)
    (:uncleared "U")
    (:pending "P")
    (:cleared "C")
    (t "-")))



(defun possible-budget-names (entry account)
  "Returns a list of budget names that can be choosen for ENTRY given we are displaying ACCOUNT.

The return is list of (fullname display-name  selected) where selected is a generalized boolean"
  (let* ((app (get-value :app))
	 (ra  (br-account app))
	 (default-name (entry-budget-name entry account)))
    (when default-name
      (loop :for a :in (account-leafs ra)
	    :for a-n = (account-display-name a)
	    :collect (list (account-fullname a) a-n (string= a-n default-name))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun update-entry (entry account date-string payee-string account-string amount-string expense-string)
  "Update the entry displayed for account with the ...-string values."
  (setf (entry-actual-date entry) (parse-timestring date-string))
  (setf (entry-payee entry) payee-string)
  (let ((tr (first (transaction-for-account entry account)))
	(app (get-value :app)))
    (format t "typeof tr: ~S~%" (type-of tr))
    (format t "trans: ~S~%" tr)
    (format t "  Entries: ~S~%" (ledger:entry-transactions entry))
    (when tr 
      (setf (xact-account tr) (find-account (ledger app) account-string)))))
