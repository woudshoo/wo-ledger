(uiop:define-package :wo-ledger/logic/entry
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:import-from #:wo-ledger/logic/transaction
		#:xact-part-of-account)
  (:import-from #:wo-ledger/logic/account
		#:account-display-name)
  (:import-from #:cambl
		#:*default-display-precision*
		#:format-value)
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
   #:entry-status-for-display))

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



(defun entry-budget-name (entry account)
  (let ((tr (remove account (transactions-for-account-or-children entry "Assets:Budget") :key #'xact-account)))
    (case (length tr)
      (1 
       (account-display-name (xact-account (first tr))))
      (0 nil)
      (t (str:join " -- " (mapcar #'(lambda (tran) (account-display-name (xact-account tran))) tr))))))


(defun entry-value-for-account (entry account)
  (let ((tr (transactions-for-account-or-children entry account))
	(*default-display-precision* 2))
    (case (length tr)
      (1
	 (format-value (xact-amount (first tr))))
      (0 "<<No Transaction>>")
      (t "<<Multiple>>"))))

(defun entry-expense-name (entry)
  (let ((tr (transactions-for-account-or-children entry "Expenses")))
    (case (length tr)
      (1 
       (account-fullname (xact-account (first tr))))
      (0 nil)
      (t "<<Multiple>>"))))


(defun entry-status-for-display (entry)
  (case (entry-status entry)
    (:uncleared "U")
    (:pending "P")
    (:cleared "C")
    (t "-")))


;;; --> Need to be a method
#+nil(defun transactions-relevant-for-account (entry account)
  "Returns T if we want to see entry in an account overview."
  (when (notevery #'xact-virtualp  (transactions-for-account-or-children entry account))
    (let ((transactions (transactions-without-account-or-children entry account))
	  (virtual? (account-virtual-p account)))
      (remove-if (lambda (xact) (logical= virtual? (xact-virtualp xact)))
		     transactions))))

