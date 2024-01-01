(uiop:define-package :wo-ledger/logic/entry
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:import-from #:wo-ledger/logic/transaction
		#:xact-part-of-account)
  (:import-from #:wo-ledger/logic/account
		#:account-virtual-p)
  (:export
   #:transactions-for-account
   #:transactions-with-filter
   #:transactions-for-account-or-children
   #:transactions-with-filter
   #:transactions-without-account-or-children
   #:transactions-relevant-for-account))

(in-package :wo-ledger/logic/entry)


(defmethod transactions-for-account ((entry entry) (account (eql nil)))
  (transactions-with-filter entry (constantly t)))

(defmethod transactions-for-account ((entry entry) (account account))
  "Returns a list of transactions in entry that touch account."
  (transactions-with-filter entry
			   #'(lambda (xact) (eq account (xact-account xact)))))

(defmethod transactions-for-account-or-children ((entry entry) (account account))
  (flet ((account-or-parent (xact)
	   (xact-part-of-account xact account)))
    (transactions-with-filter entry #'account-or-parent)))

(defmethod transactions-without-account-or-children ((entry entry) (account account))
  (flet ((account-or-parent (xact)
	   (not (xact-part-of-account xact account))))
    (transactions-with-filter entry #'account-or-parent)))

(defmethod transactions-with-filter ((entry entry) (filter function))
  (loop :with iter = (transactions-iterator entry)
	:for xact = (funcall iter)
	:while xact
	:when (funcall filter xact) :collect xact))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logical= (a b)
  (or (and a b)
      (and (not a) (not b))))

(defun transactions-relevant-for-account (entry account)
  "Returns T if we want to see entry in an account overview."
  (when (transactions-for-account entry account)
    (let ((transactions (transactions-without-account-or-children entry account))
	  (virtual? (account-virtual-p account)))
      (remove-if (lambda (xact) (logical= virtual? (xact-virtualp xact)))
		     transactions))))

