(uiop:define-package :wo-ledger/logic/transaction
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:export
   #:xact-part-of-account))

(in-package :wo-ledger/logic/transaction)


(defmethod xact-part-of-account ((xact transaction) (account account))
  "Returns t if ACCOUNT is a parent (or the same as) of the account of XACT."
  (loop :for a = (xact-account xact) :then (account-parent a)
	:while a
	:when (eq a account)
	  :do (return t)
	:finally (return nil)))

(defmethod xact-part-of-account ((xact transaction) (account string))
    (loop :for a = (xact-account xact) :then (account-parent a)
	  :while a
	  :when (string= (account-fullname a) account)
	    :do (return t)
	  :finally (return nil)))
