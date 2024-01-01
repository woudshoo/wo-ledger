(uiop:define-package :wo-ledger/logic/account
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:export
   #:account-virtual-p))

(in-package :wo-ledger/logic/account)


;;; For now hardcoded, should not be!!!
(defun account-virtual-p (account)
  (loop :for a = account :then (account-parent a)
	:while a
	:when (string= "Funds" (account-name a))
	  :do (return t)
	:finally (return nil)))
