(uiop:define-package :wo-ledger/logic/binder
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series))

(in-package :wo-ledger/logic/binder)


(defun all-accounts-2 (binder)
  (labels ((accounts (account)
	     (cons account
		   (and (account-children account)
			(loop :for sub-account :in (hash-table-values (account-children account))
			      :append (accounts sub-account))))))
    (accounts (binder-root-account binder))))

(defun all-accounts (binder)
  (remove-duplicates
   (collect
       (map-fn 'account #'(lambda (xact) (xact-account xact))
	       (scan-transactions binder)))))





