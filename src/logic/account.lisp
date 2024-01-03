(uiop:define-package :wo-ledger/logic/account
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:import-from #:cambl
		#:format-value
		#:*default-display-precision*)
  (:export
   #:account-display-name
   #:account-display-value
   #:account-leafs))

(in-package :wo-ledger/logic/account)

(defun account-parents-list (account)
  (let ((result (list)))
    (loop :while account
	  :do
	     (push account result)
	     (setf account (account-parent account)))
    result))

(defmethod account-name ((account (eql nil)))
  "Extend standard method to work on nil as well"
  nil)

(defun account-display-name (account)
  (let ((acl (account-parents-list account)))
    (cond
      ((not acl) "<<NOT EXISTS>>")
      ((and (> (length acl) 3)) (string= (account-name (nth 1 acl))
		   "Assets")
       (str:join " > " (mapcar #'account-name (subseq acl 3))))
      (t (str:join " > " (mapcar #'account-name (subseq  acl 1)))))))


(defun account-display-value (account)
  (let ((*default-display-precision* 2))
    (when account
      (format-value (ledger::account-value account :total)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun account-leaf-p (account)
  (let ((ht (account-children account)))
    (or (not ht)
	(= 0 (hash-table-count ht)))))

(defun account-leafs (account)
  "Returns a list of accounts that are in the account tree
spaned of account, but are leafs."
  (let ((result (list)))
    (labels ((traverse-children (a)
	       (if (account-leaf-p a)
		   (push a result)
		   (loop :for ca :in (hash-table-values (account-children a))
			 :do (traverse-children ca)))))
      (traverse-children account)
      result)))

#+nil(defun on-budget-accounts (start-account)
  (let ((result (list)))
    (labels ((scan-account (account collect?)
	       (if (and collect? (account-leaf-p account))
		   (push account result)
		   (loop :for ca)
		 ((string= "OnBudget" (account-name account))))
	       )))
    
    )

  )
