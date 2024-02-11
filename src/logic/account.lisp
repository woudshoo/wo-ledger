(uiop:define-package :wo-ledger/logic/account
    (:documentation "Accounts are things money lives.

For wo-ledger we consider a few different accounts:")
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:import-from #:cambl
		#:format-value
		#:*default-display-precision*)
  (:import-from #:str
		#:join)
  (:export
   #:account-display-name
   #:account-display-value
   #:account-leafs
   #:do-account-children
   #:account-leaf-p
   #:delete-empty-accounts
   #:number-of-transactions))

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

(defgeneric account-display-name (acount)
  (:method  ((account account))
    (let ((acl (account-parents-list account)))
      (cond
	((not acl) "<<NOT EXISTS>>")
	((and (> (length acl) 3)) (string= (account-name (nth 1 acl))
					   "Assets")
	 (str:join " > " (mapcar #'account-name (subseq acl 3))))
	(t (str:join " > " (mapcar #'account-name (subseq  acl 1)))))))
  (:method  ((account-list list))
    (join " -- " (mapcar #'account-display-name account-list)))

  (:method  ((account transaction))
    (account-display-name (xact-account account))))


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


(defmacro do-account-children ((var account) &body body)
  `(loop :for ,var :in (hash-table-values (or (account-children ,account) (make-hash-table)))
	 :do
	 ,@body))


(defun number-of-transactions (binder account)
  (let ((result 0))
    (loop :with iter = (transactions-iterator
			binder)
	  :for tr =  (funcall iter)
	  :while tr
	  :when (eq account (xact-account tr))
	    :do (incf result))
    result))

(defun account-remove-from-parent (account)
  (when-let (parent (account-parent account))
    (remhash (account-name account) (account-children parent))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-empty-accounts (binder &optional (root-account (binder-root-account binder)))
  (labels ((traverse (account)
	     (do-account-children (ca account)
	       (traverse ca))
	     (when (and (account-leaf-p account)
			(= 0 (number-of-transactions binder account)))
	       (account-remove-from-parent account))))
    (traverse root-account)))
