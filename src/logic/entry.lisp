(uiop:define-package :wo-ledger/logic/entry
    (:documentation "Logic for manipulating and reporting on ledger entries.

A ledger entry consists roughly of the following information:

1. Date(s)
2. Payee
3. Status
4. A list of transactions.

In wo-ledger we only support entries that fit certain constraints.  For this we consider a
few type of transactions:

1. Initial entries to setup the ledger
2. Payment out of budget accounts.
3. Payment into budget account.
4. Transfering/assigning money to budgets
5. Payment out of non-budget account
6. Payment into a non-budget account
7. Transfer between budget accounts
8. Transfer between non-budget accounts
9. Transfer between budget and non-budget account

Here are a few examples

1. Initial entries
------------------

1a Initial non budget account:

  2024/01/01 * Initial Account
           Assets:OffBudget:<account>          EUR  1000.0
           Equity:Opening Balances             EUR -1000.0

1b Inital budget account

   2024/01/01 * Initial Account
           Assets:OnBudget:<acount>           EUR  1000.0
           Equity:Opening Balances            EUR -1000.0
           [Assets:Budget:Special:Unassigned] EUR  1000.0
           [Liabilities:Budget:Source]        EUR -1000.0

2. Payment out of budget

    2024/02/02 Jumbo
            Expenses:<category>                EUR   50.0
            Assets:OnBudget:<account>          EUR  -50.0
            [Assets:Budget:Normal:Groceries]   EUR  -50.0
            [Liabilities:Budget:Source]        EUR   50.0

3. Payment into a budget

    2024/02/03 Salery
            Assets:OnBudget:<account>          EUR  800.0
            Income:<employer>                  EUR -800.0
            [Assets:Budget:Special:Unassigned] EUR  800.0
            [Liabilities:Budget:Source]        EUR -800.0

4. Transferring/assigning money to budgets

     2024/02/01 * Budget
            [Assets:Budget:Normal:<budget>]    EUR  850.0
            [Assets:Budget:Special:Unassigned] EUR -850.0

     2024/02/01 * Budget
            [Assets:Budget:Normal:<budget>]    EUR  850.0
            [Assets:Budget:Normal:<budget-2>]  EUR -850.0

5. Payment out of non-budget account

     2024/02/03 Builder
            Expenses:<category>                EUR  200.0
            Assets:OffBudger:<account>         EUR -200.0

6. Payment into a non-budget account

      2024/03/01 Windfall
            Assets:OffBudget:<account>         EUR  1200.0
            Income:<estate>                    EUR -1200.0

7. Transfer between budget accounts

       2024/03/01 Save
            Assets:OnBudget:<account-1>       EUR  400.0
            Assets:OnBudget:<account-2>       EUR -400.0

8. Transfer between non-budget accounts

       2024/03/01 Save
            Assets:OffBudget:<account-1>       EUR  400.0
            Assets:OffBudget:<account-2>       EUR -400.0

9. Transfer between budget and non-budget account

       2024/03/01 Save
            Assets:OffBudget:<account-1>       EUR  400.0
            Assets:OffBudget:<account-2>       EUR -400.0


OBSERVATIONS
------------

the transactions are either:

- 2 real transactions
- 2 real + 2 virtual transactions
- 2 virtual transactions.

The amounts in the transaction are, +/- the same value.  Each pair (real/virtual) balances to zero.

DISPLAY SUPPORT
---------------

For displaying tables of accounts we have the following:

On Budget Accounts:


- 

UPDATING
--------

When updating a transaction, we 
")
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
		#:format-value
		#:parse-amount
		#:multiply)
  (:import-from #:wo-ledger/logic/app
		#:ledger)
  (:export
;   #:transactions-for-account
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
  "Returns a list of transactions of entry such that the account
of the transaction is not account, or one of the descendents of account.

If account is nil, it will the empty list."
  (flet ((account-or-parent (xact)
	   (not (xact-part-of-account xact account))))
    (transactions-with-filter entry #'account-or-parent)))

(defmethod transactions-without-account-or-children ((entry entry) (account (eql nil)))
  nil)

(defmethod transactions-with-filter ((entry entry) (filter function))
  "Returns all transactions for ENTRY for which FILTER returns true.
- ENTRY is a ledger entry
- FILTER is a function taking one parameter, a ledger transaction.

The result is a list of ledger transactions for which FILTER is not nil."
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

(defun budget-transaction-for-account (entry account)
  (remove account (transactions-for-account-or-children entry "Assets:Budget") :key #'xact-account))

(defun virtual-transactions (entry)
  (transactions-with-filter entry #'xact-virtualp))

(defun real-transactions (entry)
  (transactions-with-filter entry (complement #'xact-virtualp)))

(defun entry-budget-name (entry account)
  (let ((tr (budget-transaction-for-account entry account)))
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
  "Update the entry displayed for account with the ...-string values.

The updating consists of:

- DATE-STRING is set on entry
- PAYEE-STRING is set on entry
- ACCOUNT-STRING, replaces the account on the transaction that is displayed.
- AMOUNT-STRING, impacts all transactions.
- EXPENSE-STRING, replaces the account on the transaction that signifies the other side,
  see ??FUNCTION??.
"
  (setf (entry-actual-date entry) (parse-timestring date-string))
  (setf (entry-payee entry) payee-string)
  (let* ((tr (first (budget-transaction-for-account entry account)))
	 (v-acc (virtual-transactions entry))
	 (other-v (first (remove tr v-acc)))
	 (acc-tr (first (transactions-for-account entry account)))
	 (r-acc (real-transactions entry))
	 (other-r (first (remove acc-tr r-acc)))
	 (app (get-value :app))
	 (camble-amount (parse-amount amount-string))
	 (min-amount (multiply -1 camble-amount)))
    (format t "typeof tr: ~S~%" (type-of tr))
    (format t "trans: ~S~%" tr)
    (format t "  Entries: ~S~%" (ledger:entry-transactions entry))
    (format t "  acc-tr: ~A,  other-r: ~A     tr: ~A,  other-v: ~A~%" acc-tr other-r tr other-v)
    (assert (logical= acc-tr other-r))
    (assert (logical= tr other-v))
    (when tr 
      (setf (xact-account tr) (find-account (ledger app) account-string))
      (setf (xact-amount tr) camble-amount))
    (when other-v
      (setf (xact-amount other-v) min-amount))
    (when acc-tr
      (setf (xact-amount acc-tr) camble-amount))
    (when other-r
      (setf (xact-amount other-r) min-amount))))
