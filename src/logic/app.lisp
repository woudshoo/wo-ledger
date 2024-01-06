(uiop:define-package :wo-ledger/logic/app
    (:use #:cl
	  #:alexandria
	  #:ledger)
  (:import-from #:wo-ledger/logic/transaction
		#:xact-serializer)
  (:export
   #:make-ledger-app
   #:ledger
   #:ta-account
   #:oba-account
   #:obl-account
   #:tl-account
   #:br-account
   #:ledger-content))

(in-package :wo-ledger/logic/app)


(defclass ledger-app ()
  ((ledger :accessor ledger :initarg :ledger)
   (on-budget-assets-account :accessor oba-account :initarg :oba-account)
   (off-budget-assest-account :accessor ta-account :initarg :ta-account)
   (on-budget-liabilities-account :accessor obl-account :initarg :obl-account)
   (off-budget-liabilities-account :accessor tl-account :initarg :tl-account)
   (budget-root-account :accessor br-account :initarg :br-account))
  (:default-initargs :ledger nil))

(defmethod initialize-instance :after ((la ledger-app) &key &allow-other-keys)
  (let ((b (ledger la)))
    (setf (oba-account la) (find-account b "Assets:OnBudget"))
    (setf (ta-account la) (find-account b "Assets:OffBudget"))
    (setf (obl-account la) (find-account b "Liabilities:OnBudget"))
    (setf (tl-account la) (find-account b "Liabilities:OffBudget"))
    (setf (br-account la) (find-account b "Assets:Budget"))))

(defun make-ledger-app ()
  (make-instance 'ledger-app
		 :ledger
		 (binder #P "/Users/wimoudshoorn/Development/Source/Lisp/wo-ledger/test.dat")))





(defun ledger-content (app)
  (with-output-to-string (s)
    (loop
      :with reporter = (xact-serializer s)
      :for tr :in (transactions-list (ledger app))
      :do
      (funcall reporter tr))))
