(uiop:define-package :wo-ledger/widgets/entry-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session
	  #:reblocks-ui/form
	  #:wo-ledger/app/session

	  #:wo-ledger/widgets/account-entry-list
	  #:wo-ledger/widgets/budget-list
	  
	  #:wo-ledger/logic/app
	  #:wo-ledger/logic/account
	  #:ledger)
  (:export
   #:entry-list
   #:set-selected-account))

(in-package :wo-ledger/widgets/entry-list)


;;; List of entries,
;;;
;;; should be filtered so it can show budget, or real transactions from an account
;;;
;;;

(defwidget entry-list ()
  ((selected-account :accessor selected-account :initarg :selected-account))
  (:default-initargs :selected-account nil))

(defmethod set-selected-account ((el entry-list) (ac account))
  (setf (selected-account el) ac)
  (update el))

(defmethod render ((el entry-list))
  (let ((app (get-value :app)))
    (if (eq (selected-account el) (br-account app))
	(render-budget-list (selected-account el))
	(render-account-entry-list (selected-account el)))))
