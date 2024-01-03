(uiop:define-package :wo-ledger/widgets/entry-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session

	  #:wo-ledger/app/session
	  #:wo-ledger/widgets/entry

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

(defun selected-account-name (entry-list)
  (account-display-name (selected-account entry-list)))

(defun selected-account-value (entry-list)
  (account-display-value (selected-account entry-list)))

(defmethod render ((el entry-list))
  (with-html
    (:h2 "Entries for " (selected-account-name el)  " [" (selected-account-value el) "]")
    (:table
     (:thead
      (render-entry-header))
     (:tbody
      (loop :with iter = (entries-iterator (session-binder))
	    :for entry = (funcall iter)
	    :while entry
	    :do
	    (render-entry entry (selected-account el)))))))
