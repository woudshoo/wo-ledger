(uiop:define-package :wo-ledger/widgets/entry-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session
	  #:wo-ledger/widgets/entry
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
  (or (when-let (sa (selected-account entry-list))
	(or (and (account-parent sa) (account-fullname sa)) "<ALL>"))
      "<UNSPECIFIED>"))

(defmethod render ((el entry-list))
  (with-html
    (:h2 "Entries for " (selected-account-name el))
    (:table
     (:thead
      (render-entry-header))
     (:tbody
      (loop :with iter = (entries-iterator (get-value :binder))
	    :for entry = (funcall iter)
	    :while entry
	    :do
	    (render-entry entry (selected-account el)))))))
