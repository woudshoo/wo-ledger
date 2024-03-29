
(uiop:define-package :wo-ledger/widgets/entry
    (:use #:cl
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/actions
;	  #:reblocks-ui/form
	  #:wo-ledger/logic/entry
	  #:ledger)
  (:documentation "Code responsible for rendering an entry in a table as row.")
  (:export
   #:render-entry-header
   #:entry-row
   #:entry
   #:in-edit))

(in-package :wo-ledger/widgets/entry)

(defwidget entry-row ()
  ((entry :accessor entry :initarg :entry)
   (account :accessor account :initarg :account)
   (in-edit :accessor in-edit :initarg :in-edit)
   (on-edit :accessor on-edit :initarg :on-edit))
  (:default-initargs :in-edit nil
		     :on-edit (constantly nil)))


(defun render-entry-header ()
  "Renders the heaer for a table containing entries."
  (with-html
    (:al-table-header
     (:al-cell "Date")
     (:al-cell "Payee")
     (:al-cell "Account/Fund")
     (:al-cell :class :wo-amount "Amount")
     (:al-cell "Target")
     (:al-cell :class :wo-status "Status"))))


(defmethod get-html-tag ((er entry-row))
  (list
   :al-table-row))



(defmethod cancel-edit-action ((er entry-row))
  (make-js-action (lambda (&rest r)
		    (setf (in-edit er) nil)
		    (update er))))

(defun render-row-default (er)
  "Render the row in its default state, that is in non edit mode."
  (let ((entry (entry er))
	(account (account er)))
    (with-html
      (:al-cell  :onclick (make-js-action (lambda (&rest r)
					    (funcall (on-edit er) er)))
                (periods:strftime (entry-actual-date entry) :format "%Y-%m-%d"))
      (:al-cell  (entry-payee entry))
      (:al-cell  (entry-budget-name entry account))
      (:al-cell  :class "wo-amount" (:wo-amount (entry-value-for-account entry account)))
      (:al-cell  (entry-expense-name entry account))
      (:al-cell  :class "wo-status" (render-entry-status er)))))

(defun render-row-editable (er)
  "Render the row in edit mode."
  (let ((entry (entry er))
	(account (account er)))
    (with-html
      (:al-cell (:input :type "date" :name "date" :value (periods:strftime (entry-actual-date entry) :format "%Y-%m-%d")))
      (:al-cell (:input :type "text" :name "payee" :value (entry-payee entry)))
      (:al-cell (alexandria:if-let (b-names (possible-budget-names entry account))
		  (with-html
		    (:select :name "account"
		      (loop :for (fn an select) :in b-names
			    :do
			       (with-html
				 (:option :value fn :selected select an)))))
		  (with-html
		    "<-->")))
      (:al-cell (:input :class "wo-amount" :type "text" :name "amount" :value (entry-value-for-account entry account)))
      (:al-cell (:input :type "text" :name "expense" :value (entry-expense-name entry account)))
      (:al-cell (:input :type "submit" :name "submit" :value "Ok")
		(:input :type "submit" :name "submit" :value "Ok-Next" "Yea!")
		(:input :type "submit" :name "submit" :value "Cancel")))))

(defmethod render ((er entry-row))
  "Render the row.
The edit state determines how it is rendered."
  (if (in-edit er)
      (render-row-editable er)
      (render-row-default er)))

(defun render-entry-status (er)
  "Renders the status (cleared/pending etc) as a toggleable entry."
  (let ((set-cleared (make-js-action
		 (lambda (&rest r)
		   (setf (entry-status (entry er)) :cleared)
		   (update er)))))
    (case (entry-status (entry er))
      (:uncleared  (with-html (:wo-uncleared      :onclick set-cleared "🆄")))
      (:cleared    (with-html (:wo-cleared                             "🅒")))
      (:pending    (with-html (:wo-pending        :onclick set-cleared "🅟")))
      (t           (with-html (:wo-unknown-status :onclick set-cleared "U"))))))

