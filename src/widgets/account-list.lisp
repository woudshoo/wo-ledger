(uiop:define-package :wo-ledger/widgets/account-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session
	  #:reblocks/actions
	  #:ledger
	  
	  #:wo-ledger/logic/account
	  #:wo-ledger/logic/app
	  #:wo-ledger/app/session
	  #:wo-ledger/widgets/account)


  (:export
   #:account-list
   #:add-selected-account-dependend))

(in-package :wo-ledger/widgets/account-list)


(defwidget account-list ()
  ((selected-account :accessor selected-account :initarg :selected-account :initform nil)
   (selected-account-dependend :accessor selected-account-dependend :initform (list))
   (hide-zero :accessor hide-zero :initarg :hide-zero))
  (:default-initargs :selected-account nil
		     :hide-zero nil))

(defun add-selected-account-dependend (al listener)
  (push listener (selected-account-dependend al)))

(defun update-selected-account (al account)
  (unless (eq (selected-account al) account)
    (setf (selected-account al) account)
    (loop :for dep :in (selected-account-dependend al)
	  :do
	  (funcall dep al account))))

(defmethod toggle-hide-zero ((al account-list))
  (setf (hide-zero al) (not (hide-zero al)))
  (update al))




(defun render-account-list (al accounts)
  "Renders a list of accounts as:

(:ol
   (:li
    (:button)
    (:wo-amount)
"
  (with-html
    (:ol :class "wo-a-list"
	 (loop :for a :in accounts :do
	   (let ((copy-a a))
	     (with-html
	       (:li :class "wo-a-line"
		    (:button :type "button"
			     :class "wo-a-button"
			     :onclick (make-js-action
				       (lambda (&rest r)
					 (update-selected-account al copy-a)
					 (update al)))
			     (render-account a (eq a (selected-account al))))
		    (:wo-amount (account-display-value a)))))))))


(defmethod render ((al account-list))
  (labels ((render-recursive (account)
	     (if (and (hide-zero al) (= 0 (ledger::account-value account :total)))
		 (with-html
		   (when (account-children account)
		     (loop :for child :in (hash-table-values (account-children account))
			   :do
			      (render-recursive child))))
		 (with-html
		   (:ul
		    (:li 
		     (:button :type "button"
			      :onclick (make-js-action
					(lambda (&rest r)
					  (update-selected-account al account)
					  (update al)))
			      (render-account account (eq account (selected-account al))))
		     (ledger::account-value account :total)
		     (when (account-children account)
		       (loop :for child :in (hash-table-values (account-children account))
			     :do
				(render-recursive child)))))))))


    (ledger::find-all-transactions (list :accounts-report t))
    (with-html
      (:h2 "Accounts")

	;;; test:


      (:h3 "On Budget")
      (let* ((app (get-value :app))
	     (ta (oba-account app)))
	(render-account-list al (account-leafs ta)))
      (:hr)
      (let* ((app (get-value :app))
	     (ta (obl-account app)))
	(render-account-list al (account-leafs ta)))

      (:h3 "Tracking")

      (let* ((app (get-value :app))
	     (ta (ta-account app)))
	(render-account-list al (account-leafs ta)))

      (:hr)
      (let* ((app (get-value :app))
	     (ta (tl-account app)))
	(render-account-list al (account-leafs ta)))

#+nil      (let ((binder (session-binder)))
	(:h3 "Old Tree")
	(:input :type "checkbox"
		:id "hide-zero"
		:checked (hide-zero al)
		:onclick (make-js-action
			  (lambda (&rest r)
			    (toggle-hide-zero al))))
	(:label :for "hide-zero" "Hide zeros")

	(render-recursive (binder-root-account binder))))))
