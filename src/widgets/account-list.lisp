(uiop:define-package :wo-ledger/widgets/account-list
    (:use #:cl
	  #:alexandria
	  #:reblocks/html
	  #:reblocks/widget
	  #:reblocks/session
	  #:reblocks/actions
	  #:ledger

	  #:wo-ledger/widgets/account)
  
  (:export
   #:account-list
   #:add-selected-account-dependend))

(in-package :wo-ledger/widgets/account-list)


(defwidget account-list ()
  ((selected-account :accessor selected-account :initarg :selected-account :initform nil)
   (selected-account-dependend :accessor selected-account-dependend :initform (list)))
  (:default-initargs :selected-account nil))

(defun add-selected-account-dependend (al listener)
  (push listener (selected-account-dependend al)))

(defun update-selected-account (al account)
  (unless (eq (selected-account al) account)
    (setf (selected-account al) account)
    (loop :for dep :in (selected-account-dependend al)
	  :do
	  (funcall dep al account)) ;; notify dependends
    ))



(defmethod render ((al account-list))
  (labels ((render-recursive (account)
	     (with-html
	       (:ul
		(:li 
		 (:button :type "button"
			  :onclick (make-js-action
				    (lambda (&rest r)
				      (update-selected-account al account)
				      (update al)))
			  (render-account account (eq account (selected-account al))))
		 (when (account-children account)
		   (loop :for child :in (hash-table-values (account-children account))
			 :do
			    (render-recursive child))))))))

    (let ((binder (get-value :binder)))
      (with-html
	(:h2 "Accounts")
	(render-recursive (binder-root-account binder))))))
