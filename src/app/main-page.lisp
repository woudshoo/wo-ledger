(uiop:define-package :wo-ledger/app/main-page
    (:use
     :cl
     :reblocks/widget
     :reblocks/html
     :reblocks/session
     :reblocks/actions
     :reblocks/dependencies

     :wo-ledger/logic/account
     
     :wo-ledger/app/session
     :wo-ledger/widgets/entry-list
     :wo-ledger/widgets/account-list)
  (:import-from #:ledger
		#:print-report
		#:entries-iterator
		#:binder-refresh-journals-if-needed)
  (:export
   #:make-main-page))

(in-package :wo-ledger/app/main-page)


(defwidget main-page ()
  ((render-counter :accessor render-counter :initform 0)
   (entry-list :accessor entry-list :initarg :entry-list)
   (account-list :accessor account-list :initarg :account-list))
  (:default-initargs :entry-list (make-instance 'entry-list)
		     :account-list (make-instance 'account-list)))

(defmethod initialize-instance :after ((mp main-page) &key &allow-other-keys)
  (add-selected-account-dependend (account-list mp)
				  #'(lambda (al se)
				      (set-selected-account (entry-list mp) se))))



(defmethod get-dependencies ((p main-page))
  (list*
   (make-dependency #P "wo-ledger.css"
     :system :wo-ledger
     :type :css)
   (call-next-method)))

(defmethod render ((p main-page))
  (with-html
    (:h1 "Ledger")
 
    (:button :type "button" :onclick (make-js-action
				      (lambda (&rest r)
					(update p)))
	     "Update Widgets")
    (:button :type "button" :onclick (make-js-action
				      (lambda (&rest r)
					(binder-refresh-journals-if-needed (session-binder))))
	     "Refresh Ledger")
    (:button :type "button" :onclick (make-js-action
				       (lambda (&rest r)
					 (delete-empty-accounts (session-binder))
					 (update p)))
	     "Delete Empty Accounts")
    (:button :type "button"
	     (format *stream* "<a href=\"api/ledger\" download=\"Budget.leger\">Download Ras</a> "))
    (:a :href "api/ledger"  "Budget.ledger" "Show")
;    (:p "Path Info: " (format *stream* "~A" (get-value :request-uri)))
;    (:p "Routes: " (format *stream* "~A" (slot-value reblocks/routes::*routes*  'routes::template)))
;    (:p "Env: " (format *stream* "~A" reblocks/session::*env*) )
;    (:p)
    (format *stream* " Render Counter: ~D" (incf (render-counter p)))
    (:p)
    (:input :type "date" )
    (let ((binder (session-binder)))
      (format *stream* "Yes we have a binder, journal read-date: ~{~A~^, ~}~%" (mapcar #'ledger::journal-read-date (ledger:binder-journals binder)))
      (format *stream* "Yes we have a binder, journal file-dates: ~{~A~^, ~}~%"
	      (mapcar #'file-write-date
		      (mapcar #'ledger:journal-source
			      (ledger:binder-journals binder))))))
  (with-html
    (:div :style "display:flex"
	  (render (account-list p))
	  (render (entry-list p)))))

(defun make-main-page ()
  (make-instance 'main-page))
