(uiop:define-package :wo-ledger/app/ledger
    (:use
     :cl
     :reblocks/app
     :reblocks/page
     :reblocks/session
     :reblocks/routes
     :wo-ledger/logic/app
     :wo-ledger/app/main-page)
  (:import-from :ledger
		#:binder))


(in-package :wo-ledger/app/ledger)

(defparameter *the-app* nil)

(defapp ledger)

(defroute (ledger /ledger/api/ledger :content-type "text/plain")
#+nil  (ledger-content (get-value :app))
  (list 200
	(list :content-type "text/plain;charset=utf-8")
	(list (ledger-content (get-value :app)))))


(defmethod init-page ((app ledger) (url-path string) expire-at)
  (declare (ignore url-path expire-at))
  (setf (get-value :app) (make-ledger-app))
  (setf *the-app* app)
  (make-main-page))
