(uiop:define-package :wo-ledger/app/ledger
    (:use
     :cl
     :reblocks/app
     :reblocks/page
     :reblocks/session
     :wo-ledger/app/main-page)
  (:import-from :ledger
		#:binder))


(in-package :wo-ledger/app/ledger)


(defapp ledger)


(defmethod init-page ((app ledger) (url-path string) expire-at)
  (declare (ignore app url-path expire-at))
  (get-value :binder (binder #P "/Users/wimoudshoorn/Development/Source/Lisp/wo-ledger/test.dat"))
  (make-main-page))
