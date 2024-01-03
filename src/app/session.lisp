(uiop:define-package :wo-ledger/app/session
    (:use
     :cl
     :reblocks/session
     :wo-ledger/logic/app)
  (:export
   #:session-binder))


(in-package :wo-ledger/app/session)


(defun session-binder ()
  (ledger (get-value :app)))

