(uiop:define-package :wo-ledger/logic/transaction
    (:use #:cl
	  #:alexandria
	  #:ledger
	  #:series)
  (:import-from #:periods
		#:strftime)
  (:export
   #:xact-part-of-account
   #:xact-serializer))

(in-package :wo-ledger/logic/transaction)


(defmethod xact-part-of-account ((xact transaction) (account account))
  "Returns t if ACCOUNT is a parent (or the same as) of the account of XACT."
  (loop :for a = (xact-account xact) :then (account-parent a)
	:while a
	:when (eq a account)
	  :do (return t)
	:finally (return nil)))

(defmethod xact-part-of-account ((xact transaction) (account string))
    (loop :for a = (xact-account xact) :then (account-parent a)
	  :while a
	  :when (string= (account-fullname a) account)
	    :do (return t)
	  :finally (return nil)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xact-serializer (stream)
  "Returns a function that serializes transactions to stream.
This is mostly the same as ledger::print-reporter but adds
missing features, such as notes, status and virtual state of transaction."
  (let (last-entry)
    (lambda (xact)
      ;; first check if we change entry
      (unless (eq last-entry (xact-entry xact))
	;; empty line
	(format stream (if last-entry "~%" "~&"))
	;; now write entry information
	(let ((entry (xact-entry xact)))
	  (format stream "~A" (strftime (entry-date entry)))
	  (format stream " ~A" (ecase (entry-status entry)
				 (:cleared "*")
				 (:uncleared " ")
				 (:pending "!")))
	  (when (entry-code entry) (format stream " (~A)" (entry-code entry)))
	  (format stream " ~A" (entry-payee entry))
	  (when-let (note (entry-note entry))
	    (format stream "~%    ; ~A" note))
	  (format stream "~%")
	  (setf last-entry entry)))

      ;; now the transaction information
      (format stream "    ~45A"
	      (if (xact-virtualp xact)
		  (str:concat "[" (account-fullname (xact-account xact)) "]")
		  (account-fullname (xact-account xact))))
      (let ((amount (xact-amount xact)))
	(format stream " ~12A"
		(if (stringp amount)
		    amount
		    (cambl:format-value amount :width 12 :latter-width 52))))
      (when-let (note (xact-note xact))
	(format stream "    ; ~A" note))
      (format stream "~%"))))
