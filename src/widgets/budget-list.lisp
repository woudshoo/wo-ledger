(uiop:define-package :wo-ledger/widgets/budget-list
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
   #:render-budget-list))


(in-package :wo-ledger/widgets/budget-list)

(defun render-budget-account (account indent)
  (with-html
    (:tr
     (:td :style (format nil "padding-left: ~Dpx" (* 20 indent))
      (account-name account))
     (:td :class "wo-amount"
	  (:wo-amount (account-display-value account))))))


(defun render-budget-group (account indent)
  (with-html
    (:tr :class "wo-budget-group"
     (:td :style (format nil "padding-left: ~Dpx" (* 20 indent))
      (account-name account))
     (:td :class "wo-amount"
	  (:wo-amount (account-display-value account))))))

(defun render-budget-list (br-account)
  (labels ((recursive-render (account indent)
	     (do-account-children (a account)
	       (if (account-leaf-p a)
		   (render-budget-account a indent)
		   (progn
		     (render-budget-group a indent)
		     (recursive-render a (+ 1 indent)))))))
    (with-html
      (:h2 "Budget List")
      (:table

       (:tbody
	(recursive-render br-account 0))))))

