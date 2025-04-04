(cl-defun pim--add-action (title 
			   &key kb sql-view endpoint query row-action
			   bufname
			   section description)
  "Only title, kb, and exactly one of sql-view/endpoint/query is required."
  (setq bufname (or bufname
		    (downcase (replace-regexp-in-string " " "-" title))
		    ))
  (pim--add-to-home `(lambda () (interactive)
		       ;; TODO this could be written better
		       (let ((func))
			 (when ',sql-view
			   (setq func `(lambda () (interactive) (pim-sql-show-view ',',sql-view)))
			   (pim-sql-show-view ',sql-view
					      (pim-grid--create-keymap :row-action ',row-action :refresh-cb func)))
			 (when ',endpoint
			   (setq func `(lambda () (interactive) (pim-sql-show-endpoint ',',endpoint ',',bufname)))
			   (pim-sql-show-endpoint ',endpoint ',bufname (pim-grid--create-keymap :row-action ',row-action :refresh-cb func)))
			 (when ',query
			   (setq func `(lambda () (interactive) (pim-sql-show-query ',',query ',',bufname)))
			   (pim-sql-show-query ',query ',bufname (pim-grid--create-keymap :row-action ',row-action :refresh-cb func)))))
		    kb title section description))

(pim--add-action "Stock Holdings by Account"
		 ;; TODO merge with stock-accounts-cash-balances, they're fundamentally the same except that this has taxCategory
		 ;; and that they have different row actions. Maybe that's enough of a reason for separation?
		 ;; Or merge, add taxCategory to the endpoint, rename it to stock-accounts/summary, and decide which is a row actions and which is a cell action. . 
		 :kb (kbd "s a")
		 :sql-view "v_stock_holdings_account_summary"
		 :row-action (list 'pim-fin--stock-account-holdings-summary "stock_account_id" "name")
		 :section "Stocks"
		 :description "Summary of stock holdings by account - shows the v_stock_holdings_account_summary view")

(pim--add-action "Stock Cash by Account"
		 :kb (kbd "s c")
		 :endpoint "stock-accounts/cash-balances"
		 :row-action (list 'pim-fin--stock-account-cash-flow "id" "accountName")
		 :section "Stocks"
		 :description "Similar to stock holdings by account")

(pim--add-action "Stock Holdings Summary"
		 :kb (kbd "s s")
		 :endpoint "stock-holdings-summary"
		 :row-action (list 'pim-fin--stock-transactions-filtered "tickerSymbol" "taxCategory")
		 :section "Stocks"
		 :description "Stock holdings across accounts")

(defun pim-fin--stock-transactions-filtered (tickerSymbol taxCategory)
  (pim-sql-show-query (format "select * from v_stock_transactions st
                               inner join stock_account sa on st.account_id = sa.stock_account_id
                               where ticker_symbol = '%s' and tax_category = '%s'"
			      tickerSymbol taxCategory)
		      (format "stock_transactions (%s, %s)" tickerSymbol taxCategory)))


(defun pim-fin--stock-account-cash-flow (account-id name)
  ;; if not absolutely trivial use the web server via pim-sql-show-endpoint
  (pim-sql-show-query (format "select * from v_stock_account_cash_flow where account_id = %d
                               order by transaction_date desc, transaction_time desc, record_id desc"
			      account-id)
		      (format "stock-account-cash-flow (%s)" name)))

(defun pim-fin--stock-account-holdings-summary (account-id name)
  (pim-sql-show-query (format "select * from v_stock_holdings_account_ticker_summary
                               where stock_account_id = %d"
			      account-id)
		      (format "stock-account-holdings-summary (%s)" name)))

(defun pim-fin-stock-transactions ()
  (interactive)
  (pim-sql-show-endpoint "stock-transactions"
			 "stock-transactions"))
(pim--add-to-home 'pim-fin-stock-transactions (kbd "s t") "Stock Transactions" "Stocks")

(defun pim-fin-fx-history ()
  (interactive)
  (pim-sql-show-endpoint "fx-history"
			 "fx-history"))
(pim--add-to-home 'pim-fin-fx-history (kbd "f x") "FX History" "Stocks")

(defun pim-fin-income ()
  (interactive)
  (pim-sql-show-endpoint "income"
			 "income"))
(pim--add-to-home 'pim-fin-income (kbd "c i") "Income" "Cash")

(defun pim-fin-spending ()
  (interactive)
  (pim-sql-show-endpoint "payments"
			 "payments"))
(pim--add-to-home 'pim-fin-spending (kbd "c s") "Payments" "Cash")

(defun pim-fin-unreported-spending ()
  (interactive)
  (pim-sql-show-endpoint "unreported-spending"
			 "unreported-spending"))
(pim--add-to-home 'pim-fin-unreported-spending (kbd "c u") "Unreported Spending" "Cash")

(defun pim-fin-car-summary ()
  (interactive)
  (pim-sql-show-endpoint "car/summary"
			 "car-summary"))
(pim--add-to-home 'pim-fin-car-summary (kbd "c a") "Cash Assets History" "Cash")

(defun pim-fin-cash-allocations ()
  (interactive)
  (pim-sql-show-endpoint "cash-allocations"
			 "cash-allocations"
			 (pim-grid--create-keymap
			  :row-action (list 'pim-fin--cash-allocations-history "allocationCode"))))
(pim--add-to-home 'pim-fin-cash-allocations (kbd "c c") "Cash Allocations" "Cash")


(defun pim-fin--cash-allocations-history (allocation-code)
  (pim-sql-show-endpoint (format "cash-allocations/history/%s" allocation-code)
			 (format "allocation-history-%s" allocation-code)))
