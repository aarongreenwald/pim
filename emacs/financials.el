
(defun pim-fin-stock-holdings-account-summary ()
  ;; TODO remove? The only thing this has that the poorly named stock-accounts-cash-balances endpoint does not have is the tax category
  (interactive)
  (pim-sql-show-view "v_stock_holdings_account_summary"
		     (pim-grid--create-keymap :row-action (list 'pim-fin--stock-account-cash-flow "stock_account_id"))))
(pim--add-to-home 'pim-fin-stock-holdings-account-summary (kbd "s a") "Stock Holdings by Account" "Stocks")


(defun pim-fin-stock-accounts-cash-balances ()
  (interactive)
  (pim-sql-show-endpoint "stock-accounts/cash-balances"
			 "stock-account-cash-balances"
			 (pim-grid--create-keymap :row-action (list 'pim-fin--stock-account-cash-flow "id"))))
(pim--add-to-home 'pim-fin-stock-accounts-cash-balances (kbd "s c") "Stock Cash by Account" "Stocks")

(defun pim-fin--stock-account-cash-flow (account-id)
  ;; if not absolutely trivial use the web server via pim-sql-show-endpoint
  (pim-sql-show-query (format "select * from v_stock_account_cash_flow where account_id = %d
                               order by transaction_date desc, transaction_time desc, record_id desc"
			      account-id)
		      "stock-account-cash-flow"))

(defun pim-fin-stock-holdings-summary ()
  (interactive)
  (pim-sql-show-endpoint "stock-holdings-summary"
			 "stock-holdings-summary"))
(pim--add-to-home 'pim-fin-stock-holdings-summary (kbd "s s") "Stock Holdings Summary" "Stocks")

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
(pim--add-to-home 'pim-fin-car-summary (kbd "c a") "Cash Assets" "Cash")

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
