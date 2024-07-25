(defun pim-fin-stock-holdings-account-summary ()
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
  (pim-sql-show-query (concat "select * from v_stock_account_cash_flow where account_id = " ;; if not absolutely trivial use the web server via pim-sql-show-endpoint
			      (number-to-string account-id)

			      " order by transaction_date desc, transaction_time desc, record_id desc")
		      "stock-account-cash-flow"))

