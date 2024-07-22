(defun add-to-map-row-action (map key func col-name) ;; TODO move out of this file, create something similar for cell-specific actions
  "Add an action that will take effect anywhere on the row. Value in `col-name' is what will be passed to `func'"
  (define-key map key `(lambda ()
			 (interactive)
			 (,func (pim-query-get-column-value-from-selected-row ',col-name)))))

(defun pim-fin-stock-holdings-account-summary ()
  (interactive)
  (let ((map (make-sparse-keymap))) ;; TODO make this a leaner utility, something like (make-map &rest) with minimal common stuff like SPC, etc. 
    (add-to-map-row-action map (kbd "SPC") 'pim-fin--stock-account-cash-flow "stock_account_id")
    (pim-sql-show-view "v_stock_holdings_account_summary" map)))

(defun pim-fin--stock-account-cash-flow (account-id)
  (pim-sql-show-query (concat "select * from v_stock_account_cash_flow where account_id = " ;; if not absolutely trivial use the web server, create pim-sql-show-api
			      (number-to-string account-id))
		      "stock-account-cash-flow"))
