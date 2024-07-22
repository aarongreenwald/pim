-------- Dividend Reinvestment -------------
 --amount_per_share is often not available and could cause data inconsistency, skip it
insert into stock_dividend (account_id, ticker_symbol, payment_date, total_amount)
values (3, 'VEA', 20231221, 100),
       (3, 'VEA', 20230921, 100),
       (3, 'VEA', 20230623, 100),
       (3, 'VEA', 20230323, 100)

-- qty * price doesn't always exactly match the dividend amount due to rounding issues, which results
-- in a few cents that the system would expect to be in stock_cash_account_balances but will go missing.
-- This is fine. 
insert into stock_transaction (account_id, ticker_symbol, transaction_date, unit_price, quantity)
values (3, 'VEA', 20231221, 47.16, 1.234),
       (3, 'VEA', 20230921, 45.54, 1.234),
       (3, 'VEA', 20230623, 45.91, 1.234),
       (3, 'VEA', 20230323, 43.87, 1.234)

