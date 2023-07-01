/**
This shows the total amount (shares and dollars) bought/sold over a period of time.
The sum of the shares column across all time is also the current holdings, but the sum of 
the dollar amounts across time is NOT the total amount invested over that period, because (a) purchases 
with dividends are included, (b) cost basis of sales isn't considered, so if selling and buying a different
symbol with the proceeds, the first symbol has outflow in the amount of the sale and the second inflow in the (roughly) same amount, regardless of how much of that money was principal and how much is growth. 
***
Bottom line: Be very careful summing the total column, and cost_basis column is intentionally left out, this should not be used as a basis for studying investment over time or growth. It's primarily for seeing net change in share quanities in a period and how much asset allocation was changed during that period.
*/
with by_year as (
select
    strftime('%Y', transaction_date / 1000,  'unixepoch') year,
    tax_category,
    ticker_symbol,
    sum(quantity) quantity,
    sum(quantity * unit_price) usd
from v_stock_transactions st inner join stock_account sa on st.account_id = sa.stock_account_id
group by strftime('%Y', transaction_date / 1000,  'unixepoch'), tax_category, ticker_symbol
)
select year,
       ticker_symbol,
       sum(quantity) quantity,
       sum(usd) total,
       sum(usd) / sum(quantity) avg_unit_price
from by_year
group by year, ticker_symbol
