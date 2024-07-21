drop view if exists v_payment;

create view v_payment as
select payment.payment_id,
       paid_date,
       counterparty,
       coalesce(incurred_begin_date, paid_date)                    incurred_begin_date,
       coalesce(incurred_end_date, incurred_begin_date, paid_date) incurred_end_date,
       case currency when 'ILS' then amount else null end          ils,
       case currency when 'USD' then amount else null end          usd,
       c.name                                                      category_name,
       note,
       c.category_id
from payment
         inner join category c on c.category_id = payment.category_id;

drop view if exists v_car_summary;
create view v_car_summary as
with by_currency as (
    select record_date,
           case currency when 'ILS' then amount else null end ils,
           case currency when 'USD' then amount else null end usd
    from cash_assets_record
             left join cash_account ca on cash_assets_record.cash_account_id = ca.cash_account_id)
select record_date, sum(ils) ils, sum(usd) usd
from by_currency
group by record_date;

drop view if exists v_car;
create view v_car as
select ca.name,
       record_date,
       case currency when 'ILS' then amount else null end ils,
       case currency when 'USD' then amount else null end usd
from cash_assets_record
         left join cash_account ca on cash_assets_record.cash_account_id = ca.cash_account_id
;

drop view if exists v_category;
create view v_category as
WITH RECURSIVE all_categories(category_id, hierarchical_name, name, level, parent) AS (
    select category_id, name, name, 0 as level, parent_category_id
    from category
    where parent_category_id is null
    UNION ALL
    SELECT category.category_id,
           replace(hex(zeroblob(level + 1)), '00', '--') || category.name,
           category.name,
           level + 1,
           parent_category_id
    FROM category
             inner join all_categories on parent_category_id = all_categories.category_id
    order by category_id, parent_category_id, level, name
)
select * from all_categories
;

/*
 This view would ideally be a TVF that took a root category as an argument,
 and return all its descendant categories and how they roll up to one level
 beneath the root. The idea is that payments can then be grouped by the group_category_id,
 and what you get is all payments under the root broken out to one level, with another group
 for the catch-all parent.

 Because sqlite doesn't have TVFs, this view outputs that data for ALL roots (ie all categories).
 There's no point using this view without immediately adding `where root_category_id = ?`
 */
drop view if exists v_rollup_categories;
create view v_rollup_categories as
WITH RECURSIVE rollup_category(category_id, group_category_id, root_category_id, level, full_name) AS (
    select category_id, category_id, category_id, 0 as level,  '' as full_name -- root is implied by the where clause on the calling query
    from category
         -- where category_id = @root --if this was a TVF
    UNION ALL
    SELECT category.category_id,
           --this rolls everything up to the first level, but not past it. So the root is left as is,
           --and the first level is left as is, and subsequent levels get rolled up
           case when level >= 1 then rollup_category.group_category_id else category.category_id end,
           rollup_category.root_category_id root,
           level + 1,
	   full_name || ' > ' || category.name full_name
    from category
             inner join rollup_category on parent_category_id = rollup_category.category_id
)
select rc.category_id, rc.group_category_id, rc.root_category_id,
       c.name name, gc.name group_category_name, r.name root_category_name, level, full_name
--the joins are just for readability, so the the category names are return. Consider removing them
--and doing the join outside the view if necessary
from rollup_category rc left join category c on rc.category_id = c.category_id
                        left join category gc on rc.group_category_id = gc.category_id
                        left join category r on rc.root_category_id = r.category_id

;drop view if exists v_cash_assets_allocation;
;create view v_cash_assets_allocation as
select allocation_code,
       sum(case currency when 'USD' then amount else null end) usd,
       sum(case currency when 'ILS' then amount else null end) ils
from cash_assets_allocation
group by allocation_code
having coalesce(usd, 0) <> 0 or coalesce(ils, 0) <> 0;

;drop view if exists v_cash_assets_allocation_history;
;create view v_cash_assets_allocation_history as
select record_date,
       allocation_code,
       case currency when 'USD' then amount else null end usd,
       sum(case currency when 'USD' then amount else null end)
           over (partition by allocation_code order by record_date asc	      
	         rows between unbounded preceding and current row) running_total_usd,
       case currency when 'ILS' then amount else null end ils,
       sum(case currency when 'ILS' then amount else null end)
           over (partition by allocation_code order by record_date asc	      
	         rows between unbounded preceding and current row) running_total_ils,
       note
from cash_assets_allocation;

/*
 v_unallocated_cash_snapshot represents the current status of unallocated money.
 It is essentially CAR - New Payments + New Income - Allocations
 Consider the impact of off-by-one issues on dates. Payments/Income/Allocations are
 booked on the day they ocurred, CAR is as of midnight of the record date.
 */
drop view if exists v_unallocated_cash_snapshot;
create view v_unallocated_cash_snapshot as
with caa_summary as (
    select sum(ils) ils, sum(usd) usd
    from v_cash_assets_allocation
),
current_car as (
    select record_date, ils, usd from v_car_summary order by record_date desc limit 1
),
all_data as (
     select ils * -1 ils, usd * -1 usd
     from v_payment
     where paid_date >= (select record_date from current_car)
     union all
     select ils, usd
     from current_car
     union all
     select case currency when 'ILS' then amount else null end ils,
            case currency when 'USD' then amount else null end usd
     from income
     where paid_date >= (select record_date from current_car)
     union all
     select ils * -1, usd * -1
     from caa_summary
 )
select sum(ils) ils, sum(usd) usd
from all_data
;

drop view if exists v_period;
create view v_period as
select
    coalesce(lead(record_date) over (order by record_date desc), 0) start_date,
    cast(strftime('%Y%m%d',
	          date(substr(record_date, 0, 5) || '-' || substr(record_date, 5, 2) || '-' || substr(record_date, 7, 2),
	               '-1 day')
	) as integer) end_date,
    record_date next_start_date
from v_car_summary
union all
select max(record_date), 20991231 from v_car_summary
;

/*
 Unreported spending is Starting CAR + Income - Spending - Ending CAR
 TODO for some reason trying to filter out the first period throws an error unless
 it's in the HAVING. Also fix the hardcoding for max end_date
 */
drop view if exists v_unreported_spending;
create view v_unreported_spending as
with
    period as (
      select * from v_period
      where end_date < 4102358400000
    ),
    spending_by_period as (
        select end_date, -1 * sum(ils) ils, -1 * sum(usd) usd
        from period left join v_payment p
            on paid_date >= period.start_date and paid_date <= period.end_date
        group by end_date
    ),
    income_by_period as (
        select end_date,
               sum(case currency when 'ILS' then amount else null end) ils,
               sum(case currency when 'USD' then amount else null end) usd
        from period  left join income
            on paid_date >= period.start_date and paid_date <= period.end_date
        group by end_date
    ),
    combined as (
        select period.end_date, ils, usd
        from v_car_summary inner join period
            on record_date = period.start_date
        union all
        select * from income_by_period
        union all
        select * from spending_by_period
        union all
        --ending CAR is the CAR on the day after the period ends (period end date is
        --defined as the day before the next CAR)
        select period.end_date, -1 * ils, -1 * usd
        from v_car_summary inner join period
            on record_date = period.next_start_date
    )
select p.start_date, p.end_date,
       sum(ils) ils, sum(usd) usd
from combined inner join v_period p
    on combined.end_date = p.end_date
group by p.start_date, p.end_date
having p.start_date > 0;

/*
 TODO check that this is right. It would be better to leave the currency unpivoted until later,
 as a future improvement. Also, consider a way to expose the breakdown of allocations at a given date
 */
;drop view if exists v_unallocated_cash_history
;create view v_unallocated_cash_history as
    with car_date_allocations as (
        select car.record_date,
               sum(case caa.currency when 'ILS' then amount else 0 end) ils,
               sum(case caa.currency when 'USD' then amount else 0 end) usd
        from v_car_summary car
                 inner join cash_assets_allocation caa on car.record_date > caa.record_date
        group by car.record_date
    )
     select car.record_date
          , car.ils - alloc.ils ils
          , car.usd - alloc.usd usd
     from v_car_summary car
              inner join car_date_allocations alloc on car.record_date = alloc.record_date

/*
TODO once the issue with timestamps is sorted out, this might need to be fixed.
Currently it's mixing timestamps in different timezones (fx is UTC, stock is exchange time). 
*/
;drop view if exists v_stock_account_cash_flow
;create view v_stock_account_cash_flow as
select
	'cash_flow' record_type,
	transaction_id record_id,
	transaction_date,
	null transaction_time,
	account_id,
	case currency when 'ILS' then amount else null end ils,
	case currency when 'USD' then amount else null end usd,
	note description
from stock_account_cash_transaction
union all
select
	'stock_dividend' record_type,
	dividend_id record_id,	
	payment_date,
        null transaction_time,
	account_id,
	null ils,
	total_amount as usd,
	ticker_symbol || ' - ' || amount_per_share || ' per share'
from stock_dividend
union all
select
	'stock_transaction' record_type,
	stock_transaction_id record_id,
	cast(strftime('%Y%m%d', transaction_date / 1000, 'unixepoch') as int),
        strftime('%H:%M:%f', transaction_date / 1000, 'unixepoch'),
	account_id,
	null ils,
	 --positive quantities are purchases (outflow), negative qty are sales (income), commissions are always positive and are outflow
	(-1 * unit_price * quantity) - coalesce(commission, 0) as usd,
	case when quantity > 0 then 'B ' else 'S ' end || quantity || ' ' || ticker_symbol  || ' @ $' || unit_price as description
from stock_transaction
union all
select
	'fx_transaction' record_type,
	fx_transaction_id record_id,
	cast(strftime('%Y%m%d', transaction_date / 1000, 'unixepoch') as int),
        strftime('%H:%M:%f', transaction_date / 1000, 'unixepoch'),
	account_id,
	-- in both cases, the sign represents flow from account. Commissions are always negative flow on USD
	-- if the currency isn't ils, ignore
	case foreign_currency when 'ILS' then foreign_qty else NULL end ils,
	local_qty - local_commission usd,
	note description
from fx_transaction

;drop view if exists v_stock_account_cash_balances
;create view v_stock_account_cash_balances as
select account_id,
       sum(ils) ils,
       sum(usd) usd
from v_stock_account_cash_flow
group by account_id


;drop view if exists v_fuel_log
;create view v_fuel_log as
    -- TODO handle is_full = false
    with withkm as (
        select *
             , odometer - lag(odometer) over (order by odometer asc) kilometers
        from fuel_log
    ) select
        fuel_log_id
           , timestamp
           , odometer
           , liters
           , kilometers
           , kilometers * 1.0 / liters km_per_liter
           , withkm.note
           , is_full
           , withkm.payment_id
           , payment.amount
           , payment.currency
    from withkm
        left join payment on withkm.payment_id = payment.payment_id
    order by odometer desc

--For now assume all entries are in the same currency
;drop view if exists v_fuel_log_summary
;create view v_fuel_log_summary as
    select sum(kilometers) kilometers,
           sum(liters) liters,
           sum(amount) ils,
           sum(kilometers) * 1.0 / sum(liters) kilometers_per_liter
    from v_fuel_log

;drop view if exists v_stock_split_multipliers
;create view v_stock_split_multipliers as
with recursive split_multiples as (
    with ordered_split_history as (
        -- order and number the splits per ticker
        select
            row_number() over (partition by ticker_symbol order by split_date desc) split_number
             , *
        from stock_split
    )
    select split_number,
           ticker_symbol,
           coalesce((select split_date from ordered_split_history i where o.ticker_symbol = i.ticker_symbol and i.split_number = o.split_number + 1), 0) from_date,
           split_date to_date,
           1.0 * new_share_qty / previous_share_qty multiplier
    from ordered_split_history o where split_number = 1
    union all
    select osh.split_number,
           sm.ticker_symbol,
           coalesce((select split_date from ordered_split_history i where osh.ticker_symbol = i.ticker_symbol and i.split_number = osh.split_number + 1), 0) from_date,
           sm.from_date to_date,
           1.0 * new_share_qty / previous_share_qty * multiplier
    from ordered_split_history osh
             inner join split_multiples sm
                        on osh.split_number = sm.split_number + 1
                            and osh.ticker_symbol = sm.ticker_symbol

) select * from split_multiples;

drop view if exists v_current_market_data;
create view v_current_market_data as
-- Avg price is hardcoded to five years prior to today. Ideally this would be a TVF
-- once migrating to a better db
with rolling as (
select ticker_symbol,
       max(date) date,
       min(date) start_date,
       count(*)  count_dates,
       avg(price) avg_price
from market_data
where date >= cast(strftime('%Y%m%d', date()) as int) - 5e4
group by ticker_symbol)
select rolling.*, price
from rolling inner join market_data md
     on rolling.ticker_symbol = md.ticker_symbol and rolling.date = md.date

-- Primary purpose of this view is to account for splits. It retroactively pretends that a transaction happened at half the price,
-- with twice as many shares, so that holdings can be calculated. The commission doesn't change because it's not per-share, so the total
-- amount spent on commissions does not change retroactively.
-- Untested, in particular wrt to selling stocks that have been split, or splitting stocks that have been sold, and what that does to the
-- overall holdings/cost basis calculation. There might be bugs here. 
drop view if exists v_stock_transactions;
create view v_stock_transactions as
select
    stock_transaction_id
     , transaction_date
     , account_id
     , stock_transaction.ticker_symbol
     , 1.0 * unit_price / coalesce(multiplier, 1) unit_price
     , 1.0 * cost_basis / coalesce(multiplier, 1) cost_basis
     , quantity * coalesce(multiplier, 1) quantity
     , commission
from stock_transaction
         left join v_stock_split_multipliers ss
                   on stock_transaction.ticker_symbol = ss.ticker_symbol
                       and cast(strftime('%Y%m%d', transaction_date / 1000, 'unixepoch') as int) >= ss.from_date
                       and cast(strftime('%Y%m%d', transaction_date / 1000, 'unixepoch') as int) < ss.to_date;


;drop view if exists v_stock_holdings
;create view v_stock_holdings as
select
    sa.stock_account_id,
    name,
    tax_category,
    st.ticker_symbol,
    sum(quantity) quantity,
    -- SELLs have a cost_basis and a unit_price, BUYs have only a unit price
    -- The cost_basis of the outstanding shares (currently held) is the sum of
    -- the buy price for all shares less the buy price for the shares sold. I have to check this. 
    sum(coalesce(cost_basis, unit_price) * st.quantity) / sum(quantity) avg_cost_basis,
    sum(coalesce(cost_basis, unit_price) * st.quantity) cost_basis,
    md.price market_price,
    sum(quantity) * md.price market_value    
from v_stock_transactions st
         inner join stock_account sa on sa.stock_account_id = st.account_id
	 left join v_current_market_data md on st.ticker_symbol = md.ticker_symbol
group by sa.stock_account_id, sa.name, st.ticker_symbol, tax_category

-- stock holdings by ticker
;drop view if exists v_stock_holdings_summary
;create view v_stock_holdings_summary as
select ticker_symbol,
       tax_category,
       sum(quantity) quantity,       
       sum(cost_basis) / sum(quantity) avg_cost_basis,
       sum(cost_basis) cost_basis,
       sum(market_value) market_value, market_price
from v_stock_holdings
group by tax_category, ticker_symbol

-- stock holdings by account/ticker
;drop view if exists v_stock_holdings_account_ticker_summary
;create view v_stock_holdings_account_ticker_summary as
select stock_account_id,
       name,
       ticker_symbol,
       sum(quantity) quantity,
       sum(cost_basis) / sum(quantity) avg_cost_basis,
       sum(cost_basis) cost_basis,
       sum(market_value) market_value, market_price
from v_stock_holdings
group by stock_account_id, name, ticker_symbol

-- total stock holdings by account
;drop view if exists v_stock_holdings_account_summary
;create view v_stock_holdings_account_summary as
select stock_account_id, name, tax_category, sum(cost_basis) cost_basis, sum(market_value) market_value
from v_stock_holdings
group by stock_account_id, name, tax_category

-- Filter out anything not ILS for now so that I can assume the currency. 
;drop view if exists v_fx_history
;create view v_fx_history as
select
  fx_transaction_id,
  transaction_date,
  sa.name account_name,
  foreign_qty ils,
  local_qty usd,
  -1.0 * local_commission usd_commission,
  abs(1.0 * foreign_qty / local_qty) fx_rate,
  abs(1.0 * foreign_qty / (local_qty - local_commission)) effective_rate,
  note
from fx_transaction f left join stock_account sa on f.account_id = sa.stock_account_id
where foreign_currency = 'ILS'

;drop view if exists v_file
;create view v_file as
select filename.*,
       f.sha1, f.bytes, f.mimetype, f.storage_account_1, f.storage_account_2,
       row_number() over (partition by filename.name order by filename.created desc) version
from filename inner join file f on filename.sha256 = f.sha256

;drop view if exists v_files_current
;create view v_files_current as
select * from v_file
where version = 1

;drop view if exists v_duplicate_files
;create view v_duplicate_files as
select * from v_file where sha256 in (select sha256 from filename group by sha256 having count(*) > 1)

;drop view if exists v_orphaned_files
;create view v_orphaned_files as
select * from file where sha256 not in (select sha256 from filename)

;drop view if exists v_orphaned_filenames
;create view v_orphaned_filenames as
select * from filename where sha256 not in (select sha256 from file)

