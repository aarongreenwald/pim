/* Returns breakdown of cash allocations as of a given date */

with latest_record_date as (
     -- latest record_date available per allocation
     select allocation_code, max(record_date) latest_date
     from v_cash_assets_allocation_history
     where record_date <= 20240630 -- <- DATE GOES HERE
     group by allocation_code
), latest_record as (
     -- add column to identify the latest record within the latest date
     -- this is technically arbitrary, but the running totals are calculated
     -- using the cash_assets_allocation_id as the ordering field, and we need the last one
   select caa.*,
   	  max(caa.cash_assets_allocation_id) over (partition by caa.allocation_code) max_id
   from v_cash_assets_allocation_history caa
       inner join latest_record_date on caa.record_date = d.latest_date
                                    and caa.allocation_code = d.allocation_code
)
select allocation_code, running_total_usd, running_total_ils
from latest_record
where max_id = cash_assets_allocation_id
