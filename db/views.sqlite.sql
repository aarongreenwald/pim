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
WITH RECURSIVE all_categories(category_id, name, level, parent) AS (
    select category_id, name, 0 as level, parent_category_id
    from category
    where parent_category_id is null
    UNION ALL
    SELECT category.category_id,
           replace(hex(zeroblob(level + 1)), '00', '--') || category.name,
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
WITH RECURSIVE rollup_category(category_id, group_category_id, root_category_id, level) AS (
    select category_id, category_id, category_id, 0 as level
    from category
         -- where category_id = @root --if this was a TVF
    UNION ALL
    SELECT category.category_id,
           --this rolls everything up to the first level, but not past it. So the root is left as is,
           --and the first level is left as is, and subsequent levels get rolled up
           case when level >= 1 then rollup_category.group_category_id else category.category_id end,
           rollup_category.root_category_id root,
           level + 1
    from category
             inner join rollup_category on parent_category_id = rollup_category.category_id
)
select rc.category_id, rc.group_category_id, rc.root_category_id,
       c.name name, gc.name group_category_name, r.name root_category_name, level
--the joins are just for readability, so the the category names are return. Consider removing them
--and doing the join outside the view if necessary
from rollup_category rc left join category c on rc.category_id = c.category_id
                        left join category gc on rc.group_category_id = gc.category_id
                        left join category r on rc.root_category_id = r.category_id
;

drop view if exists v_cash_assets_allocation;

create view v_cash_assets_allocation as
select allocation_code,
       sum(case currency when 'USD' then amount else null end) usd,
       sum(case currency when 'ILS' then amount else null end) ils
from cash_assets_allocation
group by allocation_code
having coalesce(usd, 0) <> 0 or coalesce(ils, 0) <> 0;


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
    record_date - (24 * 60 * 60) end_date
from v_car_summary
union all
select max(record_date), 4102358400000 from v_car_summary
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
            on record_date = period.end_date + (24 * 60 * 60)
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
