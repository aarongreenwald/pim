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


/*
 v_unallocated_cash_snapshot represents the current status of unallocated money.
 It is essentially CAR - New Payments + New Income - Allocations
 */
drop view if exists v_unallocated_cash_snapshot;
create view v_unallocated_cash_snapshot as
with caa_summary as (
    --this can be its own view
    select record_date,
           case currency when 'ILS' then amount else null end ils,
           case currency when 'USD' then amount else null end usd,
           sum(amount)                                        amount
    from cash_assets_allocation
    group by record_date, currency
),
     current_caa as (
         select record_date, ils, usd from caa_summary order by record_date desc limit 1
     ),
     current_car as
         (select record_date, ils, usd from v_car_summary order by record_date desc limit 1),
     all_data as (
         select paid_date, ils * -1 ils, usd * -1 usd
         from v_payment
         where paid_date >= (select record_date from current_car)
         union all
         select record_date, ils, usd
         from current_car
         union all
         select paid_date,
                case currency when 'ILS' then amount else null end ils,
                case currency when 'USD' then amount else null end usd
         from income
         where paid_date >= (select record_date from current_car)
         union all
         select record_date, ils * -1, usd * -1
         from current_caa
     )
select sum(ils) ils, sum(usd) usd
from all_data
;

