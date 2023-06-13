with decl as (
     select
     	17 as root_cat
),
with_month as (
      select strftime('%Y%m', paid_date / 1000,  'unixepoch') month, ils, usd from v_payment p
      cross join decl
      inner join v_rollup_categories c on p.category_id = c.category_id and c.root_category_id = decl.root_cat
      -- add where for specific categories to further filter specific categories within the root category,
      -- or completely remove the root cat filter and join specific categories
      
      union all

      select strftime('%Y%m', end_date / 1000,  'unixepoch') month, ils, usd
      from v_unreported_spending
          
            
    )
    select month, round(sum(ils)) ils, round(sum(usd)) usd
    from with_month
    group by month
    order by month desc   
;

-----------------------------
-- Drill into month
------------------------------

with decl as (
     select
	'202211' as mnth,
     	19 as root_cat
),
with_month as (
     select strftime('%Y%m', paid_date / 1000,  'unixepoch') month, c.full_name category, ils, usd
     from v_payment p
     cross join decl
     inner join v_rollup_categories c on p.category_id = c.category_id and c.root_category_id = decl.root_cat
      
      union all

      select strftime('%Y%m', end_date / 1000,  'unixepoch') month, 'Unreported', ils, usd
      from v_unreported_spending
     
            
    )
select month, category, round(sum(ils)) ils, round(sum(usd)) usd
from with_month inner join decl on with_month.month = decl.mnth
group by month, category
order by month desc   

------------------------
-- Get categories
--------------------------
;
select * from v_category

;
-----------------
-- Specific category/month drill-down
----------------------------------------
with decl as (
     select
	'202211' as mnth,
     	3 as cat
),
with_month as (
     select strftime('%Y%m', paid_date / 1000,  'unixepoch') month, p.*
     from v_payment p
     inner join decl on p.category_id = decl.cat
            
    )
select month, * 
from with_month inner join decl on with_month.month = decl.mnth
order by paid_date desc
