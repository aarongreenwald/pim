drop view if exists v_spending;

create view v_spending as
select spending.spending_id,
       paid_date,
       coalesce(incurred_begin_date, paid_date) incurred_begin_date,
       coalesce(incurred_end_date, incurred_begin_date, paid_date) incurred_end_date,
       amount,
       currency,
       c.name category_name,
       r.name register_name,
       note,
       c.category_id,
       c.register_id
from spending
         inner join category c on c.category_id = spending.category_id
         inner join register r on r.register_id = c.register_id;