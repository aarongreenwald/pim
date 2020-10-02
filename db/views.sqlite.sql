drop view if exists v_payment;

create view v_payment as
select payment.payment_id,
       paid_date,
       counterparty,
       coalesce(incurred_begin_date, paid_date) incurred_begin_date,
       coalesce(incurred_end_date, incurred_begin_date, paid_date) incurred_end_date,
       amount,
       currency,
       c.name category_name,
       note,
       c.category_id
from payment
         inner join category c on c.category_id = payment.category_id;

drop view if exists v_car;
create view v_car as
with by_currency as (
    select record_date,
           case currency when 'ILS' then amount else null end ils,
           case currency when 'USD' then amount else null end usd
    from cash_assets_record
             left join cash_account ca on cash_assets_record.cash_account_id = ca.cash_account_id)
select record_date, sum(ils) ils, sum(usd) usd
from by_currency
group by record_date;

