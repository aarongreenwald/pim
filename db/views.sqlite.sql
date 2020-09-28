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
       r.name register_name,
       note,
       c.category_id,
       c.register_id
from payment
         inner join category c on c.category_id = payment.category_id
         inner join register r on r.register_id = c.register_id;