with
     period as (
        select * from v_period
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
         from period left join income
             on paid_date >= period.start_date and paid_date <= period.end_date
         group by end_date
     ),
     combined as (
        select '0 Starting CAR' record, period.end_date, ils, usd from v_car_summary inner join period on record_date = period.start_date
        union all
        select '1 Income', * from income_by_period
        union all
        select '2 Spending', * from spending_by_period
        union all
        select '3 Ending CAR', period.end_date, ils, usd
        from v_car_summary inner join period
            on record_date = period.end_date + (24 * 60 * 60)
    )
    select record,
           date(v_period.start_date / 1000, 'unixepoch') start_date,
           date(v_period.end_date / 1000, 'unixepoch') end_date,
           ils, usd
    from combined
        inner join v_period on combined.end_date = v_period.end_date
    order by end_date desc, record desc;
;


select
    date(start_date / 1000, 'unixepoch') start_date,
    date(end_date / 1000, 'unixepoch') end_date,
    ils, usd
from v_unreported_spending
where start_date <> 0
