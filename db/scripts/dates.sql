select *
     , date(record_date / 1000, 'unixepoch')
from cash_assets_allocation