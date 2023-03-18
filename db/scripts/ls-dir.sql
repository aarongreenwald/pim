with parent as (select './' parent) --include trailing slash
--directories
select substr(replace(name, parent.parent, ''), 0, instr(replace(name, parent.parent, ''), '/')) name
            , min(created) created
            , max(accessed) accessed
            , NULL sha256
            , NULL sha1
            , sum(bytes) bytes
            , 'dir' mimetype
            , min(storage_account_1) storage_account_1
            , min(storage_account_2) storage_account_2
            , NULL version
            , NULL name
from v_file, parent
where name like parent.parent || '%/%'
group by substr(replace(name, parent.parent, ''), 0, instr(replace(name, parent.parent, ''), '/'))
union all
-- files
select replace(name, parent.parent, ''),
       created,
       accessed,
       sha256,
       sha1,
       bytes,
       mimetype,
       storage_account_1,
       storage_account_2,
       version,
       name
from v_file, parent
where name like parent.parent || '%' and replace(name, parent.parent, '') not like '%/%'