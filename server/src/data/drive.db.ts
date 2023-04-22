import { all, getDb } from './db.helpers';

export const lsDir: (dir: string) => Promise<any[]> = async (dir) => {
    // dir should have a trailing slash. 
    const db = await getDb();
    return all(db, `
with parent as (select '${dir}' parent) --include trailing slash
--directories
, data as (
    select substr(replace(name, parent.parent, ''), 0, instr(replace(name, parent.parent, ''), '/')) name
            , datetime(min(created), 'unixepoch') created
            , datetime(min(accessed), 'unixepoch') accessed
            , NULL sha256
            , NULL sha1
            , sum(bytes) bytes
            , 'dir' mimetype
            , min(storage_account_1) storage_account_1
            , min(storage_account_2) storage_account_2
            , NULL version
    --            , NULL name
    from v_file, parent
    where name like parent.parent || '%/%'
    group by substr(replace(name, parent.parent, ''), 0, instr(replace(name, parent.parent, ''), '/'))

    union all
    
    -- files
    select replace(name, parent.parent, ''),
       datetime(created, 'unixepoch') created,
       datetime(accessed, 'unixepoch') accessed,
       sha256,
       sha1,
       bytes,
       mimetype,
       storage_account_1,
       storage_account_2,
       version
    --       name
    from v_file, parent
    where name like parent.parent || '%' and replace(name, parent.parent, '') not like '%/%'
)
select 
  case mimetype when 'dir' then 'D' else 'F' end 
  || case storage_account_1 when 1 then '-' else 'x' end
  || case when version = 1 then '-' 
          when version is NULL then '-' 
          when version < 10 then version 
          else '^' end
  as flags
 , name
 , ${formatHumanReadableBytes("bytes")}  as size
 , created
 , sha256
 , sha1
 , accessed
 , mimetype
from data
`)
}

const formatHumanReadableBytes = (bytesColName) => {
    return `

case when ${bytesColName} < 1e3 then round(${bytesColName}, 1) || '  '
when ${bytesColName} < 1e6 then round(${bytesColName} / 1e3, 1) || ' K'
when ${bytesColName} < 1e9 then round(${bytesColName} / 1e6, 1) || ' M'
when ${bytesColName} < 1e12 then round(${bytesColName} / 1e9, 1) || ' G' 
end

    `
}
