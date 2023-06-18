/*
 In order for the rollup logic to work right, there must be exactly one category ("uncategorized" or "root")
 with a null parent_category_id
 */
CREATE TABLE category(category_id  integer primary key not null
    ,name varchar(50) not null unique
    ,parent_category_id null references category);

CREATE TABLE payment(payment_id  integer primary key not null
    ,paid_date date not null
    ,incurred_begin_date date null --if empty assume the paid_date
    ,incurred_end_date date null --if empty assume the incurred_begin_date
    ,recipient varchar(50) not null
    ,amount decimal(19,4) not null
    ,incurred_amount decimal(19,4) not null
    ,currency char(3) not null default 'ILS'
    ,category_id int not null references category
    ,note text null
    --,cost decimal(19,4) null --only meaningful in the context of "items" table that should hang off this table
);

CREATE TABLE income(income_id integer primary key not null
    , source varchar(50) NOT NULL
    , paid_date date NOT NULL
    , amount decimal(19,4) NOT NULL
    , currency char(3) not null default 'ILS'
    , note text NULL
);

CREATE TABLE cash_account(cash_account_id integer primary key not null
    ,name varchar( 50 )NOT NULL unique
    ,currency char(3) NOT NULL default 'USD'
    ,active boolean NOT NULL default true --when false, hidden from the input form in the UI, but everything else works as normal
);

CREATE TABLE cash_assets_record(cash_assets_record_id integer primary key not null
    ,record_date date NOT NULL
    ,cash_account_id int NOT NULL REFERENCES cash_account
    ,amount decimal(19,4) NOT NULL
    ,unique(record_date, cash_account_id)
);

CREATE TABLE stock_account(stock_account_id integer primary key not null
    ,name varchar(50) NOT NULL unique
    ,tax_category char(10) NOT NULL check ( tax_category in ('deferred', 'exempt', 'taxable') ));

CREATE TABLE stock_transaction (stock_transaction_id integer primary key not null
    /*
        Consider changing this to a date, with timezone. As it stands, it is a timestamp but without timezone,
        and represents the time in New York. Some of the historical data I have (automatic reinvestment of dividends)
        seems to have been exported in Jerusalem time, which needs to be fixed.
    */
    ,transaction_date timestamp NOT NULL
    ,account_id int NOT NULL REFERENCES stock_account
    ,ticker_symbol varchar(20) NOT NULL
    ,unit_price decimal(19,4) NOT NULL
    ,quantity decimal(12,4) NOT NULL
    ,cost_basis decimal(19,4) NULL -- relevant for sales only
    ,commission decimal(19,4) NULL -- all assignable transaction fees go here
);

create table stock_split(
      split_date date NOT NULL
    , ticker_symbol varchar(20) NOT NULL
    , previous_share_qty int NOT NULL
    , new_share_qty int NOT NULL
);

create table fx_transaction(fx_transaction_id integer primary key not null
   /*
   Table assumes that all transactions include a constant local currency (eg USD), which is implied. An alternative is to specify it per row, but then there's nothing that forces a given currency to always be in the same column, and at that point I may as well have bought/sold columns and keep all numbers positive. Easier to read and ue, harder to sum across the table to find the balance of a given currency. Might be a worthwhile refactor, though. 

   The transaction rate is implied by foreign_qty / local_qty. This can also be changed, so that instead of recording amount_paid I record the unit_price (fx_rate), but then I need to make sure that the rate is in the correct direction. With amount_paid I can easily validate that they have opposite signs. 

   The outflow of USD is local_qty + local_commission. Separating commissions from qty is a bit annoying, but might be useful for tracking. On the other hand, in stock_transaction the commission must be separated because the price*qty is also the cost_basis, in this table there might be no point. Revisit this if it proves too clunky to use. Also, there might be foreign_commissions to deal with as well. 
   */
   , transaction_date timestamp NOT NULL --see comment about stock_transaction date
   , account_id int NOT NULL REFERENCES stock_account -- TODO: support fx in cash_accounts as well, possibly by combining the tables or possibly with a separate FK
   , foreign_currency char(3) NOT NULL 
   , foreign_qty decimal(19,4) NOT NULL --eg the amount of foreign currency bought (+) /sold (-)
   , local_qty decimal(19, 4) NOT NULL --amount in base currency (eg USD), not including commission that can be separated out. Sign is opposite foreign_qty
   , local_commission decimal(19, 4) NULL --Separate commissions and fees, also in base currency. 
   , note text NULL
   , check (foreign_qty != 0 and local_qty != 0)
   , check (foreign_qty > 0 != local_qty > 0) --opposite signs
)

create table stock_dividend(dividend_id integer primary key not null --this might be useless but relatively harmless
   , account_id int not null references stock_account
   , ticker_symbol varchar(20) NOT NULL
   , payment_date date not null
   , total_amount decimal(19,4) not null
   , amount_per_share decimal(19, 4) null
   , unique(account_id, ticker_symbol, payment_date)
)

create table stock_account_cash_transaction(transaction_id integer primary key not null
       , transaction_date date NOT NULL
       , account_id int NOT NULL REFERENCES stock_account
       , currency char(3) default 'USD'
       , amount decimal(19,4) NOT NULL
       , note text NULL
)

create table cash_assets_allocation(cash_assets_allocation_id integer primary key not null
    --consider a better name than record_date, since it's not a snapshot but a transaction
    ,record_date date NOT NULL
    ,allocation_code varchar(6) NOT NULL
    ,currency char(3) NOT NULL
    ,amount decimal(19,4) NOT NULL
    ,note text NULL

);

create table financial_constants(
    code varchar(10) not null primary key,
    amount decimal(19,4) not null
);

create table fuel_log(
    fuel_log_id integer primary key not null
    ,timestamp datetime NOT NULL
    ,odometer integer NOT NULL
    ,liters decimal(6,4) NOT NULL
    ,is_full boolean NOT NULL DEFAULT TRUE
    ,note text NULL
    ,payment_id integer NOT NULL REFERENCES payment
);

/*
 Timestamp semantics:

 file.created is the first time a file with this hash was seen.
 filename.created is the first time this particular filename pointed to this particular hash.

 Corollaries:

 * Creating a copy of an existing file to another path doesn't change the file.created, but
 the filename.created of the new copy will be the current time.
 * The current version of a file at a given path is the filename with the newer created
 * When consuming old files, the file.created should always get the oldest mtime seen for the hash,
 and filename.created should also get the oldest mtime for the filename/hash

 Access times are just for logging when the values are changed, for debugging. There's no real semantic meaning to them.
 Consider getting rid of them.
 */

create table file(
    -- md5?
    sha256 text primary key not null,
    sha1 text not null, --todo check that this is 1:1 on the pk
    bytes integer not null,
    accessed timestamp null, --poorly named, this is only updated when the row is updated, should be called "recorded" or something
    created timestamp null,
    mimetype text null,
    storage_account_1 boolean not null default false,
    storage_account_2 boolean not null default false
);

create table filename(
    name text not null,
    created timestamp null, --this can also be thought of as the modified date on the logical "file" (ie a given path)
    sha256 not null references file,
    accessed timestamp null, --poorly named, this is only updated when the row is updated, should be called "recorded" or something
    unique(name, created) --primary key - this is a "file version"
)
