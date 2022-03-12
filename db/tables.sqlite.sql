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
    ,currency char(3) not null default 'ILS'
    ,category_id int not null references category
    ,note text null
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
);

create table stock_split(
      split_date date NOT NULL
    , ticker_symbol varchar(20) NOT NULL
    , previous_share_qty int NOT NULL
    , new_share_qty int NOT NULL
);

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