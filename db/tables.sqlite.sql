/*
 Using the same schema as in pgsql, but why did I separate spending and income into two tables?
 I don't recall the logic. Regardless this db is probably overkill for now but it won't hurt,
 it can be refactored or changed later with some data migrations.
 */
CREATE TABLE register(register_id integer primary key not null
    ,name varchar(50) not null unique);


CREATE TABLE category(category_id  integer primary key not null
    ,name varchar(50) not null unique
    ,register_id int not null references register);

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
    , register_id int NOT NULL REFERENCES register
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
);

CREATE TABLE stock_transaction (stock_transaction_id integer primary key not null
    ,transaction_date date NOT NULL
    ,ticker_symbol varchar(20) NOT NULL
    ,unit_price decimal(19,4) NOT NULL
    ,quantity decimal(12,4) NOT NULL
    ,is_purchase boolean NOT NULL
);