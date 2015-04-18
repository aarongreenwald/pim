

CREATE TABLE fin_register(register_id serial primary key not null
						,name varchar(50) NOT NULL UNIQUE);


CREATE TABLE fin_category(category_id  serial primary key not null
	,name varchar(50) NOT NULL UNIQUE
	,register_id int NOT NULL REFERENCES fin_register);



--choice of table name is because it most closely represents the concept differentiating spending from income tables.
--payment/earning, debit/credit, do not capture the difference when it comes to returns and negative spending/income. 
--The pluralization is clumsy but not terrible. spending_item? blech.
--expenditure is better than spending but too clumsy
CREATE TABLE fin_spending(spending_id  serial primary key not null
					,paid_date date NOT NULL
					,incurred_begin_date date NOT NULL
					,incurred_end_date date NOT NULL
					,recipient varchar(50) NOT NULL
					,amount decimal(19,4) NOT NULL
					,category_id int NOT NULL REFERENCES fin_category
					,note text NULL					
					);

CREATE TABLE fin_income(income_id serial primary key not null
				  , source varchar(50) NOT NULL
				  , paid_date date NOT NULL
				  , amount decimal(19,4) NOT NULL
				  , register_id int NOT NULL REFERENCES fin_register
				  , note text NULL
				  );

CREATE TABLE fin_cash_account(cash_account_id serial primary key not null
			,name varchar( 50 )NOT NULL unique
			);

CREATE TABLE fin_cash_assets_record(cash_assets_record_id serial primary key not null
			,record_date date NOT NULL
			,cash_account_id int NOT NULL REFERENCES fin_cash_account
			,amount decimal(19,4) NOT NULL
			);

CREATE TABLE fin_stock_transaction (stock_transaction_id serial primary key not null
			,ticker_symbol varchar(20) NOT NULL
			,unit_price decimal(19,4) NOT NULL
			,quantity decimal(12,4) NOT NULL
			,is_purchase boolean NOT NULL
			);


CREATE TABLE dry_entry(entry_id serial primary key not null
			,start_datetime timestamp NOT NULL
			,updated_datetime timestamp NOT NULL
			,title varchar(128) NULL
			,content text NULL
);

CREATE TABLE dry_entry_tags(entry_id int not null references dry_entry(entry_id)
			,name varchar(20) NOT NULL
			,primary key (entry_id, name)			
);