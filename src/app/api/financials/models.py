from sqlalchemy import *
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import (
    scoped_session,
    sessionmaker,
    )
        
from zope.sqlalchemy import ZopeTransactionExtension

DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension()))
Base = declarative_base()        
"""
class FinCashAccount(Base):
	__tablename__ = 'fin_cash_account'
	cash_account_id = Column(Integer, primary_key=True)
	name = Column(Text, unique = True)

	def __init__(self, name):
		self.name = name				
"""
##from sqlautocode. I had to modify the pip installed version of 
#the library per this: https://bitbucket.org/x746e/sqlautocode/commits/4c04152dfffec19f2df1b710b1189f91fcb417fb
class CashAccount(Base):
    __tablename__ = 'fin_cash_account'

    __table_args__ = {}

    #column definitions
    cash_account_id = Column('cash_account_id', INTEGER(), primary_key=True, nullable=False)
    name = Column('name', VARCHAR(length=50), nullable=False)

    #relation definitions


class CashAssetsRecord(Base):
    __tablename__ = 'fin_cash_assets_record'

    __table_args__ = {}

    #column definitions
    amount = Column('amount', NUMERIC(precision=19, scale=4), nullable=False)
    cash_account_id = Column('cash_account_id', INTEGER(), ForeignKey('fin_cash_account.cash_account_id'), nullable=False)
    cash_assets_record_id = Column('cash_assets_record_id', INTEGER(), primary_key=True, nullable=False)
    record_date = Column('record_date', DATE(), nullable=False)

    #relation definitions


class Category(Base):
    __tablename__ = 'fin_category'

    __table_args__ = {}

    #column definitions
    category_id = Column('category_id', INTEGER(), primary_key=True, nullable=False)
    name = Column('name', VARCHAR(length=50), nullable=False)
    register_id = Column('register_id', INTEGER(), ForeignKey('fin_register.register_id'), nullable=False)

    #relation definitions


class Income(Base):
    __tablename__ = 'fin_income'

    __table_args__ = {}

    #column definitions
    amount = Column('amount', NUMERIC(precision=19, scale=4), nullable=False)
    income_id = Column('income_id', INTEGER(), primary_key=True, nullable=False)
    note = Column('note', TEXT())
    paid_date = Column('paid_date', DATE(), nullable=False)
    register_id = Column('register_id', INTEGER(), ForeignKey('fin_register.register_id'), nullable=False)
    source = Column('source', VARCHAR(length=50), nullable=False)

    #relation definitions


class Register(Base):
    __tablename__ = 'fin_register'

    __table_args__ = {}

    #column definitions
    name = Column('name', VARCHAR(length=50), nullable=False)
    register_id = Column('register_id', INTEGER(), primary_key=True, nullable=False)

    #relation definitions


class Spending(Base):
    __tablename__ = 'fin_spending'

    __table_args__ = {}

    #column definitions
    amount = Column('amount', NUMERIC(precision=19, scale=4), nullable=False)
    category_id = Column('category_id', INTEGER(), ForeignKey('fin_category.category_id'), nullable=False)
    incurred_begin_date = Column('incurred_begin_date', DATE(), nullable=False)
    incurred_end_date = Column('incurred_end_date', DATE(), nullable=False)
    note = Column('note', TEXT())
    paid_date = Column('paid_date', DATE(), nullable=False)
    recipient = Column('recipient', VARCHAR(length=50), nullable=False)
    spending_id = Column('spending_id', INTEGER(), primary_key=True, nullable=False)

    #relation definitions


class StockTransaction(Base):
    __tablename__ = 'fin_stock_transaction'

    __table_args__ = {}

    #column definitions
    is_purchase = Column('is_purchase', BOOLEAN(), nullable=False)
    quantity = Column('quantity', NUMERIC(precision=12, scale=4))
    stock_transaction_id = Column('stock_transaction_id', INTEGER(), primary_key=True, nullable=False)
    ticker_symbol = Column('ticker_symbol', VARCHAR(length=20), nullable=False)
    unit_price = Column('unit_price', NUMERIC(precision=19, scale=4), nullable=False)

    #relation definitions
