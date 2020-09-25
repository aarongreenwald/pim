from sqlalchemy import *
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import (
    scoped_session,
    sessionmaker,
    )
        
from zope.sqlalchemy import ZopeTransactionExtension

DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension(keep_session=True)))
Base = declarative_base()        


class CashAccount(Base):
    __tablename__ = 'fin_cash_account'
    cash_account_id = Column('cash_account_id', INTEGER(), primary_key=True, nullable=False)
    name = Column('name', VARCHAR(length=50), nullable=False)

class CashAssetsRecord(Base):
    __tablename__ = 'fin_cash_assets_record'
    amount = Column('amount', NUMERIC(precision=19, scale=4), nullable=False)
    cash_account_id = Column('cash_account_id', INTEGER(), ForeignKey('fin_cash_account.cash_account_id'), nullable=False)
    cash_assets_record_id = Column('cash_assets_record_id', INTEGER(), primary_key=True, nullable=False)
    record_date = Column('record_date', DATE(), nullable=False)

class Category(Base):
    __tablename__ = 'fin_category'
    category_id = Column('category_id', INTEGER(), primary_key=True, nullable=False)
    name = Column('name', VARCHAR(length=50), nullable=False)
    register_id = Column('register_id', INTEGER(), ForeignKey('fin_register.register_id'), nullable=False)

class Income(Base):
    __tablename__ = 'fin_income'
    amount = Column('amount', NUMERIC(precision=19, scale=4), nullable=False)
    income_id = Column('income_id', INTEGER(), primary_key=True, nullable=False)
    note = Column('note', TEXT())
    paid_date = Column('paid_date', DATE(), nullable=False)
    register_id = Column('register_id', INTEGER(), ForeignKey('fin_register.register_id'), nullable=False)
    source = Column('source', VARCHAR(length=50), nullable=False)

class Register(Base):
    __tablename__ = 'fin_register'

    name = Column('name', VARCHAR(length=50), nullable=False)
    register_id = Column('register_id', INTEGER(), primary_key=True, nullable=False)

class Spending(Base):
    __tablename__ = 'fin_spending'
    amount = Column('amount', NUMERIC(precision=19, scale=4), nullable=False)
    category_id = Column('category_id', INTEGER(), ForeignKey('fin_category.category_id'), nullable=False)
    incurred_begin_date = Column('incurred_begin_date', DATE(), nullable=False)
    incurred_end_date = Column('incurred_end_date', DATE(), nullable=False)
    note = Column('note', TEXT())
    paid_date = Column('paid_date', DATE(), nullable=False)
    recipient = Column('recipient', VARCHAR(length=50), nullable=False)
    spending_id = Column('spending_id', INTEGER(), primary_key=True, nullable=False)

class StockTransaction(Base):
    __tablename__ = 'fin_stock_transaction'
    transaction_date = Column('transaction_date', DATE(), nullable=False)
    is_purchase = Column('is_purchase', BOOLEAN(), nullable=False)
    quantity = Column('quantity', NUMERIC(precision=12, scale=4))
    stock_transaction_id = Column('stock_transaction_id', INTEGER(), primary_key=True, nullable=False)
    ticker_symbol = Column('ticker_symbol', VARCHAR(length=20), nullable=False)
    unit_price = Column('unit_price', NUMERIC(precision=19, scale=4), nullable=False)
