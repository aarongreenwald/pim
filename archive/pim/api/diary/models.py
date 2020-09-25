from sqlalchemy import *
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import (
    scoped_session,
    sessionmaker,
    )
        
from zope.sqlalchemy import ZopeTransactionExtension

DBSession = scoped_session(sessionmaker(extension=ZopeTransactionExtension(keep_session=False)))
Base = declarative_base()        


class Entry(Base):
    __tablename__ = 'dry_entry'

    entry_id = Column('entry_id', INTEGER(), primary_key=True, nullable=False)
    start_datetime = Column('start_datetime', DATE(), nullable=False)
    updated_datetime = Column('updated_datetime', DATE(), nullable=False)
    title = Column('title', VARCHAR(length=128))
    content = Column('content', TEXT())

class EntryTag(Base):
    __tablename__ = 'dry_entry_tag'

    entry_id = Column('entry_id', INTEGER(), ForeignKey('dry_entry.entry_id'), primary_key=True, nullable=False)
    name = Column('name', VARCHAR(length=20), primary_key=True, nullable=False)
    
    


