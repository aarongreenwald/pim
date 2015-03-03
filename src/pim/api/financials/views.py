from pyramid.response import Response
from pyramid.view import view_config
from pyramid.view import forbidden_view_config
import pyramid

from models import DBSession
import models as m

import datetime
import numbers

def format(obj):	
	if isinstance(obj, datetime.date):
		return str(obj)
	elif isinstance(obj, numbers.Number):
		return str(obj) 
	else:
		return obj

def serialize(result_set):			
	if not isinstance(result_set, list):
		result = {c.name: format(getattr(result_set, c.name)) for c in result_set.__table__.columns}
	else:
		result = []
		for row in result_set:
			serialized = {c.name: format(getattr(row, c.name)) for c in row.__table__.columns}
			result.append(serialized)
	return result

"""
@view_config(route_name='test', renderer='templates/test.pt')
def test(request):
    parameter1 = request.matchdict['parm1']
    data = DBSession.query(FinCashAccount).first()
    if data is None:
        return HTTPNotFound('No FinCashAccounts')    
    
    return dict(data=data, parameter1 = parameter1)

@view_config(route_name='home', renderer='home.pt', permission=pyramid.security.NO_PERMISSION_REQUIRED)
def home(request):
	items=['financials/cash_accounts', 
			'financials/spending_items', 
			'financials/spending_item/1',
			'logout' ]
	return dict(actions=items) 
 """ 
@view_config(route_name='cash_accounts', renderer='json')
def cash_accounts(request):
	data = DBSession.query(m.CashAccount).all()	
	return serialize(data)
	
@view_config(route_name='spending_items', renderer='json')
def spending_items(request):
	data = DBSession.query(m.Spending).all()
	return serialize(data)
		
@view_config(route_name='spending_item', renderer='json')
def spending_item(request):
	id = request.matchdict['id']
	data = DBSession.query(m.Spending).filter(m.Spending.spending_id == id).first()
	if data is None:
		return {} #[]?
	else:
		return serialize(data)	
