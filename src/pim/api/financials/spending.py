from pyramid.response import Response
from pyramid.view import view_config
from pyramid.view import forbidden_view_config
from .. import utilities
import pyramid
from models import DBSession
import models as m

@view_config(route_name='spending_items', renderer='json', request_method='GET')
def get_spending_items(request):
	data = DBSession.query(m.Spending).all()
	return utilities.serialize(data)

@view_config(route_name='spending_items', renderer='json', request_method='POST')
def post_spending_item(request):	
	spending = m.Spending()
	print(request.body)
	spending.amount = request.get_json('amount')
	spending.category_id = request.get_json('category_id')
	spending.incurred_begin_date = request.get_json('incurred_begin_date')
	spending.incurred_end_date = request.get_json('incurred_end_date')
	spending.note = request.get_json('note')
	spending.paid_date = request.get_json('paid_date')
	spending.recipient = request.get_json('recipient')
	DBSession.add(spending)	
	DBSession.flush()	
	return spending.spending_id
		
@view_config(route_name='spending_item', renderer='json', request_method='GET')
def get_spending_item(request):
	id = request.matchdict['id']
	data = DBSession.query(m.Spending).filter(m.Spending.spending_id == id).first()
	if data is None:
		raise 'Not implemented - need a 404 here'
	else:
		return utilities.serialize(data)	

@view_config(route_name='spending_item', renderer='json', request_method='PUT')
def put_spending_item(request):
	id = request.matchdict['id']
	raise 'Not implemented'
