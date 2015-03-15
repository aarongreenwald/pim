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

@view_config(route_name='diary_entries', renderer='json')
def entries(request):
	data = 5
	return data

@view_config(route_name='diary_entry', renderer='json')
def entry(request):	
	return {'boo': 'hello there'}
