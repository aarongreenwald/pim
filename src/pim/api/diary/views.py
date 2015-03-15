from pyramid.response import Response
from pyramid.view import view_config
from pyramid.view import forbidden_view_config
import pyramid
from .. import utilities
from models import DBSession
import models as m
import datetime
import transaction


#@view_defaults(renderer='json')
#class Diary(object):	
#	def __init__(self, request):		
#		self.request = request

@view_config(route_name='diary_entries', renderer='json', request_method='GET')
def get_entries(request):
	data = DBSession.query(m.Entry).all()	
	return utilities.serialize(data)

@view_config(route_name='diary_entries',  renderer='json', request_method='POST')
def post_entry(request):
	entry = m.Entry()
	entry.start_datetime = datetime.datetime.now()
	entry.updated_datetime = datetime.datetime.now()
	DBSession.add(entry)
	transaction.commit()
	return entry.entry_id

@view_config(route_name='diary_entry', renderer='json', request_method='GET')
def get_entry(request):	
	entry = DBSession.query(m.Entry).filter(m.Entry.entry_id == request.matchdict['id']).first()
	return utilities.serialize(entry)

@view_config(route_name='diary_entry', renderer='json', request_method='PUT')
def put_entry(request):
	return {}
