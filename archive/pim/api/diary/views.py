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
	#todo this searching is terrible but it's a POC
	if not any(param == 'search' for param in request.params) or request.GET['search'] == '':
		data = DBSession.query(m.Entry).order_by(m.Entry.start_datetime.desc()).slice(0, 10).all()
	else:		
		searchExpression = '%' + request.GET['search'] + '%'
		data = DBSession.query(m.Entry) \
			.filter((m.Entry.content.ilike(searchExpression) | m.Entry.title.ilike(searchExpression))) \
			.all()
	return utilities.serialize(data)

@view_config(route_name='diary_entries',  renderer='json', request_method='POST')
def post_entry(request):
	entry = m.Entry()
	entry.start_datetime = datetime.datetime.now()
	entry.updated_datetime = datetime.datetime.now()
	DBSession.add(entry)
	DBSession.flush()
	return entry.entry_id

@view_config(route_name='diary_entry', renderer='json', request_method='GET')
def get_entry(request):	
	entry = DBSession.query(m.Entry).filter(m.Entry.entry_id == request.matchdict['id']).first()
	return utilities.serialize(entry)

@view_config(route_name='diary_entry', renderer='json', request_method='PUT')
def put_entry(request):
	entry = DBSession.query(m.Entry).filter(m.Entry.entry_id == request.matchdict['id']).first()
	entry.updated_datetime = datetime.datetime.now()	
	entry.title = request.json_body['title']
	entry.content = request.json_body['content']	
	return utilities.serialize(entry)

@view_config(route_name='diary_entry', renderer='json', request_method='DELETE')
def delete_entry(request):
	entry = DBSession.query(m.Entry).filter(m.Entry.entry_id == request.matchdict['id']).first()
	DBSession.delete(entry)	
	return {}
