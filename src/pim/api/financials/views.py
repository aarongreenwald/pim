from pyramid.response import Response
from pyramid.view import view_config
from pyramid.view import forbidden_view_config
from .. import utilities
import pyramid

from models import DBSession
import models as m


@view_config(route_name='cash_accounts', renderer='json', request_method='GET')
def get_cash_accounts(request):
	data = DBSession.query(m.CashAccount).all()	
	return utilities.serialize(data)
