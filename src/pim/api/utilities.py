import datetime
import numbers
from pyramid import request

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

def get_json(self, prop):
	try:
		return self.json_body[prop]
	except Exception, e:
		return None
request.Request.get_json = get_json	