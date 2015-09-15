import datetime
import numbers
from pyramid import request


def camelCase(str):
	result = str.title().replace('_', '')		
	return result[0].lower() + result[1:]

def serialize(result_set):			
	if not isinstance(result_set, list):
		result = {camelCase(c.name): getattr(result_set, c.name) for c in result_set.__table__.columns}
	else:
		result = []
		for row in result_set:
			serialized = {camelCase(c.name): getattr(row, c.name) for c in row.__table__.columns}
			result.append(serialized)
	return result

def get_json(self, prop):
	try:
		return self.json_body[prop]
	except Exception, e:
		return None

request.Request.get_json = get_json	