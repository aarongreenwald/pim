from pyramid.renderers import JSON
import decimal
import datetime

def number_adapter(obj, request):	
	return float(obj)
	
def datetime_adapter(obj, request):	
	return datetime.datetime.isoformat(obj)	

def date_adapter(obj, request):		
	return datetime_adapter(datetime.datetime(obj.year, obj.month, obj.day), request)	
    
json_renderer = JSON()
json_renderer.add_adapter(decimal.Decimal, number_adapter)
json_renderer.add_adapter(datetime.datetime, datetime_adapter)
json_renderer.add_adapter(datetime.date, date_adapter)