from pyramid.response import FileResponse
from pyramid.view import view_config
import os
@view_config(route_name='index')
def index(request):
	here = os.path.dirname(__file__)
	index_html = os.path.join(here, 'ui', 'index.html')
	response = FileResponse(
        index_html,
        request=request,
        content_type='text/html'
        )
	return response
