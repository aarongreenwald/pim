from pyramid.response import FileResponse
from pyramid.view import view_config
import os

@view_config(route_name='app')
def app(request):
	here = os.path.dirname(__file__)
	spa_html = os.path.join(here, 'ui', 'pim.html')
	response = FileResponse(
        spa_html,
        request=request,
        content_type='text/html'
        )
	return response


