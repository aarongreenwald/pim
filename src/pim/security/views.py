from pyramid.view import (
    view_config,
    forbidden_view_config,    
    )

from pyramid.response import Response
from pyramid.request import Request

from pyramid.security import (
    remember,
    forget,
    authenticated_userid,
    NO_PERMISSION_REQUIRED
    )

from pyramid.httpexceptions import (
    HTTPFound,
    HTTPNotFound,
    )    

import auth

@view_config(route_name='login', renderer='templates/login.pt', request_method='POST', permission=NO_PERMISSION_REQUIRED)
def post_login(request):
    username = request.params['login']
    password = request.params['password']
    came_from = request.params['came_from']
    if auth.authenticate(username, password, request):
        headers = remember(request, username)
        return HTTPFound(location = came_from,
                         headers = headers)
    else:
        request.invoke_subrequest(Request.blank('/login?came_from=' + came_from))


#this is where forbidden views go. alternatively use a special view, see below.
@forbidden_view_config(renderer='templates/login.pt') 
@view_config(route_name='login', renderer='templates/login.pt', request_method='GET')
def get_login(request):     
    referrer = request.url
    if referrer == request.route_url('login'):
        referrer = '/' # never use the login form itself as came_from
    came_from = request.params.get('came_from', referrer)
    
    return dict(
        message = '',
        url = request.application_url + '/login',
        came_from = came_from,
        login = '',
        password = '',
        )

@view_config(route_name='logout')
def logout(request):
    headers = forget(request)
    return HTTPFound(location = request.route_url('login'),
                     headers = headers)

from pyramid.view import notfound_view_config
@notfound_view_config(request_method='GET')
def notfound_get(request):
    return Response('NOT FOUND (GET)', status='404 Not Found')

@notfound_view_config(request_method='POST')
def notfound_post(request):
    return Response('NOT FOUND (POST)', status='404 Not Found')
