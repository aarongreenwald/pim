from pyramid.view import (
    view_config,
    forbidden_view_config,    
    )

from pyramid.response import Response

from pyramid.security import (
    remember,
    forget,
    authenticated_userid,
    )

from pyramid.httpexceptions import (
    HTTPFound,
    HTTPNotFound,
    )    

import auth

@view_config(route_name='login', renderer='templates/login.pt')
#this is where forbidden views go. alterantively use a special view, see below.
@forbidden_view_config(renderer='templates/login.pt') 
def login(request):    
    referrer = request.url
    if referrer == request.route_url('login'):
        referrer = '/' # never use the login form itself as came_from
    came_from = request.params.get('came_from', referrer)
    message = 'You need to be logged in!!'
    login = ''
    password = ''
    if 'form.submitted' in request.params:
        username = request.params['login']
        password = request.params['password']
        if auth.authenticate(username, password, request):
            headers = remember(request, username)
            return HTTPFound(location = came_from,
                             headers = headers)
        message = 'Failed login!!!!!!!!!!!'

    return dict(
        message = message,
        url = request.application_url + '/login',
        came_from = came_from,
        login = login,
        password = password,
        )

@view_config(route_name='logout')
def logout(request):
    headers = forget(request)
    return HTTPFound(location = request.route_url('app'),
                     headers = headers)
                     
#@forbidden_view_config()
def forbidden(request):
    return Response('TODO')

from pyramid.view import notfound_view_config
@notfound_view_config(request_method='GET')
def notfound_get(request):
    return Response('NOT FOUND (GET)', status='404 Not Found')

@notfound_view_config(request_method='POST')
def notfound_post(request):
    return Response('NOT FOUND (POST)', status='404 Not Found')
