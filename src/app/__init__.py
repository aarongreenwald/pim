from pyramid.config import Configurator
from pyramid.authentication import AuthTktAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy
from sqlalchemy import engine_from_config

import api.financials.models
import security.auth


def main(global_config, **settings):
    """ This function returns a Pyramid WSGI application.
    """
    engine = engine_from_config(settings, 'sqlalchemy.')
    authn_policy = AuthTktAuthenticationPolicy(secret = settings['auth_secret']
							, callback = security.auth.groupfinder
							, cookie_name = settings['auth_cookie_name']
							, secure = False #default. I'd like to make this true
							, include_ip = True #not default. What're the implications?
							, timeout = int(settings['auth_timeout'])
							, reissue_time = int(settings['auth_reissue_time'])
							, max_age = None #default...what does this do?
							, http_only = True #not default..no reason to access the cookie from JS, but not all browsers honor this
							, wild_domain = True #default...what does this do?
							, debug = ['auth_debug'] 
							#, hashalg = ['auth_hashalg']
							)
    authz_policy = ACLAuthorizationPolicy()
    config = Configurator(settings=settings, root_factory='app.security.root_factory.RootFactory')
    
    config.include('pyramid_chameleon')
    
    config.set_authentication_policy(authn_policy)
    config.set_authorization_policy(authz_policy)
    config.set_default_permission('access')

    api.financials.models.DBSession.configure(bind=engine)
    api.financials.models.Base.metadata.bind = engine

    #config.add_static_view('static', 'static', cache_max_age=3600)
    config.add_static_view('', 'ui', permission='access', cache_max_age = 0)
    config.add_route('index', '/') #messes up the base href    
    config.add_route('login', '/login')
    config.add_route('logout', '/logout')
    config.add_route('home', '/home')
    
    config.add_route('cash_accounts', 'api/financials/cash-accounts')
    config.add_route('spending_item', 'api/financials/spending-item/{id}')
    config.add_route('spending_items', 'api/financials/spending-items')
    config.scan()

    return config.make_wsgi_app()
