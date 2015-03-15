from pyramid.config import Configurator
from pyramid.authentication import AuthTktAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy
from sqlalchemy import engine_from_config

import api.financials.models
import security.auth


def main(global_config, **settings):
    
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
    config = Configurator(settings=settings, root_factory='pim.security.root_factory.RootFactory')
    
    config.include('pyramid_chameleon')
    
    config.set_authentication_policy(authn_policy)
    config.set_authorization_policy(authz_policy)
    config.set_default_permission('access')

    api.financials.models.DBSession.configure(bind=engine)
    api.financials.models.Base.metadata.bind = engine

    #config.add_static_view('static', 'static', cache_max_age=3600)
    

    #for the main entry point to the SPA
    config.add_route('app', '/')     
    config.add_route('login', '/login')
    config.add_route('logout', '/logout')    
    
    config.add_route('cash_accounts', 'api/financials/cash-accounts')
    config.add_route('spending_item', 'api/financials/spending-item/{id}')
    config.add_route('spending_items', 'api/financials/spending-items')
    
    config.add_route('diary_entries', 'api/diary/entries')
    config.add_route('diary_entry', 'api/diary/entries/{id}')
    
    #this sets up the ui folder to be served from the root, so things like /common/...js work    
    config.add_static_view('', 'ui', permission='access', cache_max_age = 0) 

    config.scan()

    return config.make_wsgi_app()
