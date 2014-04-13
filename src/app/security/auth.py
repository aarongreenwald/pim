import hashlib, uuid
#this should be object oriented?

def authenticate(username, password, request):	
	correct_username = request.registry.settings['auth_username']
	correct_password = request.registry.settings['auth_password']	
	
	#salt = str(request.registry.settings['auth_salt']) #shouldn't be necessary to str() it	
	#hashed_username = hashlib.sha512(username + salt).hexdigest()
	#hashed_password = hashlib.sha512(password + salt).hexdigest()
	hashed_username = username
	hashed_password = password
	
	return correct_username == hashed_username and correct_password == hashed_password
		
def groupfinder(userid, request):
	if userid == request.registry.settings['auth_username']:
		return ['group:authorized']

#I don't need this level of complexity with just a single user/group
"""
USERS = {'name':'password', 
		'anotheruser':'anotherpassword'} 
		
GROUPS = {'name':['group:authorized', 'group:another'],
			'anotheruser':['group:another']} 


def groupfinder(userid, request):
    if userid in USERS:
        return GROUPS.get(userid, [])

"""
from pyramid.security import Allow
class RootFactory(object):
	"""
	There's only one group - authorized - which is allowed to 
	access to everything because the default permission requires 'access'.
	There are select few views with permission=pyramid.security.NO_PERMISSION_REQUIRED,
	overriding the default permission and allowing anonymous access. There are 
	no in-between groups.
	"""
	__acl__ = [(Allow, 'group:authorized', 'access')]
	def __init__(self, request):
		pass
        
