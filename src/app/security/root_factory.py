from pyramid.security import Allow
class RootFactory(object):
	"""
	There's only one group - authorized - which is allowed to 
	access to everything because the default permission requires 'access'.
	There are select few views with permission=pyramid.security.NO_PERMISSION_REQUIRED,
	overriding the default permission and allowing anonymous access. There are 
	no in between groups.
	"""
	__acl__ = [(Allow, 'group:authorized', 'access')]
	def __init__(self, request):
		pass
        
