import unittest
import transaction

from pyramid import testing

from api.financials.models import DBSession


class TestMyView(unittest.TestCase):
    def setUp(self):
        self.config = testing.setUp()
        from sqlalchemy import create_engine
        engine = create_engine('sqlite://')
        from .models import (
            Base,
			FinCashAccount
            )
        DBSession.configure(bind=engine)
        Base.metadata.create_all(engine)
        with transaction.manager:
            model = FinCashAccount(name='one')
            DBSession.add(model)

    def tearDown(self):
        DBSession.remove()
        testing.tearDown()

    def test_it(self):
        from .views import test
        request = testing.DummyRequest()
        info = test(request)
#        self.assertEqual(info['one'].name, 'one')
#        self.assertEqual(info['project'], 'tutorial')
