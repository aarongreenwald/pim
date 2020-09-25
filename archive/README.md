pim
===

personal information manager - learning project for python and the non-.NET life. Uses pyramid for the server-side and angularjs for the client

Getting Started
===============
From the current directory (`archive`):
* cd ..
* mkdir env
* virtualenv env --no-site-packages
* cd archive
* ../env/bin/python setup.py develop
* ../env/bin/pserve development.ini --reload

If you don't have postgresql set up yet:
* sudo apt-get install postgresql libpq-dev python-dev
* Create a user and a pim database