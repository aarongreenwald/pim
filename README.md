For running web locally you need GITHUB_TOKEN on the path but it's not used, so it doesn't need to be valid. 

[![pim-web](https://github.com/aarongreenwald/pim/actions/workflows/web.yml/badge.svg)](https://github.com/aarongreenwald/pim/actions/workflows/web.yml)

### Overview

* *db* - to be accessed _only_ via the server
* *server*: nodejs webserver, serves all frontends
* *web*: React frontend
* *emacs*: Emacs frontend
* *drought*: Utility for loading drive data into PIM
* *common*: Typescript shared by server and web
* *archive*: Delete this? 

### Currency Support

* The db tables are currency agnostic, _except_ `fx_transaction` which assumes a constant home currency. Still, it doesn't specify what it is. 
* Code (including views in db) hardcode ILS (foreign) and USD (local).

### TODO

* Nightly marketdata
* DIP (Docs-in-PIM) - see `./drought/README.md`
