For running web locally you need GITHUB_TOKEN on the path but it's not used, so it doesn't need to be valid. 

[![pim-web](https://github.com/aarongreenwald/pim/actions/workflows/web.yml/badge.svg)](https://github.com/aarongreenwald/pim/actions/workflows/web.yml)

### Currency Support

* The db tables are currency agnostic, _except_ `fx_transaction` which assumes a constant home currency. Still, it doesn't specify what it is. 
* Code (including views in db) hardcode ILS (foreign) and USD (local).

### TODO

* Nightly marketdata
