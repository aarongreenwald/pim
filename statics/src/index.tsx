import * as React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import {Spending} from './financials/spending';

render((
  <Router history={browserHistory}>
    <Route component={Spending} path="/" />
  </Router>
), document.getElementById('app'))