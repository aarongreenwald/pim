import * as React from 'react'
import { render } from 'react-dom'
import { Router, Route, browserHistory } from 'react-router'
import {Spending} from './financials/spending';

//test the compilation of common
import {doSomethingReal} from '@pim/common';
console.log(doSomethingReal)

render((
  <Router history={browserHistory}>
    <Route component={Spending} path="/" />
  </Router>
), document.getElementById('app'))