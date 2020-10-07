import * as React from 'react'
import {render} from 'react-dom'
import {browserHistory, Route, Router} from 'react-router'
import {FinancialsHome} from './financials/finacials.home';
import { initializeIcons } from '@uifabric/icons';
import {withLogin} from './common/with-login.hoc';

initializeIcons();

const App = () => (
    <Router history={browserHistory}>
        <Route component={FinancialsHome} path="/" />
    </Router>
)
const AppWithLogin = withLogin(App);

render(<AppWithLogin />, document.getElementById('app'))