import * as React from 'react'
import {render} from 'react-dom'
import {browserHistory, Route, Router} from 'react-router'
import {Spending} from './financials/spending';

import {useCallback, useState} from 'react';
import {login, logout} from './services/server-api';

function LoginForm({onLoggedIn}: {onLoggedIn: (success: boolean) => void}) {
    const [password, setPassword] = useState('')
    const onSubmit = useCallback(() => login(password).then(onLoggedIn), [password, onLoggedIn])
    const onInputChange = useCallback((event) => setPassword(event.target.value), [])
    return (
        <div>
            <input value={password} onChange={onInputChange} type="password"/>
            <button onClick={onSubmit} >Login</button>
        </div>
    )
}

function withLogin (WrappedComponent) {
    return function Component() {
        //TODO put a logout function on the context and use it elsewhere
        //TODO identify if logged in and initialize correctly
        //TODO any 401 anywhere should flip the loggedIn state to false
        const [loggedIn, setLoggedIn] = useState<boolean>(false)
        const onLogout = useCallback(() => {
            logout().then(success => setLoggedIn(success ? false : loggedIn))
        }, [loggedIn])
        return loggedIn ?
            <div>
                <button onClick={onLogout}>Logout</button>
                <WrappedComponent/>
            </div> :
            <LoginForm onLoggedIn={setLoggedIn}/>
    }
}
const App = () => (
    <Router history={browserHistory}>
        <Route component={Spending} path="/" />
    </Router>
)
const AppWithLogin = withLogin(App);

render(<AppWithLogin />, document.getElementById('app'))