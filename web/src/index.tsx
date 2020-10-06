import * as React from 'react'
import {render} from 'react-dom'
import {browserHistory, Route, Router} from 'react-router'
import {AddPayment} from './financials/add-payment';

import {useCallback, useEffect, useState} from 'react';
import {login, logout, getLoggedIn} from './services/server-api';
import styled from '@emotion/styled';

function LoginForm({onLoggedIn}: {onLoggedIn: (success: boolean) => void}) {
    const [password, setPassword] = useState('')
    const onSubmit = useCallback(e => {
        e.preventDefault(); //prevent page reload on form submission
        login(password).then(onLoggedIn);
    }, [password, onLoggedIn])
    const onInputChange = useCallback((event) => setPassword(event.target.value), [])
    return (
        <StyledForm>
            <input value={password} onChange={onInputChange} type="password" onSubmit={onSubmit}/>
            <button onClick={onSubmit}>Login</button>
        </StyledForm>
    )
}

const StyledForm = styled.form`
    display: flex;
    justify-content: center;
`

function withLogin (WrappedComponent) {
    return function Component() {
        //TODO put a logout function on the context and use it elsewhere
        //TODO any 401 anywhere should flip the loggedIn state to false
        const [loggedIn, setLoggedIn] = useState<boolean>(false)
        useEffect(() => {
            getLoggedIn().then(setLoggedIn)
        }, [])
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
        <Route component={AddPayment} path="/" />
    </Router>
)
const AppWithLogin = withLogin(App);

render(<AppWithLogin />, document.getElementById('app'))