import * as React from 'react'
import {render} from 'react-dom'
import {browserHistory, Route, Router} from 'react-router'
import {useCallback, useEffect, useState} from 'react';
import {login, logout, getLoggedIn} from './services/server-api';
import styled from '@emotion/styled';
import {FinancialsHome} from './financials/finacials.home';
import {AppBar, IconButton, Toolbar} from '@material-ui/core';
import {ExitToApp} from '@material-ui/icons';

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
            <>
                <AppBar position={'relative'}>
                    <Toolbar>
                        <IconButton onClick={onLogout} color={'inherit'}>
                            <ExitToApp />
                        </IconButton>
                    </Toolbar>
                </AppBar>
                <WrappedComponent/>
            </> :
            <LoginForm onLoggedIn={setLoggedIn}/>
    }
}
const App = () => (
    <Router history={browserHistory}>
        <Route component={FinancialsHome} path="/" />
    </Router>
)
const AppWithLogin = withLogin(App);

render(<AppWithLogin />, document.getElementById('app'))