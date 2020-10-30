import {useCallback, useEffect, useState} from 'react';
import {getLoggedIn, login, logout} from '../services/server-api';
import styled from '@emotion/styled';
import {ActionButton} from '@fluentui/react';
import * as React from 'react';

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

function useLoginState() {
    //TODO put a logout function on the context and use it elsewhere
    //TODO any 401 anywhere should flip the loggedIn state to false
    const [loggedIn, setLoggedIn] = useState<boolean>(false)
    useEffect(() => {
        getLoggedIn().then(setLoggedIn)
    }, [])
    const onLogout = useCallback(() => {
        logout().then(success => setLoggedIn(success ? false : loggedIn))
    }, [loggedIn])
    return {loggedIn, setLoggedIn, onLogout};
}

export function withLogin (WrappedComponent) {
    return function Component() {
        const {loggedIn, setLoggedIn, onLogout} = useLoginState();
        return loggedIn ?
            <WrappedComponent onLogout={onLogout}/> :
            <LoginForm onLoggedIn={setLoggedIn}/>
    }
}
