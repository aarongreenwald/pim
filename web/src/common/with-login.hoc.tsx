import {useCallback, useEffect, useState} from 'react';
import {getLoggedIn, login, logout} from '../services/server-api';
import styled from '@emotion/styled';
import {DefaultButton, Spinner, TextField} from '@fluentui/react';
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
      <TextField value={password} onChange={onInputChange} type="password" onSubmit={onSubmit}/>
      <DefaultButton onClick={onSubmit}>Login</DefaultButton>
    </StyledForm>
  )
}

const StyledForm = styled.form`
    display: flex;
    justify-content: center;
    height: 100vh;
    align-items: center;
    flex-direction: column;
    
    > *:not(:last-child) {
      margin-bottom: 8px;
    }
  
    > * {
      height: 32px;
      width: 100%;
      max-width: 400px;
    }
`

function useLoginState() {
  //TODO put a logout function on the context and use it elsewhere
  //TODO any 401 anywhere should flip the loggedIn state to false
  const [loggedIn, setLoggedIn] = useState<boolean>(false)
  const [loading, setLoading] = useState<boolean>(true)
  useEffect(() => {
    setLoading(true)
    getLoggedIn().then(res => {
      setLoading(false);
      setLoggedIn(res);
    })
  }, [])
  const onLogout = useCallback(() => {
    logout().then(success => setLoggedIn(success ? false : loggedIn))
  }, [loggedIn])
  return {loggedIn, setLoggedIn, onLogout, loading};
}

export function withLogin (WrappedComponent) {
  return function Component() {
    const {loggedIn, setLoggedIn, onLogout, loading} = useLoginState();

    if (loading) {
      return <Spinner styles={spinnerStyles}/>
    }

    return loggedIn ?
      <WrappedComponent onLogout={onLogout}/> :
      <LoginForm onLoggedIn={setLoggedIn}/>
  }
}

const spinnerStyles = {root: {height: '100vh'}};
