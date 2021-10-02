import * as React from 'react'
import {render} from 'react-dom'
import {FinancialsHome} from './financials/finacials.home';
import { initializeIcons } from '@uifabric/icons';
import {withLogin} from './common/with-login.hoc';
import {ActionButton, Nav} from '@fluentui/react';
import {useBoolean} from '@uifabric/react-hooks';
import styled from '@emotion/styled';
import {SwipeableDrawer} from '@material-ui/core';
import {Income} from './financials/income';
import {FuelHistory} from './financials/fuel-log';
import { Notes } from './notes/notes.index';
import {
    HashRouter as Router,
    Switch,
    Route,
    useHistory
} from 'react-router-dom';
import {Home} from './home/home.index';

initializeIcons();

const Header = ({showNav, onLogout}) => {
    const history = useHistory()
    return (
        <StyledAppHeader>
                    <span>
                        <ActionButton iconProps={{iconName: 'CollapseMenu'}} onClick={showNav}/>
                        <ActionButton iconProps={{iconName: 'Home'}} onClick={() => history.push('/')}/>
                    </span>
            <ActionButton iconProps={{iconName: 'SignOut'}} text={'Logout'} onClick={onLogout}/>
        </StyledAppHeader>
    )
}

const App = ({onLogout}) => {
    const [shouldShowNav, {setTrue: showNav, setFalse: hideNav}] = useBoolean(false)
    return (
        <>
            <Router>
                <Header showNav={showNav} onLogout={onLogout}/>

                <SwipeableDrawer onClose={hideNav} onOpen={showNav} open={shouldShowNav}>
                    <Nav groups={navGroups}
                         onLinkClick={hideNav}
                         styles={navStyles}/>
                </SwipeableDrawer>
                <Switch>
                    <Route path="/income">
                        <Income />
                    </Route>
                    <Route path="/fuel">
                        <FuelHistory />
                    </Route>
                    <Route path="/spending">
                        <FinancialsHome />
                    </Route>
                    <Route path="/notes" component={Notes}/>
                    <Route path="/" component={Home}/>
                </Switch>
            </Router>
        </>
    );
}
const AppWithLogin = withLogin(App);

render(<AppWithLogin />, document.getElementById('app'))

const navGroups = [
    {
        name: '',
        links: [
            {
                key: 'home',
                name: 'Home',
                url: '#/'
            },
            {
                key: 'notes',
                name: 'Notes',
                url: '#/notes'
            }
        ]
    },
    {
        name: 'Financials',
        links: [
            {
                key: 'spending',
                name: 'Spending',
                url: '#/spending'
            },
            {
                key: 'income',
                name: 'Income',
                url: '#/income'
            },
            {
                key: 'fuel',
                name: 'Fuel Log',
                url: '#/fuel'
            }
        ]
    }
]

//TODO style this according to the theme, or use MUI's AppBar or something.
//or put the header in a left
const StyledAppHeader = styled.div`
    display: flex;
    justify-content: space-between;
    // background-color: black;
    border-bottom: 1px solid gray;     
`

const navStyles = {
    root: {
        minWidth: 300
    },
    groupContent: {
        marginBottom: 0
    }
};
