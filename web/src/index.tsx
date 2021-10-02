import * as React from 'react'
import {render} from 'react-dom'
import {initializeIcons} from '@uifabric/icons';
import {withLogin} from './common/with-login.hoc';
import {useBoolean} from '@uifabric/react-hooks';
import {HashRouter as Router} from 'react-router-dom';
import {Header} from './header/header';
import {Navigation} from './header/navigation';

initializeIcons();

const App = ({onLogout}) => {
    const [shouldShowNav, {setTrue: showNav, setFalse: hideNav}] = useBoolean(false)
    return (
        <>
            <Router>
                <Header showNav={showNav} onLogout={onLogout}/>
                <Navigation showNav={showNav} shouldShowNav={shouldShowNav} hideNav={hideNav}/>
            </Router>
        </>
    );
}

const AppWithLogin = withLogin(App);

render(<AppWithLogin />, document.getElementById('app'))
