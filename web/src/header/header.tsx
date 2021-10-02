import {ActionButton} from '@fluentui/react';
import * as React from 'react';
import styled from '@emotion/styled';
import {useHistory} from 'react-router-dom';

interface HeaderProps {
    showNav: () => void;
    onLogout: () => void;
}
export const Header: React.FC<HeaderProps> = ({showNav, onLogout}) => {
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
//TODO style this according to the theme, or use MUI's AppBar or something.
//or put the header in a left
const StyledAppHeader = styled.div`
    display: flex;
    justify-content: space-between;
    // background-color: black;
    border-bottom: 1px solid gray;     
`