import {useEffect, useState} from 'react';
import {getRecentFiles} from '../services/server-api';
import * as React from 'react';
import { Link } from 'react-router-dom';
import {Icon} from '@fluentui/react';
import {fileIconName} from './icons';
import styled from '@emotion/styled';

function useRecentNotes() {
    const [recentNotes, setRecentNotes] = useState<string[]>(null)
    useEffect(() => {
        getRecentFiles().then(setRecentNotes);
    }, [])
    return recentNotes;
}

export const RecentNotes: React.FC = () => {
    const recentNotes = useRecentNotes();
    return (
        <>
            {
                recentNotes?.map(path => {
                    return <StyledLine key={path}>
                            <Icon iconName={fileIconName} styles={iconStyles}/>
                            <Link to={`/notes/?path=${encodeURIComponent(path)}`}>{path}</Link>
                        </StyledLine>;
                    }
                )
            }
        </>
    )
}

const StyledLine = styled.div`
  line-height: 1.2em;
  font-size: 1.1em;
  display: flex;
  word-break: break-word;
`

const iconStyles = {root: {marginRight: 4, transform: 'translateY(0.2em)'}};