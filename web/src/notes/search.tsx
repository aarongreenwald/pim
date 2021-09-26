import {useDebouncedInput} from '../common/debounced-input.hook';
import * as React from 'react';
import {useEffect, useState} from 'react';
import {searchNotes} from '../services/server-api';
import {Icon, Stack, Panel, SearchBox, Toggle, FocusZone} from '@fluentui/react';
import ReactMarkdown from 'react-markdown';
import {Link} from 'react-router-dom'
import styled from '@emotion/styled';

export const Search = ({show, onDismiss}) => {
    const {inputVal, debouncedValue, updateValue} = useDebouncedInput('')
    const [excludeHidden, setExcludeHidden] = useState(true);
    const [searchResults, setSearchResults] = useState(null)
    useEffect(() => {
        if (debouncedValue?.length < 3) {
            setSearchResults(null)
            return;
        }

        searchNotes(debouncedValue, excludeHidden).then(setSearchResults)
    }, [debouncedValue, excludeHidden])

    return (
        <Panel isOpen={show} 
               onDismiss={onDismiss} 
               isHiddenOnDismiss 
               onRenderHeader={() =>
                   (
                       <Stack styles={headerStackStyles}>
                           <SearchBox value={inputVal}
                                      onChange={(_, val) => updateValue(val)}/>
                           <Toggle checked={!excludeHidden}
                                   onChange={(_, val) => setExcludeHidden(!val)}
                                   label={'Include hidden'}/>
                       </Stack>
                   )}>
            <FocusZone>
            {
                searchResults?.names.map(item => <DirectoryItemSearchResult key={item.path} item={item} onDismiss={onDismiss}/>)
            }
            {
                searchResults?.contents.map((result) =>
                    <StyledSearchContentResult key={`${result.path}_${result.lineNumber}`}>
                        <Link to={`/notes/?path=${result.path}`} onClick={onDismiss}>{result.path}</Link>
                        <ReactMarkdown>{result.text}</ReactMarkdown>
                    </StyledSearchContentResult>
                )
            }
            </FocusZone>
        </Panel>
    )
}

const headerStackStyles = {
    root: {
        flexGrow: 1,
        marginLeft: 24 //hack - needs to match the panel content padding
    }
};

const DirectoryItemSearchResult = ({item, onDismiss}) => {
    return (
        <StyledSearchResult>
            <Icon iconName={'TextDocument'}/>
            <Link to={`/notes/?path=${item.path}`} onClick={onDismiss}>{item.fileName}</Link>
        </StyledSearchResult>
    )
}

const StyledSearchResult = styled.div`
  border-top: 1px solid gray;
  padding-top: 12px;
  padding-bottom: 12px;
  overflow-wrap: anywhere;
`;

const StyledSearchContentResult = styled(StyledSearchResult)`
  padding-bottom: 0;
`